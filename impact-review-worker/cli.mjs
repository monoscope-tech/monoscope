#!/usr/bin/env node
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { randomUUID } from 'node:crypto';
import * as fs from 'node:fs';
import writeFileAtomic from 'write-file-atomic';
import { z } from 'zod';
import { addAbortSignal } from 'node:stream';
import { createInterface } from 'node:readline';

const controller = new AbortController();
for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, () => controller.abort());
const signal = AbortSignal.any([controller.signal, AbortSignal.timeout(420000)]);
let agentDir, lines;
let pending = Promise.resolve();
let executeGuest;
try {
  const guestImage = process.env.IMPACT_REVIEW_GUEST_IMAGE;
  let bytes = 0, inputLimit = 2 * 1024 * 1024;
  const input = addAbortSignal(signal, process.stdin);
  input.on('data', (chunk) => { bytes += chunk.length; if (bytes > inputLimit) controller.abort(); });
  lines = createInterface({ input, crlfDelay: Infinity });
  const messages = lines[Symbol.asyncIterator]();
  const envelope = JSON.parse((await messages.next()).value);
  if (!guestImage) throw new Error('A pinned guest image is required');
  const isFix = envelope.tag === 'FixTask';
  const isAdmission = envelope.tag === 'AdmitTask';
  const isFixReview = envelope.tag === 'ReviewFixTask';
  if (isFixReview) inputLimit = 8 * 1024 * 1024;
  if (envelope.tag !== undefined && !isFix && !isAdmission && !isFixReview) throw new Error('Unknown repository task');
  const task = isAdmission || isFixReview ? envelope.contents.fix : envelope.contents;
  const request = isFix || isAdmission || isFixReview ? { ...task.request, issueId: task.issueId, ...(isAdmission ? { artifact: envelope.contents.artifact } : isFixReview ? { artifacts: envelope.contents.artifacts, imageBuildId: task.imageBuildId } : {}) } : envelope;
  const outboxRoot = fs.realpathSync(process.env.IMPACT_REVIEW_USAGE_DIR);
  const syncDirectory = (path) => {
    const fd = fs.openSync(path, 'r');
    try { fs.fsyncSync(fd); } finally { fs.closeSync(fd); }
  };
  const outbox = fs.realpathSync(process.env.IMPACT_REVIEW_OUTBOX_DIR);
  const exchange = (tag, contents, active = signal) => {
    const reply = pending.then(async () => {
      process.stdout.write(JSON.stringify({ tag, contents }) + '\n');
      let response = JSON.parse((await messages.next()).value);
      while (response.tag === 'Guest') {
        let output;
        try {
          if (!executeGuest) throw new Error('Guest workspace is unavailable');
          output = { Right: await executeGuest(response.contents, active) };
        } catch { output = { Left: 'Guest workspace command failed or exceeded its output limit' }; }
        process.stdout.write(JSON.stringify({ tag: 'GuestResult', contents: output }) + '\n');
        response = JSON.parse((await messages.next()).value);
      }
      if (typeof response.Left !== 'string' && !Object.hasOwn(response, 'Right')) throw new Error('Invalid query response');
      if (['Call', 'Usage', 'Allocation', 'Check', 'Phase'].includes(tag)) {
        if (Object.hasOwn(response, 'Left')) throw new Error('Receipt was rejected');
      }
      return response;
    });
    pending = reply.catch(() => controller.abort());
    return reply;
  };
  const checkoutReply = await exchange('Checkout', []);
  if (Object.hasOwn(checkoutReply, 'Left')) throw new Error('Host checkout was rejected');
  const preparedCheckout = z.object({ directory: z.string().min(1), repositorySkills: z.string().min(1), rootInstructions: z.string(), projectId: z.uuid(), repository: z.string(), base: z.string(), head: z.string(), cacheStatus: z.enum(['hit', 'miss', 'verified']), durationMs: z.number().int().nonnegative(), policy: z.object({ instructions: z.string(), tools: z.array(z.object({ name: z.string(), description: z.string(), parameters: z.record(z.string(), z.unknown()), sequential: z.boolean() }).strict()) }).strict(), evidence: z.discriminatedUnion('tag', [z.object({ tag: z.literal('SourceOnly') }).strict(), z.object({ tag: z.literal('VerifiedEvidence'), contents: z.object({ result: z.record(z.string(), z.unknown()), files: z.record(z.string(), z.string()) }).strict() }).strict()]) }).strict().parse(checkoutReply.Right);
  if (isFixReview !== (preparedCheckout.evidence.tag === 'VerifiedEvidence')) throw new Error('Task requires matching host-verified evidence');
  const checkoutHead = isFixReview ? envelope.contents.artifacts.result.head : isFix || isAdmission ? request.base : request.head;
  if (request.gitToken || preparedCheckout.projectId !== request.projectId || preparedCheckout.repository !== request.repository || preparedCheckout.base !== request.base || preparedCheckout.head !== checkoutHead) throw new Error('A matching credential-free host checkout is required');
  const artifactDir = join(outbox, 'checks');
  fs.mkdirSync(artifactDir, { recursive: true, mode: 0o700 });
  syncDirectory(outbox);
  const persistReceipt = async (tag, receipt) => {
    writeFileAtomic.sync(join(outbox, `${receipt.id}.json`), JSON.stringify(tag === 'Usage' ? receipt : { tag, contents: receipt }) + '\n', { mode: 0o600 });
    syncDirectory(outbox);
    return exchange(tag, receipt);
  };
  const enqueue = (tag, receipt) => { void persistReceipt(tag, receipt).catch(() => controller.abort()); };
  const attemptId = z.uuid().parse(request.attemptId);
  const expectedImageBuildId = isFix || isAdmission || isFixReview ? z.uuid().parse(task.imageBuildId) : undefined;
  agentDir = await mkdtemp(join(tmpdir(), 'monoscope-review-agent-'));
  if (process.env.IMPACT_REVIEW_DEPENDENCY_ARCHIVE && !process.env.IMPACT_REVIEW_PREPARE_SCRIPT) throw new Error('Dependency archives require a trusted preparation script');
  const cacheDir = fs.realpathSync(process.env.IMPACT_REVIEW_CACHE_DIR);
  const [{ ModelRuntime }, { review, modelUsage }, { fixIssue }, { admitRegression }] = await Promise.all([
    import('@earendil-works/pi-coding-agent'), import('./review.mjs'), import('./fix.mjs'), import('./admit.mjs'),
  ]);
  const modelRuntime = await ModelRuntime.create({ authPath: join(agentDir, 'auth.json'), modelsPath: process.env.IMPACT_REVIEW_MODELS ?? null, refreshOnCreate: false });
  const result = await (isAdmission ? admitRegression : isFix ? fixIssue : review)({ ...request, head: checkoutHead }, {
    modelRuntime, guestImage, expectedImageBuildId, cacheDir, preparedCheckout, usageDir: outboxRoot, artifactDir, attemptId, skillPaths: process.argv.slice(2), optionsTool: (invocation) => exchange('Tool', invocation), validateDraft: (draft) => exchange('Validate', draft), signal,
    admitTest: (artifact) => exchange('Admit', artifact),
    proposeCandidate: (cause, active) => exchange('Propose', cause, active),
    bindGuest: (execute) => { executeGuest = execute; },
    assertCandidate: (identifier, active) => exchange('AssertCandidate', identifier, active),
    workspaceCache: process.env.IMPACT_REVIEW_PREPARE_SCRIPT ? {
      cacheDir: process.env.IMPACT_REVIEW_CACHE_DIR, script: fs.readFileSync(process.env.IMPACT_REVIEW_PREPARE_SCRIPT, 'utf8'),
      dependencyArchive: process.env.IMPACT_REVIEW_DEPENDENCY_ARCHIVE && resolve(process.env.IMPACT_REVIEW_DEPENDENCY_ARCHIVE),
    } : undefined,
    recordStep: (step) => persistReceipt('Phase', { id: randomUUID(), attemptId, step }),
    validateFix: (result) => exchange('Validate', result),
    validateAdmission: (result) => exchange('Validate', result),
    onEvent(event) {
      if (event.type === 'model_started') return persistReceipt('Call', event.receipt);
      if (event.type === 'model_response') {
        const message = event.message;
        enqueue('Usage', modelUsage(message, modelRuntime.getModel(message.provider, message.model)?.cost ?? null, attemptId, event.callId));
      }
      if (event.type === 'regression_execution') enqueue('Check', event.receipt);
      if (event.type === 'agent_allocation') enqueue('Allocation', event.receipt);
      if (event.type === 'checkout_prepared' || event.type === 'workspace_cache') process.stderr.write(JSON.stringify(event) + '\n');
      if (event.type === 'tool_execution_start' || event.type === 'tool_execution_end') {
        process.stderr.write(JSON.stringify({ event: event.type, tool: event.toolName, failed: event.isError ?? false }) + '\n');
      }
    },
  });
  await pending;
  signal.throwIfAborted();
  process.stdout.write(JSON.stringify({ tag: 'Result', contents: result }) + '\n');
} catch {
  await pending;
  process.stderr.write(signal.aborted ? 'Impact review cancelled or timed out\n' : 'Impact review did not produce a validated result\n');
  process.exitCode = 1;
} finally {
  lines?.close();
  process.stdin.destroy();
  if (agentDir) await rm(agentDir, { recursive: true, force: true });
}
