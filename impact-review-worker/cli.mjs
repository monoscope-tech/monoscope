#!/usr/bin/env node
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import * as fs from 'node:fs';
import writeFileAtomic from 'write-file-atomic';
import { z } from 'zod';
import { addAbortSignal } from 'node:stream';
import { jsonLines } from './jsonl.mjs';

const controller = new AbortController();
for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, () => controller.abort());
const signal = AbortSignal.any([controller.signal, AbortSignal.timeout(420000)]);
let agentDir;
let pending = Promise.resolve();
try {
  const messages = jsonLines(addAbortSignal(signal, process.stdin), 16 * 1024 * 1024);
  const envelope = JSON.parse((await messages.next()).value);
  const request = envelope.tag === undefined ? envelope : envelope.tag === 'FixTask' ? envelope.contents.request : envelope.contents.fix.request;
  const outbox = fs.realpathSync(process.env.IMPACT_REVIEW_OUTBOX_DIR);
  const syncDirectory = (path) => {
    const fd = fs.openSync(path, 'r');
    try { fs.fsyncSync(fd); } finally { fs.closeSync(fd); }
  };
  const exchange = (tag, contents) => {
    const reply = pending.then(async () => {
      process.stdout.write(JSON.stringify({ tag, contents }) + '\n');
      const response = JSON.parse((await messages.next()).value);
      if (typeof response.Left !== 'string' && !Object.hasOwn(response, 'Right')) throw new Error('Invalid query response');
      if (['Call', 'Usage'].includes(tag) && Object.hasOwn(response, 'Left')) throw new Error('Receipt was rejected');
      return response;
    });
    pending = reply.catch(() => controller.abort());
    return reply;
  };
  const checkoutReply = await exchange('Checkout', []);
  if (Object.hasOwn(checkoutReply, 'Left')) throw new Error('Host checkout was rejected');
  const preparedCheckout = z.object({ guest: z.string(), envelope: z.record(z.string(), z.unknown()).nullable(), directory: z.string().min(1), repositorySkills: z.string().min(1), rootInstructions: z.string(), projectId: z.uuid(), repository: z.string(), base: z.string(), head: z.string(), cacheStatus: z.enum(['hit', 'miss', 'verified']), durationMs: z.number().int().nonnegative(), policy: z.object({ instructions: z.string(), tools: z.array(z.object({ name: z.string(), description: z.string(), parameters: z.record(z.string(), z.unknown()), sequential: z.boolean() }).strict()), builtins: z.array(z.string()) }).strict() }).strict().parse(checkoutReply.Right);
  // Receipts are durable before transmission so a lost host can replay them.
  const persistReceipt = async (tag, receipt) => {
    writeFileAtomic.sync(join(outbox, `${receipt.id}.json`), JSON.stringify(tag === 'Usage' ? receipt : { tag, contents: receipt }) + '\n', { mode: 0o600 });
    syncDirectory(outbox);
    return exchange(tag, receipt);
  };
  const attemptId = z.uuid().parse(request.attemptId);
  agentDir = await mkdtemp(join(tmpdir(), 'monoscope-review-agent-'));
  const [{ ModelRuntime }, { review, modelUsage }] = await Promise.all([import('@earendil-works/pi-coding-agent'), import('./review.mjs')]);
  const modelRuntime = await ModelRuntime.create({ authPath: join(agentDir, 'auth.json'), modelsPath: process.env.IMPACT_REVIEW_MODELS ?? null, refreshOnCreate: false });
  const result = await review({ ...request, head: preparedCheckout.head }, {
    modelRuntime, preparedCheckout, attemptId, signal,
    optionsTool: (invocation) => exchange('Tool', invocation),
    guestOperation: (operation) => exchange('Guest', operation),
    validateDraft: (draft) => exchange('Validate', draft),
    onEvent(event) {
      if (event.type === 'model_started') return persistReceipt('Call', event.receipt);
      if (event.type === 'model_response') {
        const message = event.message;
        void persistReceipt('Usage', modelUsage(message, modelRuntime.getModel(message.provider, message.model)?.cost ?? null, attemptId, event.callId)).catch(() => controller.abort());
      }
      if (event.type === 'checkout_prepared') process.stderr.write(JSON.stringify(event) + '\n');
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
  process.stdin.destroy();
  if (agentDir) await rm(agentDir, { recursive: true, force: true });
}
