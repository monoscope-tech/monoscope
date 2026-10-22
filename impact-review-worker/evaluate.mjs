import { execFileSync } from 'node:child_process';
import { createHash, randomUUID } from 'node:crypto';
import { mkdir, mkdtemp, readFile, writeFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { ModelRuntime } from '@earendil-works/pi-coding-agent';
import { z } from 'zod';
import parseDiff from 'parse-diff';
import { review, reviewDraft, modelUsage } from './review.mjs';

const caseSchema = z.object({
  checkout: z.object({ guest: z.null(), envelope: z.null(), directory: z.string(), repositorySkills: z.string().min(1), rootInstructions: z.string(), projectId: z.uuid(), repository: z.string(), base: z.string(), head: z.string(), cacheStatus: z.enum(['hit', 'miss', 'verified']), durationMs: z.number().int().nonnegative(), policy: z.object({ instructions: z.string(), tools: z.array(z.object({ name: z.string(), description: z.string(), parameters: z.record(z.string(), z.unknown()), sequential: z.boolean() }).strict()), builtins: z.array(z.string()) }).strict() }).strict(),
  projectId: z.uuid(), repository: z.string(), base: z.string().regex(/^[a-f0-9]{40}$/), head: z.string().regex(/^[a-f0-9]{40}$/),
  provider: z.string(), model: z.string(), evidenceKind: z.enum(['synthetic', 'snapshot']),
  evidence: z.array(z.object({ key: z.string().min(1), kind: z.enum(['telemetry', 'monitor', 'dashboard']) }).passthrough()).refine((items) => new Set(items.map((item) => item.key)).size === items.length, 'Evidence keys must be unique'), rubric: z.array(z.string()),
});
const controller = new AbortController();
for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, () => controller.abort());
const signal = AbortSignal.any([controller.signal, AbortSignal.timeout(900000)]);
let agentDir, artifact, output;
try {
  const [casePath, resultPath] = process.argv.slice(2);
  if (!casePath || !resultPath) throw new Error('Usage: node evaluate.mjs CASE.json RESULT.json');
  if (!process.env.IMPACT_REVIEW_GUEST_IMAGE) throw new Error('A pinned guest image is required');
  const input = caseSchema.parse(JSON.parse(await readFile(casePath, 'utf8')));
  const repository = resolve(dirname(casePath), input.repository);
  const git = (...args) => execFileSync('git', ['-C', repository, '-c', 'core.quotePath=false', ...args], { encoding: 'utf8', timeout: 60000, maxBuffer: 16 * 1024 * 1024 });
  const diff = git('diff', '--no-ext-diff', '--no-textconv', '--no-color', `${input.base}...${input.head}`, '--');
  const changed = new Set(parseDiff(diff).flatMap((file) => file.chunks.flatMap((chunk) => chunk.changes
    .filter((change) => change.type !== 'normal')
    .map((change) => JSON.stringify([file.to === '/dev/null' ? file.from : file.to, change.type === 'del' ? 'base' : 'head', change.ln])))));
  const evidence = new Map(input.evidence.map((item) => [item.key, item]));
  const validateDraft = (draft) => {
    for (const finding of draft.findings) {
      if (!changed.has(JSON.stringify([finding.location.path, finding.location.side, finding.location.line]))) return { Left: 'Finding does not reference a changed line' };
      if (finding.evidenceKeys.some((key) => !evidence.has(key))) return { Left: 'Finding cites unavailable evidence' };
      if (!finding.evidenceKeys.some((key) => evidence.get(key)?.kind === 'telemetry')) return { Left: 'Finding has no measured production context' };
    }
    return { Right: [] };
  };
  // Keep the former reviewer's patch limits while holding model and evidence constant.
  const patches = diff.split(/(?=^diff --git )/m).filter(Boolean);
  const boundedDiff = patches.slice(0, 30).map((patch) => patch.slice(0, 2000)).join('\n');
  output = resolve(resultPath);
  const pkg = JSON.parse(await readFile(new URL('./package.json', import.meta.url), 'utf8'));
  const skills = JSON.parse(await readFile(new URL('./skills/superpowers/UPSTREAM.json', import.meta.url), 'utf8'));
  const paths = ['review.mjs', 'pi-host.mjs', 'workspace.mjs', '../src/Data/Effectful/CodeReview/Artifacts.hs', '../src/Data/Effectful/CodeReview/Policy.hs', 'evaluate.mjs', 'package-lock.json', 'skills/superpowers/UPSTREAM.json', ...Object.keys(skills.sha256).map((path) => `skills/superpowers/${path}`)];
  const files = Object.fromEntries(await Promise.all(paths.map(async (path) => [path, createHash('sha256').update(await readFile(new URL(path, import.meta.url))).digest('hex')])));
  artifact = { validation: { changedLines: true, evidenceKeys: true, telemetryContext: true, semanticQuality: 'manual' }, createdAt: new Date().toISOString(), ...input, harness: { dependencies: { ...pkg.dependencies, ...pkg.devDependencies }, skills: { repository: skills.repository, revision: skills.revision }, files }, runs: [] };
  await writeFile(output, JSON.stringify(artifact, null, 2) + '\n', { flag: 'wx', mode: 0o600 });
  agentDir = await mkdtemp(join(tmpdir(), 'impact-evaluation-'));
  const runtime = await ModelRuntime.create({ authPath: join(agentDir, 'auth.json'), modelsPath: process.env.IMPACT_REVIEW_MODELS ?? null, refreshOnCreate: false });
  const model = runtime.getModel(input.provider, input.model);
  if (!model) throw new Error('Evaluation model is not configured');
  const prompt = `Review concrete production reliability, performance, and observability risks introduced in changed lines between base ${input.base} and head ${input.head}. High traffic alone is not a finding. Follow related callers where tools are available. Cite supplied evidence keys and distinguish inferred impact from observed behavior. Never claim checks ran without tool results. Return only JSON: {"findings":[{"location":{"path":"file","line":1,"side":"head"},"mechanism":"inferred risk","nextStep":"check","evidenceKeys":["key"]}],"coverage":["missing evidence"]}. At most five findings. Evidence kind: ${input.evidenceKind}; synthetic evidence is a test fixture, never a live production observation.\n${JSON.stringify(input.evidence)}`;
  for (const mode of ['bounded-patch', 'checkout-harness']) {
    const started = Date.now();
    const runSignal = AbortSignal.any([signal, AbortSignal.timeout(420000)]);
    const run = { mode, attemptId: randomUUID(), status: 'failed', tools: [], usage: [], allocations: [], submissions: [] };
    const recordUsage = (message) => run.usage.push({ ...message.usage, receipt: modelUsage(message, runtime.getModel(message.provider, message.model)?.cost ?? null, run.attemptId) });
    const checkDraft = (draft) => {
      const reply = validateDraft(draft);
      run.submissions.push({ draft, accepted: !Object.hasOwn(reply, 'Left'), error: reply.Left ?? null });
      return reply;
    };
    artifact.runs.push(run);
    let responseText;
    try {
      if (mode === 'bounded-patch') {
        run.patchFiles = Math.min(30, patches.length);
        run.totalChangedFiles = patches.length;
        const response = await runtime.completeSimple(model, { systemPrompt: 'Review production impact. You have only the supplied patch and evidence; report missing context.', messages: [{ role: 'user', content: prompt + '\nPR patch:\n' + boundedDiff, timestamp: Date.now() }] }, { reasoning: 'high', signal: runSignal });
        recordUsage(response);
        run.stopReason = response.stopReason;
        responseText = response.content.filter((block) => block.type === 'text').map((block) => block.text).join('');
        if (response.stopReason !== 'stop') throw new Error('Model request did not complete');
        const candidate = reviewDraft.parse(JSON.parse(responseText));
        const reply = checkDraft(candidate);
        if (Object.hasOwn(reply, 'Left')) {
          run.failureType = 'validation';
          run.validationError = reply.Left;
          throw new Error(reply.Left);
        }
        run.draft = candidate;
      } else {
        run.runId = randomUUID();
        run.artifactDir = output + '.artifacts';
        await mkdir(run.artifactDir, { mode: 0o700 });
        run.draft = await review({ runId: run.runId, projectId: input.projectId, repository, base: input.base, head: input.head, gitToken: '', prompt, provider: input.provider, model: input.model, thinkingLevel: 'high' }, {
          preparedCheckout: input.checkout, artifactDir: run.artifactDir, attemptId: run.attemptId, modelRuntime: runtime, guestImage: process.env.IMPACT_REVIEW_GUEST_IMAGE, cacheDir: process.env.IMPACT_REVIEW_CACHE_DIR, signal: runSignal, validateDraft: checkDraft,
          onEvent(event) {
            if (event.type === 'checkout_prepared') run.checkout = event;
            if (event.type === 'agent_allocation') run.allocations.push(event.receipt);
            if (event.type === 'tool_execution_start') run.tools.push(event.toolName);
            if (event.type === 'model_response') recordUsage(event.message);
          },
        });
      }
      run.status = 'completed';
    } catch (error) {
      if (responseText) run.invalidResponse = responseText;
      run.failureType ??= error instanceof z.ZodError ? 'schema' : error instanceof SyntaxError ? 'json' : 'execution';
      run.failure = runSignal.aborted ? 'Evaluation cancelled or timed out' : 'Model or review execution failed; no quality result';
      process.exitCode = 1;
    }
    run.durationMs = Date.now() - started;
    await writeFile(output, JSON.stringify(artifact, null, 2) + '\n');
    process.stderr.write(`${mode}: ${run.status}\n`);
    if (run.status !== 'completed' && !['schema', 'json', 'validation'].includes(run.failureType)) break;
  }
} catch {
  process.stderr.write('Evaluation could not run; check case, output path, guest image, and model configuration\n');
  process.exitCode = 1;
} finally {
  if (agentDir) await rm(agentDir, { recursive: true, force: true });
}
