#!/usr/bin/env node
import assert from 'node:assert/strict';
import { writeFileSync } from 'node:fs';
import { randomUUID } from 'node:crypto';
import { createInterface } from 'node:readline';

process.on('uncaughtException', (error) => { writeFileSync(process.argv[2], error.message); process.exit(1); });
const lines = createInterface({ input: process.stdin, crlfDelay: Infinity });
const input = lines[Symbol.asyncIterator]();
const envelope = JSON.parse((await input.next()).value);
assert.equal(envelope.tag, 'FixTask');
const { request, issueId } = envelope.contents;
const mode = process.argv[4];
const rpc = async (tag, contents) => {
  process.stdout.write(JSON.stringify({ tag, contents }) + '\n');
  return JSON.parse((await input.next()).value);
};
const step = (value) => ({ id: randomUUID(), attemptId: request.attemptId, step: value });
const candidate = { revision: 'b'.repeat(40), rootCause: 'The runtime drops retry tokens.' };
assert.ok(Object.hasOwn(await rpc('Phase', step({ tag: 'RestartReproduction' })), 'Right'));
if (mode.startsWith('--app-job')) {
  const observation = await rpc('Query', { service: 'test-service', query: '| summarize events = count()' });
  assert.ok(Object.hasOwn(observation, 'Right'));
  assert.ok(Object.hasOwn(await rpc('Knowledge', { tag: 'Recall', contents: 'test-service' }), 'Right'));
  assert.ok(Object.hasOwn(await rpc('Knowledge', { tag: 'Remember', contents: { service: 'test-service', summary: 'Retry token preservation needs a retained runtime regression.', evidenceKeys: [observation.Right.key] } }), 'Right'));
}
assert.deepEqual(await rpc('Phase', step({ tag: 'ProposeCandidate', contents: candidate })), { Left: 'ReproductionRequired' });
const artifact = { script: 'bash regression.sh', testPatch: 'admitted-test-patch', failureMessage: 'retry token was lost' };
assert.ok(Object.hasOwn(await rpc('Admit', { ...artifact, testPatch: 'invalid-test' }), 'Left'));
assert.ok(Object.hasOwn(await rpc('Admit', artifact), 'Right'));
const call = { id: randomUUID(), attemptId: request.attemptId, provider: request.provider, model: request.model };
assert.ok(Object.hasOwn(await rpc('Call', call), 'Right'));
const usage = { id: randomUUID(), attemptId: request.attemptId, callId: call.id, provider: request.provider, model: request.model, responseId: 'fix-fixture', inputTokens: 12, outputTokens: 3, cacheReadTokens: 0, cacheWriteTokens: 0, costNanousd: 42000, pricing: {}, succeeded: true };
assert.ok(Object.hasOwn(await rpc('Usage', usage), 'Right'));
const check = { id: randomUUID(), attemptId: request.attemptId, checkId: randomUUID(), side: 'base', revision: request.base, imageBuildId: process.argv[3], scriptSha256: 'd'.repeat(64), regression: { tag: 'InjectedTest', sha256: 'e'.repeat(64) }, status: 'failed', exitCode: 1, durationMs: 10, cpus: 2, memoryMiB: 1024, logBytes: 20, logSha256: 'f'.repeat(64) };
if (mode.startsWith('--app-job')) {
  assert.ok(Object.hasOwn(await rpc('Phase', step({ tag: 'Observe', contents: check })), 'Left'), 'an unrecorded check cannot advance the job');
  assert.ok(Object.hasOwn(await rpc('Phase', { ...step({ tag: 'RestartReproduction' }), attemptId: randomUUID() }), 'Left'), 'an unknown attempt cannot mutate the job');
}
assert.ok(Object.hasOwn(await rpc('Check', check), 'Right'));
if (mode.startsWith('--app-job')) assert.ok(Object.hasOwn(await rpc('Phase', step({ tag: 'Observe', contents: { ...check, logBytes: check.logBytes + 1 } })), 'Left'), 'the execution receipt must match exactly');
const reproduction = step({ tag: 'Observe', contents: check });
const reproduced = await rpc('Phase', reproduction);
if (mode === '--deny-phase') { assert.ok(Object.hasOwn(reproduced, 'Left')); process.exit(1); }
assert.ok(Object.hasOwn(reproduced, 'Right'));
assert.ok(Object.hasOwn(await rpc('Phase', reproduction), 'Right'), 'phase redelivery must be idempotent');
assert.ok(Object.hasOwn(await rpc('Phase', step({ tag: 'ProposeCandidate', contents: candidate })), 'Right'));
const base = { ...check, id: randomUUID(), checkId: randomUUID() };
const head = { ...base, id: randomUUID(), side: 'head', revision: candidate.revision, regression: { ...base.regression, tag: 'RetainedTest' }, status: 'passed', exitCode: 0 };
const observations = [base, head].map((receipt) => step({ tag: 'Observe', contents: receipt }));
for (const observation of observations) {
  assert.ok(Object.hasOwn(await rpc('Check', observation.step.contents), 'Right'));
  assert.ok(Object.hasOwn(await rpc('Phase', observation), 'Right'));
}
const result = { projectId: request.projectId, runId: request.runId, issueId, attemptId: request.attemptId, candidateId: randomUUID(), base: request.base, head: candidate.revision, patchSha256: 'a'.repeat(64), bundleSha256: 'b'.repeat(64), rootCause: candidate.rootCause, checkId: head.checkId, regressionSha256: head.regression.sha256, summary: 'Preserve retry tokens.' };
assert.ok(Object.hasOwn(await rpc('Validate', { ...result, head: 'branch-name' }), 'Left'), 'invalid revision must fail at the typed boundary');
assert.ok(Object.hasOwn(await rpc('Validate', { ...result, runId: randomUUID() }), 'Left'));
assert.ok(Object.hasOwn(await rpc('Validate', { ...result, rootCause: 'Invented after the candidate proposal' }), 'Left'));
assert.ok(Object.hasOwn(await rpc('Validate', { ...result, checkId: randomUUID() }), 'Left'));
assert.ok(Object.hasOwn(await rpc('Validate', result), 'Right'));
if (mode === '--tampered-result' || mode === '--app-job-tampered') result.summary = 'Changed after acceptance';
if (mode === '--post-phase') assert.ok(Object.hasOwn(await rpc('Phase', observations[1]), 'Right'));
const allocation = { id: randomUUID(), attemptId: request.attemptId, durationMs: 20, cpus: 2, memoryMiB: 1024, outcome: mode === '--post-phase' ? 'release_failed' : 'released' };
assert.ok(Object.hasOwn(await rpc('Allocation', allocation), 'Right'));
assert.ok(Object.hasOwn(await rpc('Allocation', allocation), 'Right'));
process.stdout.write(JSON.stringify({ tag: 'Result', contents: result }) + '\n');
lines.close();
process.stdin.destroy();
