#!/usr/bin/env node
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { createInterface } from 'node:readline';

const lines = createInterface({ input: process.stdin, crlfDelay: Infinity });
const input = lines[Symbol.asyncIterator]();
const envelope = JSON.parse((await input.next()).value);
assert.equal(envelope.tag, 'ReviewFixTask');
const { fix: { request }, artifacts } = envelope.contents;
const artifact = { script: "forbidden", testPatch: "forbidden", failureMessage: "forbidden" };
const mode = process.argv[2];
const rpc = async (tag, contents) => {
  process.stdout.write(JSON.stringify({ tag, contents }) + '\n');
  return JSON.parse((await input.next()).value);
};
assert.ok(Object.hasOwn(await rpc('Usage', { id: randomUUID(), attemptId: request.attemptId, provider: request.provider, model: request.model, responseId: 'candidate-review-fixture', inputTokens: 7, outputTokens: 3, cacheReadTokens: 0, cacheWriteTokens: 0, costNanousd: 42000, pricing: {}, succeeded: true }), 'Right'));
assert.ok(Object.hasOwn(await rpc('Tool', { name: 'query_telemetry', arguments: { service: 'test-service', query: '| summarize count()' } }), 'Left'));
assert.ok(Object.hasOwn(await rpc('Tool', { name: 'recall_knowledge', arguments: { service: 'test-service' } }), 'Left'));
assert.ok(Object.hasOwn(await rpc('Tool', { name: 'reproduce_issue', arguments: artifact }), 'Left'));
assert.ok(Object.hasOwn(await rpc('Phase', { id: randomUUID(), attemptId: request.attemptId, step: { tag: 'RestartReproduction' } }), 'Left'));
if (mode === '--check') {
  process.stdout.write(JSON.stringify({ tag: 'Check', contents: { id: randomUUID(), attemptId: request.attemptId, checkId: randomUUID(), side: 'base', revision: request.base, imageBuildId: randomUUID(), scriptSha256: 'a'.repeat(64), regression: null, status: 'passed', exitCode: 0, durationMs: 1, cpus: 1, memoryMiB: 1024, logBytes: 0, logSha256: 'b'.repeat(64) } }) + '\n');
  process.stdin.destroy(); lines.close();
} else {
  const decision = mode === '--reject' ? { tag: 'ReviewChanges', contents: ['The fix drops retry tokens in the alternate caller.'] } : mode === '--blocked' ? { tag: 'ReviewBlocked', contents: ['The required repository checks are unavailable.'] } : { tag: 'ReviewApproved', contents: 'The retained regression and callers support the exact retry-token fix.' };
  const result = { result: artifacts.result, decision };
  assert.ok(Object.hasOwn(await rpc('Validate', { ...result, result: { ...artifacts.result, summary: 'different candidate' } }), 'Left'));
  assert.ok(Object.hasOwn(await rpc('Validate', { ...result, decision: { ...decision, contents: ' ' } }), 'Left'));
  assert.ok(Object.hasOwn(await rpc('Validate', { ...result, decision: { ...decision, contents: 'x'.repeat(1201) } }), 'Left'));
  assert.ok(Object.hasOwn(await rpc('Validate', { ...result, decision: { tag: 'Invented', contents: 'invented' } }), 'Left'));
  if (mode !== '--skip') assert.ok(Object.hasOwn(await rpc('Validate', result), 'Right'));
  if (mode === '--tampered') result.result = { ...artifacts.result, summary: 'changed after acceptance' };
  if (mode === '--post-query') assert.ok(Object.hasOwn(await rpc('Tool', { name: 'query_telemetry', arguments: { service: 'test-service', query: '| summarize count()' } }), 'Left'));
  assert.ok(Object.hasOwn(await rpc('Allocation', { id: randomUUID(), attemptId: request.attemptId, durationMs: 100, cpus: 2, memoryMiB: 1024, outcome: 'released' }), 'Right'));
  process.stdout.write(JSON.stringify({ tag: 'Result', contents: result }) + '\n');
  lines.close(); process.stdin.destroy();
}
