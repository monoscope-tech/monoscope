#!/usr/bin/env node
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { randomUUID } from 'node:crypto';
import { writeFileSync } from 'node:fs';
import { createInterface } from 'node:readline';

process.on('uncaughtException', (error) => { writeFileSync(process.argv[2] + '.failure', error.message); process.exit(1); });

const lines = createInterface({ input: process.stdin, crlfDelay: Infinity });
const input = lines[Symbol.asyncIterator]();
const request = JSON.parse((await input.next()).value);
assert.equal(request.projectId, '00000000-0000-0000-0000-000000000000');
assert.match(request.runId, /^[a-f0-9-]{36}$/);
assert.equal(request.repository, 'https://github.com/impact-org/checkout.git');
assert.equal(request.base, 'd'.repeat(40));
assert.equal(request.head, 'a'.repeat(40));
assert.equal(request.gitToken, '', 'Git credentials stay in the Haskell checkout interpreter');
assert.equal(request.provider, 'fixture');
assert.equal(request.model, 'gpt-5.6-luna');
assert.equal(request.thinkingLevel, 'high');
assert.ok(request.prompt.includes('checkout.hs'));
assert.ok(!request.prompt.includes('long-review-log-body'));
assert.ok(!request.prompt.includes('fixture-token'));
for (const contents of [
  { service: 'unmapped-service', query: '| summarize count()' },
  { service: 'test-service', query: '| take 201' },
]) {
  process.stdout.write(JSON.stringify({ tag: 'Query', contents }) + '\n');
  assert.equal(typeof JSON.parse((await input.next()).value).Left, 'string');
}
const attemptId = request.attemptId;
assert.match(attemptId, /^[a-f0-9-]{36}$/);
const execution = { id: randomUUID(), attemptId, checkId: randomUUID(), side: 'base', revision: request.base, imageBuildId: 'fixture-image', scriptSha256: 'b'.repeat(64), regression: { tag: 'ScriptCheck', sha256: 'd'.repeat(64) }, status: 'failed', exitCode: 1, durationMs: 1250, cpus: 2, memoryMiB: 1024, logBytes: 23, logSha256: 'c'.repeat(64) };
if (process.argv.includes('--wrong-check-revision')) execution.revision = 'e'.repeat(40);
for (const contents of [execution, execution, { ...execution, id: randomUUID(), side: 'head', revision: request.head, status: 'passed', exitCode: 0 }]) {
  process.stdout.write(JSON.stringify({ tag: 'Check', contents }) + '\n');
  assert.deepEqual(JSON.parse((await input.next()).value), { Right: [] });
}
const usage = { id: randomUUID(), attemptId, provider: 'fixture', model: request.model, responseId: 'fixture-response', inputTokens: 12, outputTokens: 5, cacheReadTokens: 13, cacheWriteTokens: 17, costNanousd: 1234000, pricing: { input: 1, output: 2 }, succeeded: true };
for (const contents of [usage, usage, process.argv.includes('--conflicting-usage') ? { ...usage, costNanousd: 9999999 } : { ...usage, id: randomUUID(), responseId: null, costNanousd: null, succeeded: false }]) {
  process.stdout.write(JSON.stringify({ tag: 'Usage', contents }) + '\n');
  assert.deepEqual(JSON.parse((await input.next()).value), { Right: [] });
}
if (process.argv.includes('--fail-after-usage')) process.exit(1);
const observations = [];
for (const query of [
  'resource.service.name == "unmapped-service" or true == true | summarize events = count(), percent = 100.0 * count() / count()',
  '| summarize count() by bin_auto(timestamp)',
  '| summarize count() by bin(timestamp, 1s) | take 1',
]) {
  process.stdout.write(JSON.stringify({ tag: 'Query', contents: { service: 'test-service', query } }) + '\n');
  const evidence = JSON.parse((await input.next()).value).Right;
  assert.equal(evidence.key, `query-${observations.length + 1}`);
  assert.equal(evidence.service, 'test-service');
  assert.ok(evidence.queryResult.dataset.flat().includes(observations.length === 2 ? 1 : 3), JSON.stringify(evidence));
  assert.ok(evidence.chartUrl.includes('/widget.png?widgetZ='));
  assert.ok(!JSON.stringify(evidence).includes('long-review-log-body'));
  observations.push(evidence);
}
process.stdout.write(JSON.stringify({ tag: 'Query', contents: { service: 'test-service', query: 'resource.service.name == "unmapped-service" or true == true | summarize events = count(), percent = 100.0 * count() / count()' } }) + '\n');
assert.deepEqual(JSON.parse((await input.next()).value).Right, observations[0], 'an identical query in one review must reuse its saved snapshot and evidence key');
assert.ok(observations[0].queryResult.dataset.flat().includes(100));
assert.equal(observations[0].queryResult.limitReached, false);
assert.equal(observations[2].queryResult.dataset.length, 1);
assert.equal(observations[2].queryResult.limitReached, true);
process.stdout.write(JSON.stringify({ tag: 'Query', contents: { service: 'test-service', query: 'body contains "another-in-scope-event" | project body = body | take 1' } }) + '\n');
const logReply = JSON.parse((await input.next()).value);
const logs = logReply.Right;
assert.ok(logs, 'the reviewer must be able to inspect a scoped log sample: ' + JSON.stringify(logReply));
assert.ok(JSON.stringify(logs).includes('another-in-scope-event'));
assert.equal(logs.chartUrl, null, 'log bodies must not be embedded in public PNG snapshots');
for (const [query, expected] of [
  ['spans | name == "checkout-review-span" | project span = name, elapsed = duration | take 1', 'checkout-review-span'],
  ['metrics | metric_name == "review.temperature" | summarize average = avg(value)', '42'],
]) {
  process.stdout.write(JSON.stringify({ tag: 'Query', contents: { service: 'test-service', query } }) + '\n');
  const reply = JSON.parse((await input.next()).value);
  assert.ok(reply.Right, JSON.stringify(reply));
  assert.ok(JSON.stringify(reply.Right.queryResult).includes(expected), JSON.stringify(reply));
}
process.stdout.write(JSON.stringify({ tag: 'Query', contents: { service: 'test-service', query: 'body contains "long-review-log-body" | project message = body | take 1' } }) + '\n');
const truncated = JSON.parse((await input.next()).value).Right;
assert.equal(truncated?.queryResult.fieldsTruncated, true, 'truncated log fields must be explicit');
assert.ok(JSON.stringify(truncated.queryResult.dataset).length < 8100);
process.stdout.write(JSON.stringify({ tag: 'Knowledge', contents: { tag: 'Recall', contents: 'test-service' } }) + '\n');
const memories = JSON.parse((await input.next()).value).Right;
assert.ok(Array.isArray(memories), 'the reviewer must be able to recall scoped knowledge');
if (process.argv.includes('--expect-knowledge')) assert.ok(memories.some((note) => note.summary.includes('temperature gauge') && note.revision === request.head && note.windowEnd));
for (const contents of [{ tag: 'Recall', contents: 'unmapped-service' }, { tag: 'Remember', contents: { service: 'test-service', summary: 'Unsupported note', evidenceKeys: ['invented'] } }]) {
  process.stdout.write(JSON.stringify({ tag: 'Knowledge', contents }) + '\n');
  assert.equal(typeof JSON.parse((await input.next()).value).Left, 'string');
}
const failureMode = process.argv.find((arg) => ['--skip-submission', '--tampered-result', '--check-after-submission'].includes(arg));
process.stdout.write(JSON.stringify({ tag: 'Knowledge', contents: { tag: 'Remember', contents: { service: 'test-service', summary: failureMode ? 'Unpublished note ' + failureMode : 'Review temperature gauge was observed at 42 in the recorded window; recheck before relying on it.', evidenceKeys: ['query-6'] } } }) + '\n');
assert.ok(Object.hasOwn(JSON.parse((await input.next()).value), 'Right'));
const draft = JSON.parse(await readFile(process.argv[2], 'utf8'));
for (const finding of draft.findings) finding.evidenceKeys = [observations[0].key];
if (process.argv.includes('--skip-submission')) {
  process.stdout.write(JSON.stringify({ tag: 'Result', contents: draft }) + '\n');
  process.exit(0);
}
for (const contents of [
  { ...draft, findings: draft.findings.map((finding) => ({ ...finding, location: { path: 'unrelated.hs', line: 1, side: 'head' } })) },
  { ...draft, findings: draft.findings.map((finding) => ({ ...finding, evidenceKeys: ['invented'] })) },
]) {
  process.stdout.write(JSON.stringify({ tag: 'Validate', contents }) + '\n');
  assert.equal(typeof JSON.parse((await input.next()).value).Left, 'string');
}
process.stdout.write(JSON.stringify({ tag: 'Validate', contents: draft }) + '\n');
assert.deepEqual(JSON.parse((await input.next()).value), { Right: [] });
if (process.argv.includes('--check-after-submission')) {
  process.stdout.write(JSON.stringify({ tag: 'Check', contents: execution }) + '\n');
  assert.deepEqual(JSON.parse((await input.next()).value), { Right: [] });
}
const allocation = { id: randomUUID(), attemptId: process.argv.includes('--foreign-allocation') ? randomUUID() : attemptId, durationMs: 2000, cpus: process.argv.includes('--zero-allocation') ? 0 : 2, memoryMiB: 1024, outcome: 'released' };
for (const contents of [allocation, allocation]) {
  process.stdout.write(JSON.stringify({ tag: 'Allocation', contents }) + '\n');
  assert.deepEqual(JSON.parse((await input.next()).value), { Right: [] });
}
if (process.argv.includes('--tampered-result')) draft.coverage.push('Unvalidated change');
process.stdout.write(JSON.stringify({ tag: 'Result', contents: draft }) + '\n');
lines.close();
process.stdin.destroy();
