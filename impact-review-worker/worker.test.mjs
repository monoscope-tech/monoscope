import assert from 'node:assert/strict';
import { execFile } from 'node:child_process';
import { chmod, mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Readable } from 'node:stream';
import { test } from 'node:test';
import { promisify } from 'node:util';
import { fileURLToPath } from 'node:url';
import { jsonLines } from './jsonl.mjs';
import { parseSubmission } from './pi-host.mjs';

const exec = promisify(execFile);
const records = (chunks, limit) => Array.fromAsync(jsonLines(Readable.from(chunks.map((chunk) => Buffer.from(chunk))), limit));

test('research submissions obey the supplied host schema, including changed limits and strict nested objects', () => {
  const finding = { type: 'object', properties: { line: { type: 'integer', minimum: 1 }, message: { type: 'string', minLength: 1, maxLength: 4 } }, required: ['line', 'message'], additionalProperties: false };
  const parameters = { type: 'object', properties: { findings: { type: 'array', items: finding, maxItems: 1 } }, required: ['findings'], additionalProperties: false };
  const checkout = { policy: { tools: [{ name: 'submit_findings', parameters }] } };
  const valid = { findings: [{ line: 1, message: 'bug' }] };
  assert.equal(parseSubmission(checkout, 'submit_findings', valid), valid);
  for (const draft of [{}, { ...valid, extra: true }, { findings: [...valid.findings, ...valid.findings] }, ...[{ line: 0, message: 'bug' }, { line: 1.5, message: 'bug' }, { line: 1, message: '' }, { line: 1, message: 'large' }, { line: 1, message: 'bug', extra: true }].map((item) => ({ findings: [item] }))]) assert.throws(() => parseSubmission(checkout, 'submit_findings', draft));
  parameters.properties.findings.maxItems = 0;
  assert.throws(() => parseSubmission(checkout, 'submit_findings', valid));
  assert.throws(() => parseSubmission(checkout, 'submit_review', valid), /Host policy does not define submit_review/);
});

test('oversized JSONL records are rejected regardless of newline or chunk boundaries', async () => {
  for (const chunks of [['12345'], ['12345\n'], ['123', '45'], ['123', '45\n'], ['ok\n12345\n'], ['1234', '5\n']]) {
    await assert.rejects(records(chunks, 4), /Protocol record exceeds its size limit/);
  }
  await assert.rejects(records([Buffer.alloc(16 * 1024 * 1024 + 1, 97)]), /Protocol record exceeds its size limit/);
});

test('JSONL preserves valid records at the byte limit and LF-only framing', async () => {
  assert.deepEqual(await records(['1234\n\n12', '34\n1234'], 4), ['1234', '', '1234', '1234']);
  assert.deepEqual(await records(['abc\r', '\n'], 4), ['abc']);
  assert.deepEqual(await records(['"\u2028\u2029"\n'], 8), ['"\u2028\u2029"']);
  assert.deepEqual(await records(['é', 'é\n'], 4), ['éé']);
  await assert.rejects(records(['ééé\n'], 4), /Protocol record exceeds its size limit/);
});

test('guest builds use one available or explicitly selected engine through asset export', async (t) => {
  for (const { engines, preferred, profile = 'minimal', selected } of [
    { engines: ['podman'], selected: 'podman', profile: 'monoscope' },
    { engines: ['docker', 'podman'], selected: 'docker', profile: 'monoscope' },
    { engines: ['docker', 'podman'], preferred: 'podman', selected: 'podman' },
    { engines: ['docker'], preferred: 'podman' },
    { engines: [] },
  ]) await t.test(JSON.stringify({ engines, preferred, profile }), async () => {
    const root = await mkdtemp(join(tmpdir(), 'impact-guest-test-'));
    try {
      // Run the real builder with fake engine executables and only SDK asset export replaced.
      for (const engine of engines) {
        const path = join(root, engine);
        await writeFile(path, `#!${process.execPath}
import { appendFileSync, writeFileSync } from 'node:fs';
import { basename } from 'node:path';
const args = process.argv.slice(2);
appendFileSync(process.env.ENGINE_LOG, JSON.stringify([basename(process.argv[1]), ...args]) + '\\n');
if (args[0] === 'image') console.log('sha256:fixture');
if (args[0] === 'build') writeFileSync(args[args.indexOf('--iidfile') + 1], 'sha256:built');
`);
        await chmod(path, 0o755);
      }
      const sdk = 'data:text/javascript,' + encodeURIComponent(`
import { writeFileSync } from 'node:fs';
export const getDefaultBuildConfig = () => ({});
export const buildAssets = async (config) => writeFileSync(process.env.ASSET_CONFIG, JSON.stringify(config));
`);
      const hook = join(root, 'sdk.mjs');
      await writeFile(hook, `import { registerHooks } from 'node:module';
registerHooks({ resolve(specifier, context, next) {
  return specifier === '@earendil-works/gondolin' ? { url: ${JSON.stringify(sdk)}, shortCircuit: true } : next(specifier, context);
} });`);
      const env = { ...process.env, PATH: root, ENGINE_LOG: join(root, 'engine.jsonl'), ASSET_CONFIG: join(root, 'assets.json') };
      delete env.IMPACT_REVIEW_CONTAINER_ENGINE;
      if (preferred) env.IMPACT_REVIEW_CONTAINER_ENGINE = preferred;
      const build = () => exec(process.execPath, ['--import', hook, fileURLToPath(new URL('./build-guest.mjs', import.meta.url)), profile, join(root, 'output')], { env, timeout: 30000 });
      if (!selected) {
        await assert.rejects(build(), /No usable container engine/);
        return;
      }
      await build();
      const commands = (await readFile(env.ENGINE_LOG, 'utf8')).trim().split('\n').map(JSON.parse);
      assert.ok(commands.every(([engine]) => engine === selected));
      assert.deepEqual(commands.map(([, command]) => command), profile === 'monoscope' ? ['--version', 'image', 'tag', 'build'] : ['--version', 'build']);
      const config = JSON.parse(await readFile(env.ASSET_CONFIG, 'utf8'));
      assert.deepEqual(config.oci, { image: 'sha256:built', pullPolicy: 'never', runtime: selected });
    } finally { await rm(root, { recursive: true, force: true }); }
  });
});

test('native sessions preserve events, recoverable RPC errors and cleanup on EOF or abort', { timeout: 10000 }, async (t) => {
  const root = await mkdtemp(join(tmpdir(), 'impact-native-session-'));
  const previous = process.env.IMPACT_REVIEW_NATIVE_CLI;
  try {
    const cli = join(root, 'native.mjs');
    await writeFile(cli, `#!${process.execPath}
import assert from 'node:assert/strict';
import { readFileSync, writeFileSync } from 'node:fs';
import { createInterface } from 'node:readline';
const args = process.argv.slice(2);
assert.equal(args[0], 'workspace');
const request = JSON.parse(readFileSync(args[args.indexOf('--request') + 1], 'utf8'));
assert.equal(request.ownerPid, process.ppid);
if (args[1] === 'check') {
  assert.ok(args.includes('--bridge'));
  console.log(JSON.stringify(request));
  process.exit(0);
}
assert.equal(args[1], 'session');
assert.equal(readFileSync(request.host.workspace.script, 'utf8'), 'trusted preparation');
const send = (value) => process.stdout.write(JSON.stringify(value) + '\\n');
const finish = (outcome) => {
  writeFileSync(request.cleanup, outcome);
  process.stdout.write(JSON.stringify({ event: 'agent_allocation', receipt: { id: outcome } }) + '\\n', () => process.exit(0));
};
process.once('SIGTERM', () => finish('aborted'));
send({ event: 'agent_allocation', receipt: { id: 'before-ready' } });
send({ Right: { skills: '/fixture-skills' } });
for await (const line of createInterface({ input: process.stdin })) {
  const message = JSON.parse(line);
  if (message.tag === 'Check') send({ Left: 'Comparison budget exhausted' });
  else if (message.tag === 'Wait') send({ event: 'command_started' });
  else send({ Right: message.contents });
}
finish('eof');
`);
    await chmod(cli, 0o755);
    process.env.IMPACT_REVIEW_NATIVE_CLI = cli;
    const { runRegression, withNativeSession } = await import('./workspace.mjs');
    await t.test('standalone regression invokes the native verifier', async () => {
      const request = { projectId: 'project', base: 'base', artifact: { script: 'test' }, revisions: [['base', 'base']] };
      const result = await runRegression(request, { assetDir: '/image', buildId: 'pinned-image' }, { artifactDir: '/artifacts', checkout: '/checkout' });
      assert.deepEqual(result, { ...request, imageBuildId: 'pinned-image', workspace: { tag: 'Pristine' }, ownerPid: process.pid });
    });
    for (const abort of [false, true]) await t.test(abort ? 'abort' : 'normal EOF', async () => {
      const controller = new AbortController();
      const cleanup = join(root, abort ? 'abort-cleanup' : 'eof-cleanup');
      const events = [];
      const session = withNativeSession({ host: { workspace: {} }, cleanup }, {
        signal: controller.signal, script: 'trusted preparation',
        onEvent(event) {
          events.push(event);
          if (event.type === 'command_started') controller.abort();
        },
      }, async (ready, rpc) => {
        assert.deepEqual(ready, { skills: '/fixture-skills' });
        if (abort) return rpc('Wait', null);
        await assert.rejects(rpc('Check', {}), /Comparison budget exhausted/);
        return rpc('Operate', { path: '/workspace/source.hs' });
      });
      if (abort) await assert.rejects(session, /closed before replying/);
      else assert.deepEqual(await session, { path: '/workspace/source.hs' });
      const outcome = abort ? 'aborted' : 'eof';
      assert.equal(await readFile(cleanup, 'utf8'), outcome);
      assert.deepEqual(events.filter((event) => event.type === 'agent_allocation').map((event) => event.receipt.id), ['before-ready', outcome]);
    });
    await t.test('already aborted', async () => {
      const controller = new AbortController();
      controller.abort();
      const cleanup = join(root, 'pre-abort-cleanup');
      await assert.rejects(withNativeSession({ host: { workspace: {} }, cleanup }, {
        signal: controller.signal, script: 'trusted preparation', onEvent() {},
      }, () => assert.fail('An aborted session must not become ready')), { name: 'AbortError' });
      await assert.rejects(readFile(cleanup), { code: 'ENOENT' });
    });
  } finally {
    if (previous === undefined) delete process.env.IMPACT_REVIEW_NATIVE_CLI;
    else process.env.IMPACT_REVIEW_NATIVE_CLI = previous;
    await rm(root, { recursive: true, force: true });
  }
});
