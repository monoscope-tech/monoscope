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

const exec = promisify(execFile);
const records = (chunks, limit) => Array.fromAsync(jsonLines(Readable.from(chunks.map((chunk) => Buffer.from(chunk))), limit));

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
