#!/usr/bin/env node
// Exercise release decisions with a local Git remote and a fake image registry.
import assert from 'node:assert/strict';
import { execFileSync, spawnSync } from 'node:child_process';
import { afterEach, beforeEach, test } from 'node:test';
import { mkdtempSync, mkdirSync, copyFileSync, writeFileSync, readFileSync, existsSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const script = fileURLToPath(new URL('release.sh', import.meta.url));
const digest = `sha256:${'c'.repeat(64)}`;
const image = 'ghcr.io/monoscope-tech/monoscope';
let root, repo, env, sha;
const git = (...args) => execFileSync('git', args, { cwd: repo, encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore'] }).trim();
const calls = () => existsSync(join(root, 'calls')) ? readFileSync(join(root, 'calls'), 'utf8').trim().split('\n').map(JSON.parse) : [];
const builds = () => calls().filter(args => args[0] === 'buildx' && args[1] === 'build');
const registry = () => JSON.parse(readFileSync(env.REGISTRY, 'utf8'));
const release = (args, success = true) => {
  const result = spawnSync('bash', [script, ...args], { cwd: repo, env, encoding: 'utf8' });
  assert.ifError(result.error);
  if (success) assert.equal(result.status, 0, result.stderr);
  else assert.notEqual(result.status, 0, result.stdout);
  return result;
};

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), 'monoscope-release-'));
  repo = join(root, 'repo');
  mkdirSync(join(repo, 'scripts/ci'), { recursive: true });
  copyFileSync(script, join(repo, 'scripts/ci/release.sh'));
  env = { ...process.env, REGISTRY: join(root, 'registry.json'), CALLS: join(root, 'calls'), CI_ROOT: repo, GITHUB_OUTPUT: join(root, 'outputs') };
  git('init', '-q');
  git('config', 'user.email', 'test@example.com');
  git('config', 'user.name', 'test');
  writeFileSync(join(repo, 'source'), 'original');
  git('add', '.');
  git('commit', '-qm', 'first');
  sha = git('rev-parse', 'HEAD');
  git('init', '--bare', '-q', join(root, 'remote'));
  git('remote', 'add', 'origin', join(root, 'remote'));
  git('push', '-q', 'origin', 'HEAD:master');
  const bin = join(root, 'bin');
  mkdirSync(bin);
  writeFileSync(join(bin, 'docker'), `#!/usr/bin/env node
const fs = require('node:fs');
const args = process.argv.slice(2);
fs.appendFileSync(process.env.CALLS, JSON.stringify(args) + '\\n');
const path = process.env.REGISTRY;
const state = fs.existsSync(path) ? JSON.parse(fs.readFileSync(path, 'utf8')) : {};
const is = (...prefix) => prefix.every((arg, i) => args[i] === arg);
if (is('manifest', 'inspect')) process.exit(args[2] in state ? 0 : 1);
else if (is('buildx', 'imagetools', 'inspect')) {
  const ref = args[3];
  const digest = ref === 'ghcr.io/monoscope-tech/monoscope-deps:latest' ? state.deps ?? 'sha256:' + 'a'.repeat(64)
    : ref === 'debian:12-slim' ? 'sha256:' + 'b'.repeat(64) : state[ref];
  if (!digest) process.exit(1);
  console.log(args[args.indexOf('--format') + 1] === '{{.Manifest.Digest}}' ? digest : JSON.stringify({ digest }));
} else if (is('buildx', 'build')) {
  fs.writeFileSync(process.env.CALLS + '-context.tar', fs.readFileSync(0));
  if (process.env.FAIL_BUILD) process.exit(2);
  const digest = 'sha256:' + 'c'.repeat(64);
  args.forEach((arg, i) => { if (arg === '-t') state[args[i + 1]] = digest; });
  fs.writeFileSync(path, JSON.stringify(state));
  if (args.includes('--metadata-file')) fs.writeFileSync(args[args.indexOf('--metadata-file') + 1], JSON.stringify({ 'containerimage.digest': digest }));
} else if (is('buildx', 'imagetools', 'create')) {
  state[args[args.indexOf('-t') + 1]] = args.at(-1).split('@')[1];
  fs.writeFileSync(path, JSON.stringify(state));
} else process.exit(1);
`, { mode: 0o755 });
  env.PATH = `${bin}:${env.PATH}`;
});
afterEach(() => rmSync(root, { recursive: true, force: true }));

test('squash merge reuses digest without rebuilding or moving latest', () => {
  release(['build']);
  git('commit', '--allow-empty', '-qm', 'squash metadata');
  release(['build']);
  assert.equal(builds().length, 1);
  assert.ok(!builds().some(args => args.includes(`${image}:latest`)));
  assert.equal(registry()[`${image}:${git('rev-parse', 'HEAD')}`], digest);
});

test('source and toolchain changes both rebuild', () => {
  release(['build']);
  writeFileSync(join(repo, 'source'), 'changed');
  git('add', '.');
  git('commit', '-qm', 'source change');
  release(['build']);
  const deps = `sha256:${'d'.repeat(64)}`;
  writeFileSync(env.REGISTRY, JSON.stringify({ ...registry(), deps }));
  release(['build']);
  assert.equal(builds().length, 3);
  assert.ok(builds().at(-1).includes(`DEPS_IMAGE=ghcr.io/monoscope-tech/monoscope-deps@${deps}`));
});

test('stale commit cannot publish latest', () => {
  release(['build']);
  git('commit', '--allow-empty', '-qm', 'new master');
  git('push', '-q', 'origin', 'HEAD:master');
  release(['latest', sha, digest], false);
  assert.ok(!calls().some(args => args.includes(`${image}:latest`)));
  release(['latest', git('rev-parse', 'HEAD'), digest]);
  assert.ok(calls().some(args => args.includes(`${image}:latest`)));
});

test('unreachable master and dirty tree fail closed', () => {
  git('remote', 'set-url', 'origin', join(root, 'missing'));
  release(['current-master', sha], false);
  writeFileSync(join(repo, 'source'), 'dirty');
  release(['build'], false);
  assert.deepEqual(calls(), []);
});

test('stale local deploy never calls CapRover', () => {
  release(['build']);
  git('commit', '--allow-empty', '-qm', 'new master');
  git('push', '-q', 'origin', 'HEAD:master');
  writeFileSync(join(root, 'bin/curl'), '#!/bin/sh\necho called > "$CALLS-caprover"\nexit 1\n', { mode: 0o755 });
  const result = spawnSync('bash', [join(dirname(script), 'ci.sh'), 'deploy', sha], {
    cwd: repo, env: { ...env, CAPROVER_URL: 'https://example.invalid', CAPROVER_APP: 'test', CAPROVER_APP_TOKEN: 'test' }, encoding: 'utf8',
  });
  assert.ifError(result.error);
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /stale/);
  assert.ok(!existsSync(`${env.CALLS}-caprover`));
});

test('ignored local settings are not part of build context', () => {
  writeFileSync(join(repo, '.gitignore'), 'cabal.project.local\n');
  git('add', '.');
  git('commit', '-qm', 'ignore local settings');
  writeFileSync(join(repo, 'cabal.project.local'), 'unreviewed build flags');
  release(['build']);
  const files = execFileSync('tar', ['-tf', `${env.CALLS}-context.tar`], { encoding: 'utf8' }).trim().split('\n');
  assert.ok(files.includes('source'));
  assert.ok(!files.includes('cabal.project.local'));
});

test('failed build does not publish a digest or promote', () => {
  env.FAIL_BUILD = 'true';
  release(['build'], false);
  assert.ok(!existsSync(env.REGISTRY));
  assert.ok(!existsSync(env.GITHUB_OUTPUT));
});

test('unavailable native builder falls back', () => {
  env.MONOSCOPE_BUILDER = 'missing';
  release(['build']);
  assert.ok(!builds()[0].includes('--builder'));
});

test('requested revision must match checkout', () => {
  git('commit', '--allow-empty', '-qm', 'new');
  release(['build', sha], false);
  assert.deepEqual(calls(), []);
});

test('current master deploys by digest and explicit rollback preserves latest', () => {
  release(['build']);
  writeFileSync(join(root, 'bin/curl'), `#!/usr/bin/env node
const fs = require('node:fs');
const args = process.argv.slice(2);
fs.writeFileSync(process.env.CALLS + '-caprover', args[args.indexOf('-d') + 1]);
fs.writeFileSync(args[args.indexOf('-o') + 1], '{"status":100}');
process.stdout.write('200');
`, { mode: 0o755 });
  const deploy = command => {
    const result = spawnSync('bash', [join(dirname(script), 'ci.sh'), command, sha], {
      cwd: repo, env: { ...env, CAPROVER_URL: 'https://example.invalid', CAPROVER_APP: 'test', CAPROVER_APP_TOKEN: 'test' }, encoding: 'utf8',
    });
    assert.ifError(result.error);
    assert.equal(result.status, 0, result.stderr);
    const request = JSON.parse(readFileSync(`${env.CALLS}-caprover`, 'utf8'));
    assert.equal(JSON.parse(request.captainDefinitionContent).imageName, `${image}@${digest}`);
    assert.equal(request.gitHash, sha);
  };
  deploy('deploy');
  assert.equal(registry()[`${image}:latest`], digest);
  git('commit', '--allow-empty', '-qm', 'new master');
  git('push', '-q', 'origin', 'HEAD:master');
  const currentDigest = `sha256:${'d'.repeat(64)}`;
  writeFileSync(env.REGISTRY, JSON.stringify({ ...registry(), [`${image}:latest`]: currentDigest }));
  deploy('deploy-rollback');
  assert.equal(registry()[`${image}:latest`], currentDigest);
});

test('cold Cabal cache preserves prebuilt dependencies and subsequent additions', () => {
  const dockerfile = readFileSync(new URL('../../Dockerfile', import.meta.url), 'utf8');
  const mounts = [...dockerfile.matchAll(/--mount=type=cache[^\s]*target=\/root\/\.cabal\/store[^\s]*/g)].map(([mount]) => `${mount},id=monoscope-seed-${root.split('/').at(-1)}`);
  assert.equal(mounts.length, 2);
  const fixture = `FROM debian:12-slim AS dependencies
RUN mkdir -p /root/.cabal/store && printf 'prebuilt' > /root/.cabal/store/dependency
FROM dependencies AS builder
RUN ${mounts[0]} test "$(cat /root/.cabal/store/dependency)" = prebuilt && printf 'incremental' > /root/.cabal/store/addition
RUN ${mounts[1]} test "$(cat /root/.cabal/store/dependency)" = prebuilt && test "$(cat /root/.cabal/store/addition)" = incremental
`;
  const result = spawnSync('docker', ['buildx', 'build', '--no-cache', '--progress=plain', '-f', '-', repo], {
    input: fixture, encoding: 'utf8', env: process.env,
  });
  assert.ifError(result.error);
  assert.equal(result.status, 0, result.stderr);
});
