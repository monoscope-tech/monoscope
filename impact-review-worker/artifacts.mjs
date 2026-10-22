import { createHash } from 'node:crypto';
import { constants } from 'node:fs';
import { open, realpath, mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { execFile } from 'node:child_process';
import { promisify, isDeepStrictEqual } from 'node:util';
import lockfile from 'proper-lockfile';
import { z } from 'zod';

const exec = promisify(execFile);
const hash = (bytes) => createHash('sha256').update(bytes).digest('hex');
const digest = z.string().regex(/^[a-f0-9]{64}$/);
const revision = z.string().regex(/^(?:[a-f0-9]{40}|[a-f0-9]{64})$/);
const resultSchema = z.object({
  projectId: z.uuid(), runId: z.uuid(), issueId: z.uuid(), attemptId: z.uuid(), candidateId: z.uuid(),
  base: revision, head: revision, patchSha256: digest, bundleSha256: digest,
  rootCause: z.string().min(1).max(1200).refine((text) => text.trim().length > 0), checkId: z.uuid(), regressionSha256: digest, summary: z.string().min(1).max(600).refine((text) => text.trim().length > 0),
}).strict();
const receiptSchema = z.object({
  id: z.uuid(), attemptId: z.uuid(), checkId: z.uuid(), side: z.enum(['base', 'head']), revision,
  imageBuildId: z.uuid(), workspace: digest.nullable().optional().default(null), scriptSha256: digest, regression: z.object({ tag: z.enum(['InjectedTest', 'RetainedTest']), sha256: digest }).strict(),
  status: z.enum(['failed', 'passed']), exitCode: z.number().int(), durationMs: z.number().int().nonnegative(),
  cpus: z.number().int().positive(), memoryMiB: z.number().int().positive(), logBytes: z.number().int().nonnegative().max(1048576), logSha256: digest,
}).strict();
const require = (condition) => { if (!condition) throw new Error('Retained fix artifacts do not match the verified candidate'); };

export async function readPrivateArtifact(directory, name, limit) {
  require(await realpath(directory) === directory);
  const file = await open(join(directory, name), constants.O_RDONLY | constants.O_NOFOLLOW | constants.O_NONBLOCK);
  try {
    const stat = await file.stat(); require(stat.isFile() && stat.size <= limit);
    const bytes = Buffer.alloc(stat.size + 1);
    let size = 0;
    while (size < bytes.length) { const chunk = await file.read(bytes, size, bytes.length - size, null); if (!chunk.bytesRead) break; size += chunk.bytesRead; }
    require(size === stat.size);
    return bytes.subarray(0, size);
  } finally { await file.close(); }
}

export const artifactRequestSchema = z.object({ repository: z.string().min(1), result: resultSchema, baseline: receiptSchema, candidate: receiptSchema }).strict();

// No model, guest, network, or repository code is executed by this role.
export async function verifyFixArtifacts(input, options) {
  return withVerifiedFixArtifacts(input, options, ({ result }) => result);
}

export async function withVerifiedFixArtifacts(input, { usageDir, cacheDir, signal }, action) {
  const { repository, result, baseline, candidate } = artifactRequestSchema.parse(input);
  require(baseline.side === 'base' && baseline.status === 'failed' && baseline.exitCode > 0 && ![126, 127].includes(baseline.exitCode));
  require(candidate.side === 'head' && candidate.status === 'passed' && candidate.exitCode === 0 && candidate.regression.tag === 'RetainedTest');
  require(baseline.revision === result.base && candidate.revision === result.head && result.base !== result.head && baseline.id !== candidate.id);
  for (const receipt of [baseline, candidate]) require(receipt.attemptId === result.attemptId && receipt.checkId === result.checkId && receipt.regression.sha256 === result.regressionSha256);
  for (const key of ['imageBuildId', 'workspace', 'scriptSha256', 'cpus', 'memoryMiB']) require(baseline[key] === candidate[key]);
  const root = await realpath(usageDir);
  const checks = join(root, 'fixes', result.runId, 'checks');
  const candidateDir = join(checks, 'candidates', result.candidateId), executionDir = join(checks, result.checkId);
  // Canonical parents plus O_NOFOLLOW prevent evidence from escaping the private run.
  for (const directory of [candidateDir, executionDir]) require(await realpath(directory) === directory);
  const metadata = JSON.parse(await readPrivateArtifact(candidateDir, 'candidate.json', 65536));
  const { checkId: _, regressionSha256: __, summary: ___, ...expectedMetadata } = result;
  require(isDeepStrictEqual(metadata, expectedMetadata));
  const patch = await readPrivateArtifact(candidateDir, 'fix.patch', 1048576), bundle = await readPrivateArtifact(candidateDir, 'candidate.bundle', 64 * 1048576);
  require(hash(patch) === result.patchSha256 && hash(bundle) === result.bundleSha256);
  const report = JSON.parse(await readPrivateArtifact(executionDir, 'report.json', 65536));
  for (const [key, value] of Object.entries({ id: result.checkId, projectId: result.projectId, runId: result.runId, attemptId: result.attemptId, imageBuildId: baseline.imageBuildId, scriptSha256: baseline.scriptSha256, regressionSha256: result.regressionSha256, status: 'completed' })) require(report[key] === value);
  require((report.workspace ?? null) === baseline.workspace);
  const script = (await readPrivateArtifact(executionDir, 'script.sh', 262144)).toString('utf8');
  const testPatch = (await readPrivateArtifact(executionDir, 'test.patch', 1048576)).toString('utf8');
  require(hash(script) === baseline.scriptSha256 && hash(testPatch) === report.testPatchSha256 && typeof report.failureMessage === 'string' && report.failureMessage.length > 0);
  require(hash(JSON.stringify({ script, testPatch, failureMessage: report.failureMessage })) === result.regressionSha256);
  require(Array.isArray(report.results) && report.results.length === 2);
  require(digest.safeParse(report.results[0].testFilesSha256).success && report.results[0].testFilesSha256 === report.results[1].testFilesSha256);
  const logs = {};
  for (const [index, receipt] of [baseline, candidate].entries()) {
    const row = report.results[index];
    for (const key of ['id', 'side', 'revision', 'status', 'exitCode', 'logBytes', 'logSha256']) require(row[key] === receipt[key]);
    require(Number.isFinite(row.durationMs) && row.durationMs >= 0 && Math.ceil(row.durationMs) === receipt.durationMs);
    require(row.configuredResources?.cpus === receipt.cpus && row.configuredResources?.memoryMiB === receipt.memoryMiB);
    require(digest.safeParse(row.sourceBefore).success && row.sourceBefore === row.sourceAfter);
    require(row.testPatchDisposition === (receipt.regression.tag === 'RetainedTest' ? 'retained' : 'injected'));
    const log = await readPrivateArtifact(executionDir, `${receipt.side}.log`, 1048576);
    require(log.length === receipt.logBytes && hash(log) === receipt.logSha256);
    if (index === 0) require(log.toString('utf8').includes(report.failureMessage));
    logs[receipt.side] = log;
  }
  const boundary = bundle.indexOf('\n\n'); require(boundary > 0 && boundary < 65536);
  const [signature, ...header] = bundle.subarray(0, boundary).toString('utf8').split('\n');
  require(['# v2 git bundle', '# v3 git bundle'].includes(signature));
  const prerequisites = header.filter((line) => line.startsWith('-')).map((line) => line.slice(1).split(' ')[0]);
  const refs = header.filter((line) => !line.startsWith('-') && !line.startsWith('@'));
  require(isDeepStrictEqual(prerequisites, [result.base]) && isDeepStrictEqual(refs, [`${result.head} refs/heads/monoscope-candidate/${result.candidateId}`]));
  require(header.filter((line) => line.startsWith('@')).every((line) => signature === '# v3 git bundle' && line === `@object-format=${result.head.length === 40 ? 'sha1' : 'sha256'}`));
  const temporary = await mkdtemp(join(tmpdir(), 'monoscope-fix-artifacts-'));
  const git = async (cwd, ...args) => (await exec('git', ['-c', 'core.hooksPath=/dev/null', '-c', 'core.fsmonitor=false', ...args], {
    cwd, env: { PATH: process.env.PATH, LC_ALL: 'C', GIT_CONFIG_NOSYSTEM: '1', GIT_CONFIG_GLOBAL: '/dev/null', GIT_ALLOW_PROTOCOL: 'file' }, signal, timeout: 30000, maxBuffer: 1048576,
  })).stdout.trim();
  try {
    const mirror = join(await realpath(cacheDir), result.projectId, hash(repository));
    require(await realpath(mirror) === mirror);
    const release = await lockfile.lock(mirror, { stale: 30000, update: 5000, retries: { retries: 30, minTimeout: 100, maxTimeout: 1000 } });
    const checkout = join(temporary, 'checkout'), bundlePath = join(temporary, 'candidate.bundle'), patchPath = join(temporary, 'fix.patch');
    try { await git(temporary, 'clone', '--quiet', '--no-hardlinks', '--no-checkout', mirror, checkout); } finally { await release(); }
    await writeFile(bundlePath, bundle, { mode: 0o600 }); await writeFile(patchPath, patch, { mode: 0o600 });
    await git(checkout, 'bundle', 'verify', bundlePath);
    // Unlike negotiated fetch, unbundle validates the pack even when HEAD is cached.
    await git(checkout, 'bundle', 'unbundle', bundlePath);
    require(await git(checkout, 'rev-list', '--parents', '-n', '1', result.head) === `${result.head} ${result.base}`);
    await git(checkout, 'checkout', '--quiet', '--detach', result.base);
    await git(checkout, 'apply', '--index', patchPath);
    require(await git(checkout, 'write-tree') === await git(checkout, 'rev-parse', `${result.head}^{tree}`));
    return await action({ result, checkout, regression: { script, testPatch, failureMessage: report.failureMessage }, logs });
  } finally { await rm(temporary, { recursive: true, force: true }); }
}
