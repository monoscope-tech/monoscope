import { createHash } from 'node:crypto';
import { mkdtemp, mkdir, open, realpath, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { isAbsolute, join } from 'node:path';
import { execFile } from 'node:child_process';
import { promisify, isDeepStrictEqual } from 'node:util';
import lockfile from 'proper-lockfile';
import writeFileAtomic from 'write-file-atomic';
import { z } from 'zod';
import { artifactRequestSchema, readPrivateArtifact, withVerifiedFixArtifacts } from './artifacts.mjs';
import { explanationSchema, readExplanationMedia } from './media.mjs';

const exec = promisify(execFile);
const assets = [
  { key: 'video', file: 'explanation.mp4', reference: '![](explanation.mp4)' },
  { key: 'diagram', file: 'cause.png', reference: '![Reproduction, inferred cause, and verified fix](cause.png)' },
];
export const publicationRequestSchema = z.object({
  artifacts: artifactRequestSchema, explanation: explanationSchema,
  targetBranch: z.string().min(1).max(200), visibility: z.enum(['public', 'private', 'internal']),
  description: z.string().min(1).max(16384).refine((text) => !/[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]/.test(text)),
}).strict();
const stateSchema = z.discriminatedUnion('phase', [
  z.object({ phase: z.literal('prepared') }).strict(),
  z.object({ phase: z.literal('draft'), number: z.number().int().positive() }).strict(),
  z.object({ phase: z.literal('uploading'), number: z.number().int().positive(), asset: z.enum(['video', 'diagram']) }).strict(),
  z.object({ phase: z.literal('complete'), number: z.number().int().positive() }).strict(),
]);
export const publicationFailureSchema = z.enum(['InvalidRequest', 'IdentityChanged', 'AccessRevoked', 'PathChanged', 'NativeUnavailable', 'SourceChanged', 'VisibilityChanged', 'PullRequestChanged', 'UploadUncertain', 'StateChanged', 'GitHubFailure', 'PushFailed', 'NotConfigured', 'ProtocolFailure', 'DeadlineExceeded', 'InvalidReceipt']);
export class PublicationError extends Error {
  constructor(code) { super(`Fix publication ${publicationFailureSchema.parse(code)}`); this.name = 'PublicationError'; this.code = code; }
}
const fail = (code) => { throw new PublicationError(code); };
const sync = async (directory) => { const handle = await open(directory, 'r'); try { await handle.sync(); } finally { await handle.close(); } };
const normalized = (text) => text.replace(/\r\n/g, '\n').trim();
const uploadedUrl = (text) => {
  try { const url = new URL(text); return url.origin === 'https://github.com' && !url.username && !url.password && !url.search && !url.hash && /^\/user-attachments\/assets\/[a-zA-Z0-9-]+$/.test(url.pathname) ? url.href : null; } catch { return null; }
};

// Host-only capability: this is never registered as an agent or repository tool.
export async function publishVerifiedFix(input, { usageDir, cacheDir, github, authorize, signal }) {
  const request = publicationRequestSchema.parse(input), { result } = request.artifacts, explanation = request.explanation;
  if (typeof authorize !== 'function' || typeof github !== 'function') fail('InvalidRequest');
  const repository = /^https:\/\/github\.com\/([A-Za-z0-9_-]+)\/([A-Za-z0-9_.-]+)\.git$/.exec(request.artifacts.repository);
  if (!repository || !isDeepStrictEqual([result.projectId, result.runId, result.candidateId, result.head, result.issueId, result.checkId, result.regressionSha256], [explanation.projectId, explanation.runId, explanation.candidateId, explanation.head, explanation.issueId, explanation.checkId, explanation.regressionSha256])) fail('IdentityChanged');
  const repo = `${repository[1]}/${repository[2]}`, endpoint = `/repos/${repo}`;
  const branch = `monoscope/fix/${result.projectId}/${result.runId}/${result.candidateId}`;
  const marker = `<!-- monoscope-fix project=${result.projectId} run=${result.runId} candidate=${result.candidateId} source=${explanation.source} -->`;
  let description = normalized(request.description);
  if (description.split('\n').filter((line) => line === marker).length !== 1) fail('IdentityChanged');
  for (const asset of assets) {
    const lines = description.split('\n');
    if (lines.filter((line) => line === asset.reference).length !== 1) fail('InvalidRequest');
    description = lines.filter((line) => line !== asset.reference).join('\n');
  }
  description = description.replace(/\n{3,}/g, '\n\n').trim();
  const mediaSha256 = createHash('sha256').update(JSON.stringify(Object.entries(explanation.files).sort(([a], [b]) => a.localeCompare(b)))).digest('hex');
  const identity = { mediaSha256, projectId: result.projectId, runId: result.runId, head: result.head, source: explanation.source, branch, targetBranch: request.targetBranch, repo, visibility: request.visibility, description };
  const cancellation = new AbortController();
  signal = AbortSignal.any([...(signal ? [signal] : []), cancellation.signal, AbortSignal.timeout(480000)]);
  const permit = async (stage) => { signal.throwIfAborted(); if (await authorize({ ...identity, stage }) !== true) fail('AccessRevoked'); signal.throwIfAborted(); };
  await permit('prepare');
  const root = await realpath(usageDir);
  const run = join(root, 'fixes', result.runId);
  if (await realpath(run) !== run) fail('PathChanged');
  let directory = run;
  for (const part of ['publications', result.candidateId, explanation.source]) {
    const next = join(directory, part); await mkdir(next, { mode: 0o700 }).catch((error) => { if (error.code !== 'EEXIST') throw error; });
    if (await realpath(next) !== next) fail('PathChanged');
    await sync(directory); directory = next;
  }
  const release = await lockfile.lock(directory, { stale: 30000, update: 5000, retries: { retries: 30, minTimeout: 100, maxTimeout: 1000 }, onCompromised: () => cancellation.abort() });
  let temporary;
  try {
    let state = { phase: 'prepared' };
    try {
      const saved = JSON.parse(await readPrivateArtifact(directory, 'state.json', 65536));
      if (!isDeepStrictEqual(saved.identity, identity)) fail('IdentityChanged');
      state = stateSchema.parse(saved.state);
    } catch (error) { if (error.code !== 'ENOENT') throw error; }
    const save = async (next) => {
      signal.throwIfAborted();
      writeFileAtomic.sync(join(directory, 'state.json'), JSON.stringify({ identity, state: next }) + '\n', { mode: 0o600 });
      await sync(directory); state = next;
    };
    if (state.phase === 'prepared') await save(state);
    temporary = await mkdtemp(join(tmpdir(), 'monoscope-publisher-'));
    await exec('git', ['check-ref-format', `refs/heads/${request.targetBranch}`], { cwd: temporary, signal, env: { PATH: process.env.PATH, GIT_CONFIG_NOSYSTEM: '1', GIT_CONFIG_GLOBAL: '/dev/null' } });
    const local = {};
    for (const asset of assets) {
      local[asset.key] = join(temporary, asset.file);
      await writeFile(local[asset.key], await readExplanationMedia({ explanation, asset: asset.key === 'diagram' ? 'cause' : asset.key }, usageDir), { mode: 0o600 });
    }
    return await withVerifiedFixArtifacts(request.artifacts, { usageDir, cacheDir, signal }, async ({ checkout }) => {
      const client = await github(temporary, signal);
      if (await client.ready() !== true) fail('NativeUnavailable');
      const refs = async (name) => {
        const matches = await client.api('GET', `${endpoint}/git/matching-refs/heads/${encodeURIComponent(name)}`);
        if (!Array.isArray(matches)) fail('GitHubFailure');
        return matches.find((value) => value.ref === `refs/heads/${name}`)?.object?.sha;
      };
      const repoAccess = async () => {
        const metadata = await client.api('GET', endpoint);
        if (metadata.visibility !== request.visibility) fail('VisibilityChanged');
        return metadata.permissions?.push === true;
      };
      const guard = async (stage) => {
        await permit(stage);
        if (!await repoAccess()) fail('AccessRevoked');
        if (await refs(request.targetBranch) !== result.base) fail('SourceChanged');
        if (stage !== 'branch' && await refs(branch) !== result.head) fail('SourceChanged');
      };
      const mediaMarker = (asset) => `<!-- monoscope-fix-media:${asset.key}:${explanation.files[asset.file]} -->`;
      const observed = (pr) => Object.fromEntries(assets.flatMap((asset) => {
        const lines = normalized(pr.body ?? '').split('\n'), positions = lines.flatMap((line, index) => line === mediaMarker(asset) ? [index] : []);
        if (positions.length > 1) fail('PullRequestChanged');
        if (!positions.length) return [];
        const line = lines.slice(positions[0] + 1).find((line) => line.trim()) ?? '';
        const url = uploadedUrl(asset.key === 'video' ? line : /^!\[Reproduction, inferred cause, and verified fix\]\(([^)]+)\)$/.exec(line)?.[1]);
        return url ? [[asset.key, url]] : [];
      }));
      const body = (urls, uploading) => description + '\n\n' + assets.map((asset) => urls[asset.key]
        ? `${mediaMarker(asset)}\n${asset.key === 'video' ? urls[asset.key] : `![Reproduction, inferred cause, and verified fix](${urls[asset.key]})`}`
        : uploading === asset.key ? `${mediaMarker(asset)}\n${asset.reference}` : `_${asset.key === 'video' ? 'Video' : 'Diagram'} attachment pending._`).join('\n\n') + '\n';
      const checked = (pr) => {
        if (!pr || !Number.isSafeInteger(pr.number) || pr.number <= 0 || pr.head?.sha !== result.head || pr.head?.ref !== branch || pr.base?.ref !== request.targetBranch || pr.head?.repo?.full_name?.toLowerCase() !== repo.toLowerCase() || pr.base?.repo?.full_name?.toLowerCase() !== repo.toLowerCase() || !['open', 'closed'].includes(pr.state) || typeof pr.draft !== 'boolean' || !(pr.body ?? '').split(/\r?\n/).includes(marker) || pr.html_url?.toLowerCase() !== `https://github.com/${repo}/pull/${pr.number}`.toLowerCase()) fail('PullRequestChanged');
        return pr;
      };
      await permit('discover');
      await repoAccess();
      const matches = await client.api('GET', `${endpoint}/pulls?state=all&head=${encodeURIComponent(repository[1] + ':' + branch)}&per_page=100`);
      if (!Array.isArray(matches) || matches.length > 1) fail('PullRequestChanged');
      let pr = matches.length ? checked(matches[0]) : undefined;
      if (state.phase !== 'prepared' && pr?.number !== state.number) fail('StateChanged');
      if (!pr) {
        await guard('branch');
        const previous = await refs(branch);
        if (previous && previous !== result.head) fail('SourceChanged');
        if (!previous) await client.push({ checkout, repository: request.artifacts.repository, branch, head: result.head });
        await guard('create');
        pr = checked(await client.api('POST', `${endpoint}/pulls`, { title: result.summary, head: branch, base: request.targetBranch, draft: true, body: body({}) }));
        await save({ phase: 'draft', number: pr.number });
      }
      for (const asset of assets) {
        pr = checked(await client.api('GET', `${endpoint}/pulls/${pr.number}`));
        const urls = observed(pr);
        if (urls[asset.key]) { if (state.phase === 'uploading' && state.asset === asset.key) await save({ phase: 'draft', number: state.number }); continue; }
        if (state.phase === 'complete') fail('StateChanged');
        if (state.phase === 'uploading') fail('UploadUncertain');
        if (pr.state !== 'open' || pr.draft !== true || normalized(pr.body) !== normalized(body(urls))) fail('PullRequestChanged');
        await guard(`attach_${asset.key}`);
        await save({ phase: 'uploading', number: pr.number, asset: asset.key });
        await client.attach({ repo, number: pr.number, body: body(urls, asset.key), file: local[asset.key], asset: asset.key });
        pr = checked(await client.api('GET', `${endpoint}/pulls/${pr.number}`));
        if (!observed(pr)[asset.key]) fail('UploadUncertain');
        await save({ phase: 'draft', number: pr.number });
      }
      if (state.phase === 'complete' || pr.state === 'closed') { await permit('complete'); await repoAccess(); }
      else await guard('complete');
      if (Object.keys(observed(pr)).length !== 2) fail('UploadUncertain');
      await save({ phase: 'complete', number: pr.number });
      return { number: pr.number, url: pr.html_url, head: result.head, source: explanation.source, mediaSha256, assets: observed(pr), draft: pr.draft, state: pr.state };
    });
  } finally { try { if (temporary) await rm(temporary, { recursive: true, force: true }); } finally { await release(); } }
}

export function nativeGitHubPublisher({ executable, token }, execute = exec) {
  if (!isAbsolute(executable) || !/^(?:(?:ghp_|gho_|github_pat_)[A-Za-z0-9_]+|[a-fA-F0-9]{40})$/.test(token)) fail('NativeUnavailable');
  return async (directory, signal) => {
    const env = { PATH: process.env.PATH, GH_CONFIG_DIR: join(directory, 'gh'), GH_TOKEN: token, GH_HOST: 'github.com', GH_PROMPT_DISABLED: '1', GH_NO_UPDATE_NOTIFIER: '1' };
    const run = async (args) => {
      try { return (await execute(executable, args, { cwd: directory, env, signal, timeout: 90000, maxBuffer: 1048576 })).stdout; } catch { fail('GitHubFailure'); }
    };
    return {
      ready: async () => /--attach\b/.test(await run(['pr', 'edit', '--help'])),
      api: async (method, path, body) => {
        const file = join(directory, 'request.json');
        if (body) await writeFile(file, JSON.stringify(body), { mode: 0o600 });
        const args = ['api', '--hostname', 'github.com', '--method', method, path, ...(body ? ['--input', file] : [])];
        const paged = method === 'GET' && (path.includes('/pulls?') || path.includes('/git/matching-refs/'));
        const value = JSON.parse(await run(paged ? [...args, '--paginate', '--slurp'] : args));
        return paged ? value.flat() : value;
      },
      attach: async ({ repo, number, body, file }) => {
        const description = join(directory, 'description.md'); await writeFile(description, body, { mode: 0o600 });
        await run(['pr', 'edit', String(number), '--repo', repo, '--body-file', description, '--attach', file]);
      },
      push: async ({ checkout, repository, branch, head }) => {
        const settings = { 'core.hooksPath': '/dev/null', 'core.fsmonitor': 'false', 'credential.helper': '', 'http.followRedirects': 'false', 'http.https://github.com/.extraheader': 'AUTHORIZATION: basic ' + Buffer.from('x-access-token:' + token).toString('base64') };
        const gitEnv = { PATH: process.env.PATH, GIT_CONFIG_NOSYSTEM: '1', GIT_CONFIG_GLOBAL: '/dev/null', GIT_ALLOW_PROTOCOL: 'https', GIT_TERMINAL_PROMPT: '0', GIT_CONFIG_COUNT: String(Object.keys(settings).length) };
        Object.entries(settings).forEach(([key, value], index) => { gitEnv[`GIT_CONFIG_KEY_${index}`] = key; gitEnv[`GIT_CONFIG_VALUE_${index}`] = value; });
        try { await execute('git', ['push', '--porcelain', `--force-with-lease=refs/heads/${branch}:`, repository, `${head}:refs/heads/${branch}`], { cwd: checkout, env: gitEnv, signal, timeout: 90000, maxBuffer: 1048576 }); } catch { fail('PushFailed'); }
      },
    };
  };
}
