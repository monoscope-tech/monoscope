// Research runners (evaluate.mjs, reviewbench.mjs) boot their own Gondolin VM here; production VMs are host-owned.
import { execFile } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { cp, mkdtemp, mkdir, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { basename, isAbsolute, join } from 'node:path';
import { constants, createReadStream } from 'node:fs';
import { RealFSProvider, ReadonlyProvider, VM, importImageFromDirectory, resolveImageSelector } from '@earendil-works/gondolin';
import { promisify } from 'node:util';
import { fileURLToPath } from 'node:url';
import { createReadToolDefinition } from '@earendil-works/pi-coding-agent';
import { z } from 'zod';
import { bashTool, cwd, runPi } from './pi-host.mjs';
import { agentCommand, guestTmpfs, loadPreparedWorkspace, runRegression } from './workspace.mjs';
export { modelUsage } from './pi-host.mjs';

const exec = promisify(execFile);
const sha = z.string().regex(/^[a-f0-9]{40}(?:[a-f0-9]{24})?$/);
const reviewRequest = z.object({
  projectId: z.uuid(), repository: z.string().min(1), base: sha, head: sha,
  gitToken: z.string(), prompt: z.string(), model: z.string().min(1),
  provider: z.string().min(1), thinkingLevel: z.enum(['off', 'minimal', 'low', 'medium', 'high', 'xhigh']),
});
export const reviewDraft = z.object({
  findings: z.array(z.object({
    location: z.object({ path: z.string(), line: z.number().int().positive(), side: z.enum(['base', 'head']) }),
    mechanism: z.string().min(1).max(1200), nextStep: z.string().min(1).max(600),
    evidenceKeys: z.array(z.string()).min(1).max(3),
  })).max(5),
  coverage: z.array(z.string().min(1).max(400)).max(10),
});


export async function lockWorkspace(vm, cwd, signal) {
  for (const command of [
    ['/usr/bin/git', 'config', '--system', '--add', 'safe.directory', cwd],
    ['/bin/chown', '-hR', '0:0', cwd],
    ['/bin/chmod', '-R', 'u+rwX,go+rX,go-w', cwd],
    ['/usr/bin/git', 'update-index', '--refresh'],
  ]) if (!(await vm.exec(command, { cwd, signal })).ok) throw new Error('Workspace could not be protected');
}

export const review = (input, options) => runRepositoryAgent(input, options);

export async function runRepositoryAgent(input, { modelRuntime, guestImage, workspaceCache, artifactDir, preparedCheckout, attemptId = randomUUID(), skillPaths = [], validateDraft, createWorkflow, signal, onEvent = () => {} }) {
  const request = reviewRequest.parse(input);
  const runId = artifactDir ? z.uuid().parse(input.runId) : undefined;
  if (!preparedCheckout) throw new Error('A host-prepared checkout is required');
  const resolvedImage = resolveImageSelector(guestImage);
  const pinnedImage = resolvedImage.source === 'path' ? importImageFromDirectory(resolvedImage.assetDir) : resolvedImage;
  const root = await mkdtemp(join(tmpdir(), 'monoscope-review-'));
  const checkout = preparedCheckout.directory;
  const skillsDir = join(root, 'skills');
  let vm, workspace, closing, allocationStartedAt;
  const verifierSession = randomUUID();
  const close = () => closing ??= vm?.close();
  const abort = () => void close()?.catch(() => {});
  try {
    const gitEnv = { PATH: process.env.PATH, GIT_ALLOW_PROTOCOL: 'file', GIT_TERMINAL_PROMPT: '0', GIT_CONFIG_NOSYSTEM: '1', GIT_CONFIG_GLOBAL: '/dev/null',
      GIT_CONFIG_COUNT: '2', GIT_CONFIG_KEY_0: 'core.hooksPath', GIT_CONFIG_VALUE_0: '/dev/null', GIT_CONFIG_KEY_1: 'core.fsmonitor', GIT_CONFIG_VALUE_1: 'false' };
    if (!isAbsolute(checkout) || !isAbsolute(preparedCheckout.repositorySkills ?? '') || typeof preparedCheckout.rootInstructions !== 'string' || request.gitToken || preparedCheckout.projectId !== request.projectId || preparedCheckout.repository !== request.repository || preparedCheckout.base !== request.base || preparedCheckout.head !== request.head) throw new Error('Host checkout identity mismatch');
    if ((await exec('git', ['rev-parse', 'HEAD'], { cwd: checkout, env: gitEnv, signal, timeout: 60000 })).stdout.trim() !== request.head) throw new Error('Host checkout revision mismatch');
    onEvent({ type: 'checkout_prepared', cacheStatus: preparedCheckout.cacheStatus, durationMs: preparedCheckout.durationMs });
    await mkdir(skillsDir);
    await Promise.all([...skillPaths, fileURLToPath(new URL('./skills/superpowers', import.meta.url))].map((path, index) => cp(path, join(skillsDir, String(index)), { recursive: true, dereference: true })));
    signal?.addEventListener('abort', abort, { once: true });
    const workspaceStarted = performance.now();
    workspace = workspaceCache ? await loadPreparedWorkspace(request, pinnedImage, workspaceCache) : undefined;
    onEvent({ type: 'workspace_cache', status: workspace ? 'hit' : workspaceCache ? 'miss' : 'disabled', sha256: workspace?.manifest.sha256 ?? null, checkpointBytes: workspace?.manifest.bytes ?? 0, durationMs: performance.now() - workspaceStarted });
    if (workspaceCache?.dependencyArchive && !workspace) throw new Error('Configured dependency workspace has not been prepared for these pinned inputs');
    const git = ['/usr/bin/git', '-c', `safe.directory=${cwd}`, '-c', 'core.hooksPath=/dev/null', '-C', cwd];
    const mounts = { '/checkout': new ReadonlyProvider(new RealFSProvider(checkout)), '/skills': new ReadonlyProvider(new RealFSProvider(skillsDir)) };
    allocationStartedAt = performance.now();
    vm = workspace ? await workspace.resume(mounts, abort) : await VM.create({
      sessionLabel: 'monoscope-impact-review', memory: '1G', cpus: 2,
      sandbox: { imagePath: pinnedImage.assetDir, netEnabled: false }, rootfs: { mode: 'cow' }, tmpfs: guestTmpfs, vfs: { mounts },
    });
    signal?.throwIfAborted();
    await vm.start();
    onEvent({ type: 'sandbox_started', id: vm.id, hostPid: vm.getHostPid() });
    signal?.throwIfAborted();
    // Guest-owned files keep Git's ownership checks and all review writes inside the VM.
    for (const command of [
      ...(workspace ? [cwd, '/checkout/.git'].map((path) => ['/usr/bin/git', 'config', '--system', '--add', 'safe.directory', path]) : []),
      ...(workspace ? [[...git, 'fetch', '--quiet', '--no-tags', '--', '/checkout', request.head]] : [['/bin/mkdir', '-p', cwd], ['/bin/cp', '-R', '/checkout/.', cwd]]),
      [...git, 'checkout', '--quiet', '--detach', request.head],
      ['/bin/chown', '-hR', '1000:1000', cwd],
    ]) if (!(await vm.exec(command, { signal })).ok) throw new Error('Repository could not be prepared in the guest');
    await vm.fs.mkdir('/run/monoscope-review-output', { recursive: true, mode: 0o755 });
    const identity = await vm.exec(agentCommand(['/usr/bin/id', '-u']), { signal });
    if (!identity.ok || identity.stdout.trim() !== '1000') throw new Error('Guest image requires the review-agent account and setpriv');
    const tools = [bashTool(async (command, cwd, options) => {
      const commandSignal = AbortSignal.any([signal, options.signal, options.timeout ? AbortSignal.timeout(options.timeout * 1000) : undefined].filter(Boolean));
      try {
        const proc = vm.exec(agentCommand(['/bin/bash', '-lc', command]), { cwd, signal: commandSignal, stdout: 'pipe', stderr: 'pipe' });
        for await (const chunk of proc.output()) options.onData(chunk.data);
        return { exitCode: (await proc).exitCode };
      } catch (error) {
        // Gondolin aborts the host wait; closing also terminates any guest process.
        if (commandSignal.aborted) await close();
        throw error;
      }
    }, async (path) => {
      const guestPath = '/run/monoscope-review-output/' + basename(path);
      await vm.fs.writeFile(guestPath, createReadStream(path), { signal });
      return guestPath;
    }), createReadToolDefinition(cwd, { operations: {
      access: (path) => vm.fs.access(path, { mode: constants.R_OK }),
      readFile: (path) => vm.fs.readFile(path),
    } })];
    const executeRegression = async (artifact, toolSignal) => {
      const active = AbortSignal.any([signal, toolSignal].filter(Boolean));
      const result = await runRegression({ projectId: request.projectId, runId, attemptId, sessionId: verifierSession, base: request.base, artifact, revisions: [['base', request.base], ['head', request.head]] }, pinnedImage,
        { artifactDir, checkout, workspace, signal: active });
      for (const receipt of result.receipts) onEvent({ type: 'regression_execution', receipt });
      for (const measured of result.report.results) await vm.fs.writeFile(measured.logPath, createReadStream(join(artifactDir, result.report.id, measured.side + '.log')), { signal: active });
      return { content: [{ type: 'text', text: JSON.stringify(result.report) }], details: {}, isError: result.isError };
    };
    if (artifactDir && !createWorkflow && preparedCheckout.policy.tools.some((tool) => tool.name === 'run_regression')) tools.push({ name: 'run_regression', execute: (_id, params, toolSignal) => executeRegression(params, toolSignal) });
    const workflow = createWorkflow ? { ...await createWorkflow({ vm, cwd, request, root, artifactDir, executeRegression, signal, rootInstructions: preparedCheckout.rootInstructions }), sequential: true } : {
      name: preparedCheckout.policy.tools.find((tool) => tool.name.startsWith('submit_'))?.name,
      prepare: async (draft) => {
        const reply = validateDraft ? await validateDraft(draft) : { Right: [] };
        return Object.hasOwn(reply, 'Left') ? reply : { Right: draft };
      },
    };
    return await runPi({ preparedCheckout, request, modelRuntime, attemptId, tools, workflow, skillsDir, root, signal, onEvent });
  } finally {
    signal?.removeEventListener('abort', abort);
    let outcome = 'released';
    try { await close(); } catch (error) { outcome = 'release_failed'; throw error; } finally {
      try {
        if (vm) onEvent({ type: 'agent_allocation', receipt: { id: vm.id, attemptId, durationMs: Math.ceil(performance.now() - allocationStartedAt), cpus: 2, memoryMiB: 1024, outcome } });
      } finally {
        await rm(root, { recursive: true, force: true });
      }
    }
  }
}
