import { execFile, spawn } from 'node:child_process';
import { promisify } from 'node:util';
import { mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createInterface } from 'node:readline';
import { DEFAULT_GUEST_TMPFS, VmCheckpoint } from '@earendil-works/gondolin';

const exec = promisify(execFile);
export const guestTmpfs = Object.fromEntries(Object.entries(DEFAULT_GUEST_TMPFS).filter(([path]) => path !== '/root'));
export const agentCommand = (command) => ['/usr/bin/setpriv', '--reuid=1000', '--regid=1000', '--clear-groups', '--no-new-privs', '--reset-env', '--', '/usr/bin/env', 'LANG=C.UTF-8', 'LC_ALL=C.UTF-8', ...command];
const vmOptions = (image, mounts) => ({ sessionLabel: 'monoscope-prepared-workspace', cpus: 2, memory: '1G', sandbox: { imagePath: image.assetDir, netEnabled: false }, rootfs: { mode: 'cow' }, tmpfs: guestTmpfs, vfs: { mounts } });

// Native policy; this adapter only transports host-selected inputs and SDK handles.
async function nativeInput(command, request, args, script, action) {
  const temporary = await mkdtemp(join(tmpdir(), 'monoscope-native-workspace-'));
  try {
    const requestFile = join(temporary, 'request.json'), scriptFile = join(temporary, 'prepare.sh');
    await writeFile(requestFile, JSON.stringify({ ...request, ownerPid: process.pid }), { mode: 0o600 });
    if (script !== undefined) await writeFile(scriptFile, script, { mode: 0o600 });
    return await action(process.env.IMPACT_REVIEW_NATIVE_CLI ?? 'monoscope', ['workspace', command, '--request', requestFile,
      '--bridge', fileURLToPath(new URL('./sandbox.mjs', import.meta.url)), ...args, ...(script === undefined ? [] : ['--script', scriptFile])]);
  } finally { await rm(temporary, { recursive: true, force: true }); }
}
const nativeCommand = (command, request, args, signal, script) => nativeInput(command, request, args, script, async (cli, argv) =>
  JSON.parse((await exec(cli, argv, { signal, timeout: 450000, maxBuffer: 1024 * 1024 })).stdout));
const workspaceRequest = (request, image) => ({ projectId: request.projectId, repository: request.repository, base: request.base, expectedImageBuildId: image.buildId });
const workspaceArgs = (image, options) => ['--image', image.assetDir, '--cache', options.cacheDir, ...(options.dependencyArchive ? ['--dependencies', options.dependencyArchive] : [])];
export const runRegression = (request, image, { artifactDir, checkout, workspace, signal }) => nativeCommand('check',
  { ...request, imageBuildId: image.buildId, workspace: workspace ? { tag: 'PreparedCheckpoint', contents: { path: workspace.path, sha256: workspace.manifest.sha256 } } : { tag: 'Pristine' } },
  ['--image', image.assetDir, '--artifacts', artifactDir, '--checkout', checkout], signal);

const nativeLease = (prepared, request, image, options) => nativeInput('lease', { ...workspaceRequest(request, image), expectedCheckpoint: prepared.manifest.sha256 }, workspaceArgs(image, options), options.script, async (cli, argv) => {
  const child = spawn(cli, argv, { stdio: ['pipe', 'pipe', 'ignore'] });
  const exited = new Promise((resolve, reject) => {
    child.once('error', reject);
    child.once('exit', code => code === 0 ? resolve() : reject(new Error('Native workspace lease ended unexpectedly')));
    child.stdin.on('error', reject);
  });
  void exited.catch(() => {});
  let closing;
  const close = () => closing ??= (async () => { child.stdin.end(); await exited; })();
  const lines = createInterface({ input: child.stdout, crlfDelay: Infinity });
  try {
    const reply = await lines[Symbol.asyncIterator]().next();
    if (reply.done) throw new Error('Workspace preparation or integrity verification failed');
    const leased = JSON.parse(reply.value);
    if (!leased) throw new Error('Prepared workspace is no longer available');
    return { leased, exited, close, get released() { return closing !== undefined; } };
  } catch (error) { await close().catch(() => {}); throw error; }
  finally { lines.close(); }
});

function resumePrepared(prepared, request, image, options) {
  if (!prepared) return undefined;
  const active = new Set();
  return Object.freeze({
    get path() { return active.values().next().value?.leased.prepared.path ?? prepared.path; },
    manifest: prepared.manifest,
    async resume(mounts, onFailure) {
      options.signal?.throwIfAborted();
      const lease = await nativeLease(prepared, request, image, options);
      active.add(lease);
      try {
        options.signal?.throwIfAborted();
        const vm = await VmCheckpoint.load(lease.leased.resumePath).resume(vmOptions(image, mounts));
        const release = vm.close.bind(vm);
        let closing;
        vm.close = () => closing ??= (async () => { try { await release(); } finally { try { await lease.close(); } finally { active.delete(lease); } } })();
        const lost = () => { if (!lease.released) { onFailure?.(); void vm.close().catch(() => {}); } };
        void lease.exited.then(lost, lost);
        return vm;
      } catch (error) { active.delete(lease); await lease.close().catch(() => {}); throw error; }
    },
  });
}
export async function loadPreparedWorkspace(request, image, options) {
  const prepared = await nativeCommand('load', workspaceRequest(request, image), workspaceArgs(image, options), options.signal, options.script);
  return resumePrepared(prepared, request, image, options);
}
