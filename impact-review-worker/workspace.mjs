import { execFile, spawn } from 'node:child_process';
import { promisify } from 'node:util';
import { mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { jsonLines } from './jsonl.mjs';

const exec = promisify(execFile);

// Native policy; this adapter only transports host-selected inputs and SDK handles.
async function nativeInput(command, request, args, script, action) {
  const temporary = await mkdtemp(join(tmpdir(), 'monoscope-native-workspace-'));
  try {
    const requestFile = join(temporary, 'request.json'), scriptFile = join(temporary, 'prepare.sh');
    await writeFile(requestFile, JSON.stringify({ ...request, ownerPid: process.pid, ...(script !== undefined ? { host: { ...request.host, workspace: { ...request.host.workspace, script: scriptFile } } } : {}) }), { mode: 0o600 });
    if (script !== undefined) await writeFile(scriptFile, script, { mode: 0o600 });
    return await action(process.env.IMPACT_REVIEW_NATIVE_CLI ?? 'monoscope', ['workspace', command, '--request', requestFile,
      ...args]);
  } finally { await rm(temporary, { recursive: true, force: true }); }
}
const nativeCommand = (command, request, args, signal) => nativeInput(command, request, args, undefined, async (cli, argv) =>
  JSON.parse((await exec(cli, argv, { signal, timeout: 450000, maxBuffer: 1024 * 1024 })).stdout));
export const runRegression = (request, image, { artifactDir, checkout, workspace, signal }) => nativeCommand('check',
  { ...request, imageBuildId: image.buildId, workspace: workspace ? { tag: 'PreparedCheckpoint', contents: { path: workspace.path, sha256: workspace.manifest.sha256 } } : { tag: 'Pristine' } },
  ['--bridge', fileURLToPath(new URL('./sandbox.mjs', import.meta.url)), '--image', image.assetDir, '--artifacts', artifactDir, '--checkout', checkout], signal);

// The native session owns VM creation, prepared checkpoint leases, tools, checks and teardown.
export const withNativeSession = (request, { signal, script, onEvent }, action) => nativeInput('session', request, [], script, async (cli, argv) => {
  signal?.throwIfAborted();
  const child = spawn(cli, argv, { stdio: ['pipe', 'pipe', 'inherit'] });
  const exited = new Promise((resolve, reject) => {
    child.once('error', reject);
    child.once('exit', (code) => code === 0 ? resolve() : reject(new Error('Native agent session failed')));
  });
  void exited.catch(() => {});
  child.stdin.on('error', () => child.kill('SIGTERM'));
  let forced;
  const abort = () => { child.kill('SIGTERM'); forced ??= setTimeout(() => child.kill('SIGKILL'), 5000); };
  void exited.finally(() => clearTimeout(forced)).catch(() => {});
  signal?.addEventListener('abort', abort, { once: true });
  const messages = jsonLines(child.stdout, 16 * 1024 * 1024);
  const record = ({ event, ...data }) => onEvent({ type: event, ...data });
  const receive = async () => {
    for (;;) {
      const next = await messages.next();
      if (next.done) throw new Error('Native agent session closed before replying');
      const reply = JSON.parse(next.value);
      if (reply.event) { record(reply); continue; }
      if (Object.hasOwn(reply, 'Left')) throw new Error(reply.Left);
      if (!Object.hasOwn(reply, 'Right')) throw new Error('Invalid native agent response');
      return reply.Right;
    }
  };
  let pending = Promise.resolve();
  const rpc = (tag, contents) => {
    const reply = pending.then(async () => {
      signal?.throwIfAborted();
      child.stdin.write(JSON.stringify({ tag, contents }) + '\n');
      return receive();
    });
    pending = reply.catch(() => {});
    return reply;
  };
  try {
    signal?.throwIfAborted();
    const ready = await receive();
    return await action(ready, rpc);
  } finally {
    try {
      await pending;
      child.stdin.end();
      for await (const line of messages) record(JSON.parse(line));
      await exited;
    } finally { signal?.removeEventListener('abort', abort); clearTimeout(forced); }
  }
});
