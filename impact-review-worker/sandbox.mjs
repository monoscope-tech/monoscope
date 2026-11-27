#!/usr/bin/env node
// SDK bridge only. Haskell selects commands, paths, resources and cache publication;
// the bridge only evicts guest images that the resolved guest has superseded.
import { jsonLines } from './jsonl.mjs';
import { open } from 'node:fs/promises';
import { constants, readdirSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { DEFAULT_GUEST_TMPFS, RealFSProvider, ReadonlyProvider, VM, VmCheckpoint, createHttpHooks, getImageStoreDirectory, importImageFromDirectory, listImageRefs, listSessions, removeImage, resolveImageSelector } from '@earendil-works/gondolin';
// Guests run pull request code, so they reach only package registries and GitHub, only to read: GET
// and HEAD with a fixed User-Agent, plus git's upload-pack POST so git dependencies fetch. GitHub is an
// accepted risk (2026-11-19): a repository's traffic insights list the paths its visitors fetched, so
// that code could exfiltrate source through crafted URLs; without it no git-pinned dependency builds.
const { httpHooks } = createHttpHooks({
  allowedHosts: ['deb.debian.org', 'security.debian.org', 'registry.npmjs.org', 'pypi.org', 'files.pythonhosted.org', 'index.crates.io', 'static.crates.io', 'static.rust-lang.org', 'proxy.golang.org', 'sum.golang.org', 'hackage.haskell.org', 'www.stackage.org', 'downloads.haskell.org', 'get-ghcup.haskell.org', 'nodejs.org', 'repo.maven.apache.org', 'repo1.maven.org', 'rubygems.org', 'index.rubygems.org', 'raw.githubusercontent.com', 'github.com', 'codeload.github.com', 'objects.githubusercontent.com'],
  // ghcup's release metadata is the only GitHub content a guest may read.
  isRequestAllowed: (request) => {
    const { host, pathname } = new URL(request.url);
    const read = ['GET', 'HEAD'].includes(request.method) || (request.method === 'POST' && host === 'github.com' && pathname.endsWith('/git-upload-pack'));
    return read && (host !== 'raw.githubusercontent.com' || pathname.startsWith('/haskell/ghcup-metadata/'));
  },
  onRequest: (request) => { const headers = new Headers(request.headers); headers.set('user-agent', 'monoscope-sandbox'); return new Request(request, { headers }); },
});
let vm;
const controller = new AbortController();
const abort = () => { controller.abort(); void vm?.close().catch(() => {}); process.stdin.destroy(); };
for (const signal of ['SIGTERM', 'SIGINT']) process.once(signal, abort);
process.stdin.once('end', abort);
process.stdout.once('error', abort);
try {
  for await (const line of jsonLines(process.stdin, 16 * 1024 * 1024)) {
    const { tag, contents } = JSON.parse(line);
    let result = [];
    try {
      switch (tag) {
        case 'Resolve': {
          const selected = resolveImageSelector(contents), objects = resolve(getImageStoreDirectory(), 'objects');
          let image = selected;
          if (selected.source === 'path' && dirname(selected.assetDir) !== objects) {
            image = importImageFromDirectory(selected.assetDir);
            // Sessions only run the configured guest's build; once no VM is live, every
            // other untagged import is unreachable (without this each rebuild kept 9-25 GB).
            if (!(await listSessions()).some((s) => s.alive)) {
              const kept = new Set([image.buildId, ...listImageRefs().flatMap((ref) => Object.values(ref.targets))]);
              for (const id of readdirSync(objects)) if (/^[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}$/.test(id) && !kept.has(id)) try { removeImage(id); } catch {}
            }
          }
          result = { assetDir: image.assetDir, buildId: image.buildId }; break;
        }
        case 'Resume':
        case 'Create': {
          const snapshot = tag === 'Resume' ? contents[1] : undefined;
          const config = tag === 'Resume' ? contents[0] : contents;
          if (vm || config.isolation !== 'registry_cow') throw new Error('Invalid sandbox transition');
          const options = { sessionLabel: 'monoscope-native-workspace', cpus: config.cpus, memory: `${config.memoryMiB}M`,
            sandbox: { imagePath: config.imagePath, netEnabled: true }, httpHooks, rootfs: { mode: 'cow', size: '64G' },
            tmpfs: Object.fromEntries(Object.entries(DEFAULT_GUEST_TMPFS).filter(([path]) => path !== '/root')),
            vfs: { mounts: Object.fromEntries(Object.entries(config.mounts).map(([guest, host]) => [guest, new ReadonlyProvider(new RealFSProvider(host))])) } };
          vm = snapshot ? await VmCheckpoint.load(snapshot).resume(options) : await VM.create(options);
          controller.signal.throwIfAborted();
          await vm.start(); break;
        }
        case 'GetIdentity': result = vm.id; break;
        case 'ExecuteLogged': {
          const { command, path, limitBytes, overflow } = contents;
          const log = await open(path, constants.O_WRONLY | constants.O_NOFOLLOW | constants.O_NONBLOCK);
          try {
            if (!(await log.stat()).isFile()) throw new Error('Invalid log');
            const execution = vm.exec(command.argv, { cwd: command.cwd ?? undefined, signal: controller.signal, stdout: 'pipe', stderr: 'pipe' });
            void execution.catch(() => {});
            let bytes = 0, limited = false;
            for await (const chunk of execution.output()) {
              if (bytes + chunk.data.length > limitBytes && overflow !== 'discard_output') { limited = true; await vm.close(); break; }
              const kept = chunk.data.subarray(0, limitBytes - bytes);
              if (kept.length) { await log.writeFile(kept); bytes += kept.length; }
            }
            if (limited) result = { tag: 'LogLimit' };
            else result = { tag: 'Exited', contents: (await execution).exitCode };
          } finally { try { if (overflow !== 'discard_output') await log.sync(); } finally { await log.close(); } }
          break;
        }
        case 'Execute': {
          const execution = vm.exec(contents.argv, { cwd: contents.cwd ?? undefined, signal: controller.signal, stdout: contents.quiet ? 'ignore' : 'pipe', stderr: contents.quiet ? 'ignore' : 'pipe' });
          const chunks = [], errors = []; let bytes = 0;
          if (!contents.quiet) for await (const chunk of execution.output()) {
            bytes += chunk.data.length;
            if (bytes > 512 * 1024) throw new Error('Sandbox output limit');
            (chunk.stream === 'stdout' ? chunks : errors).push(chunk.data);
          }
          // stderr says why a step failed; callers parse stdout alone.
          result = { exitCode: (await execution).exitCode, stdout: Buffer.concat(chunks).toString('utf8'), stderr: Buffer.concat(errors).toString('utf8').slice(-4096) }; break;
        }
        case 'CopyFile': {
          const file = await open(contents.source, constants.O_RDONLY | constants.O_NOFOLLOW | constants.O_NONBLOCK);
          try { if (!(await file.stat()).isFile()) throw new Error('Invalid source'); await vm.fs.writeFile(contents.destination, file.createReadStream({ autoClose: false }), { signal: controller.signal }); }
          finally { await file.close(); }
          break;
        }
        case 'InspectCheckpoint': result = VmCheckpoint.load(contents).toJSON(); break;
        case 'Checkpoint': result = (await vm.checkpoint(contents)).toJSON(); break;
        case 'Close': await vm?.close(); break;
        default: throw new Error('Unknown SDK command');
      }
      process.stdout.write(JSON.stringify({ Right: result }) + '\n');
      if (tag === 'Close') break;
    } catch {
      if (controller.signal.aborted) break;
      process.stdout.write(JSON.stringify({ Left: 'Sandbox operation failed' }) + '\n');
      break;
    }
  }
} finally { process.stdin.destroy(); await vm?.close(); }
