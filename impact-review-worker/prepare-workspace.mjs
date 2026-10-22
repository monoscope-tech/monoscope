#!/usr/bin/env node
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { importImageFromDirectory, resolveImageSelector } from '@earendil-works/gondolin';
import { prepareWorkspace } from './workspace.mjs';

const controller = new AbortController();
for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, () => controller.abort());

// Operator-only preparation. No model, remote clone, or repository publication.
try {
  const [requestPath, checkout, scriptPath, dependencyArchive] = process.argv.slice(2);
  if (!requestPath || !checkout || !scriptPath || !process.env.IMPACT_REVIEW_GUEST_IMAGE || !process.env.IMPACT_REVIEW_CACHE_DIR) throw new Error('Missing preparation input');
  const selected = resolveImageSelector(process.env.IMPACT_REVIEW_GUEST_IMAGE);
  const image = selected.source === 'path' ? importImageFromDirectory(selected.assetDir) : selected;
  const prepared = await prepareWorkspace(JSON.parse(await readFile(requestPath, 'utf8')), image, {
    checkout: resolve(checkout), dependencyArchive: dependencyArchive && resolve(dependencyArchive), cacheDir: process.env.IMPACT_REVIEW_CACHE_DIR, script: await readFile(scriptPath, 'utf8'), signal: AbortSignal.any([controller.signal, AbortSignal.timeout(420000)]),
  });
  process.stdout.write(JSON.stringify(prepared.manifest) + '\n');
} catch {
  process.stderr.write('Workspace preparation failed; no cache was promoted\n');
  process.exitCode = 1;
}
