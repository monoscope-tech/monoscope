import { execFileSync } from 'node:child_process';
import { cp, mkdtemp, readFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { buildAssets, getDefaultBuildConfig } from '@earendil-works/gondolin';
import { z } from 'zod';

const cwd = fileURLToPath(new URL('.', import.meta.url));
const profile = z.enum(['minimal', 'monoscope']).parse(process.argv[2] ?? 'minimal');
const outputDir = resolve(cwd, process.argv[3] ?? (profile === 'minimal' ? '.guest' : '.guest-monoscope'));
const preferred = z.enum(['docker', 'podman']).optional().parse(process.env.IMPACT_REVIEW_CONTAINER_ENGINE);
const engine = (preferred ? [preferred] : ['docker', 'podman']).find((candidate) => {
  try { execFileSync(candidate, ['--version'], { stdio: 'ignore' }); return true; }
  catch { return false; }
});
if (!engine) throw new Error(`No usable container engine: ${preferred ?? 'install Docker or Podman'}`);
const context = await mkdtemp(join(tmpdir(), 'monoscope-guest-build-'));
try {
  await cp(join(cwd, profile === 'minimal' ? 'Dockerfile.guest' : 'Dockerfile.monoscope'), join(context, 'Dockerfile'));
  const args = ['build', '--iidfile', join(context, 'image-id')];
  if (profile === 'monoscope') {
    const source = process.env.IMPACT_REVIEW_DEPS_IMAGE ?? 'ghcr.io/monoscope-tech/monoscope-deps:latest';
    const imageId = execFileSync(engine, ['image', 'inspect', source, '--format', '{{.Id}}'], { encoding: 'utf8' }).trim();
    const pinned = `monoscope-impact-review-deps:${imageId.replace('sha256:', '')}`;
    execFileSync(engine, ['tag', imageId, pinned]);
    args.push('--build-arg', `DEPS_IMAGE=${pinned}`);
    // Send only CLI build inputs, never local credentials, artifacts, or caches.
    for (const path of ['monoscope.cabal', 'cabal.project', 'cabal.project.freeze', 'Makefile', 'shared/monoscope-shared.cabal', 'shared/src', 'cli/monoscope-cli.cabal', 'cli/CLI', 'cli/exe', 'cli/test']) {
      await cp(resolve(cwd, '..', path), join(context, path), { recursive: true });
    }
  }
  execFileSync(engine, [...args, context], { stdio: 'inherit' });
  const config = getDefaultBuildConfig();
  config.oci = { image: (await readFile(join(context, 'image-id'), 'utf8')).trim(), pullPolicy: 'never', runtime: engine };
  await buildAssets(config, { outputDir });
} finally { await rm(context, { recursive: true, force: true }); }
