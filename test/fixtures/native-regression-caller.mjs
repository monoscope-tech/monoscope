// SDK interop only; Haskell owns setup, cancellation timing and assertions.
import { readFile } from 'node:fs/promises';
import { createRequire } from 'node:module';
import { runRegression } from '../../impact-review-worker/workspace.mjs';

const { importImageFromDirectory } = await import(createRequire(new URL('../../impact-review-worker/package.json', import.meta.url)).resolve('@earendil-works/gondolin'));
const [input, cli, profile] = process.argv.slice(2);
process.env.IMPACT_REVIEW_NATIVE_CLI = cli;
if (process.platform === 'darwin') process.env.GONDOLIN_VMM = 'krun';
const { request, options } = JSON.parse(await readFile(input, 'utf8'));
const controller = new AbortController();
process.stdin.resume();
process.stdin.once('end', () => controller.abort());
try {
  await runRegression(request, importImageFromDirectory(profile), { ...options, signal: controller.signal });
} catch (error) {
  if (!controller.signal.aborted) throw error;
  console.log(JSON.stringify({ name: error.name }));
} finally { process.stdin.destroy(); }
