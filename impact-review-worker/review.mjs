// Research adapters use the same native VM host and Pi tools as production.
import { execFile } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { isAbsolute, join } from 'node:path';
import { promisify } from 'node:util';
import { fileURLToPath } from 'node:url';
import { z } from 'zod';
import { importImageFromDirectory, resolveImageSelector } from '@earendil-works/gondolin';
import { hostTools, runPi } from './pi-host.mjs';
import { withNativeSession } from './workspace.mjs';
export { modelUsage } from './pi-host.mjs';

const exec = promisify(execFile);
const sha = z.string().regex(/^[a-f0-9]{40}(?:[a-f0-9]{24})?$/);
const reviewRequest = z.object({
  projectId: z.uuid(), repository: z.string().min(1), base: sha, head: sha,
  gitToken: z.string(), prompt: z.string(), model: z.string().min(1),
  provider: z.string().min(1), thinkingLevel: z.enum(['off', 'minimal', 'low', 'medium', 'high', 'xhigh']),
});
export const review = (input, options) => runRepositoryAgent(input, options);

export async function runRepositoryAgent(input, { modelRuntime, guestImage, workspaceCache, artifactDir, preparedCheckout, attemptId = randomUUID(), skillPaths = [], validateDraft, readOnly = false, signal, onEvent = () => {} }) {
  const request = reviewRequest.parse(input);
  if (!preparedCheckout) throw new Error('A host-prepared checkout is required');
  const checkout = preparedCheckout.directory;
  if (!isAbsolute(checkout) || !isAbsolute(preparedCheckout.repositorySkills ?? '') || typeof preparedCheckout.rootInstructions !== 'string' || request.gitToken || preparedCheckout.projectId !== request.projectId || preparedCheckout.repository !== request.repository || preparedCheckout.base !== request.base || preparedCheckout.head !== request.head) throw new Error('Host checkout identity mismatch');
  const gitEnv = { PATH: process.env.PATH, GIT_ALLOW_PROTOCOL: 'file', GIT_TERMINAL_PROMPT: '0', GIT_CONFIG_NOSYSTEM: '1', GIT_CONFIG_GLOBAL: '/dev/null', GIT_CONFIG_COUNT: '2', GIT_CONFIG_KEY_0: 'core.hooksPath', GIT_CONFIG_VALUE_0: '/dev/null', GIT_CONFIG_KEY_1: 'core.fsmonitor', GIT_CONFIG_VALUE_1: 'false' };
  if ((await exec('git', ['rev-parse', 'HEAD'], { cwd: checkout, env: gitEnv, signal, timeout: 60000 })).stdout.trim() !== request.head) throw new Error('Host checkout revision mismatch');
  onEvent({ type: 'checkout_prepared', cacheStatus: preparedCheckout.cacheStatus, durationMs: preparedCheckout.durationMs });
  const selected = resolveImageSelector(guestImage);
  const image = selected.source === 'path' ? importImageFromDirectory(selected.assetDir) : selected;
  preparedCheckout = { ...preparedCheckout, policy: { ...preparedCheckout.policy, tools: preparedCheckout.policy.tools.filter((tool) => tool.name === 'run_regression' || tool.name.startsWith('submit_')) } };
  const root = await mkdtemp(join(tmpdir(), 'monoscope-review-'));
  try {
    const started = performance.now();
    return await withNativeSession({
      host: { image: image.assetDir, bridge: fileURLToPath(new URL('./sandbox.mjs', import.meta.url)), skills: [...skillPaths, fileURLToPath(new URL('./skills/superpowers', import.meta.url))], workspace: workspaceCache ? { dependencyArchive: workspaceCache.dependencyArchive ?? null } : null },
      expectedImageBuildId: image.buildId, checkout, base: request.base, head: request.head, readOnly,
      lease: workspaceCache ? { cacheDir: workspaceCache.cacheDir, request: { projectId: request.projectId, repository: request.repository, base: request.base, expectedImageBuildId: image.buildId } } : null,
      projectId: request.projectId, runId: z.uuid().parse(input.runId), attemptId, artifactDir,
    }, { signal, script: workspaceCache?.script, onEvent }, async (ready, rpc) => {
      const manifest = ready.prepared?.prepared.manifest;
      onEvent({ type: 'workspace_cache', status: manifest ? 'hit' : workspaceCache ? 'miss' : 'disabled', sha256: manifest?.sha256 ?? null, checkpointBytes: manifest?.bytes ?? 0, durationMs: performance.now() - started });
      const optionsTool = async (invocation) => {
        if (invocation.name !== 'run_regression') return { Left: 'This research runner has no production query capability' };
        const result = await rpc('Check', invocation.arguments);
        for (const receipt of result.receipts) onEvent({ type: 'regression_execution', receipt });
        return result.isError ? { Left: JSON.stringify(result.report) } : { Right: result.report };
      };
      return runPi({
        preparedCheckout, request, modelRuntime, attemptId, signal, root, skillsDir: ready.skills, onEvent,
        tools: hostTools(preparedCheckout, root, async (operation) => {
          try { return { Right: await rpc('Operate', operation) }; } catch (error) { return { Left: error.message }; }
        }),
        optionsTool,
        workflow: {
          name: preparedCheckout.policy.tools.find((tool) => tool.name.startsWith('submit_'))?.name,
          async prepare(draft) {
            const checked = validateDraft ? await validateDraft(draft) : { Right: [] };
            return Object.hasOwn(checked, 'Left') ? checked : { Right: draft };
          },
        },
      });
    });
  } finally { await rm(root, { recursive: true, force: true }); }
}
