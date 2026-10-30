import { z } from 'zod';
import { reviewRequest, runRepositoryAgent } from './review.mjs';

const fixRequest = reviewRequest.omit({ head: true }).extend({ runId: z.uuid(), issueId: z.uuid() });
export function fixIssue(input, options) {
  const issue = fixRequest.parse(input);
  z.uuid().parse(options.attemptId);
  if (!options.artifactDir || typeof options.bindGuest !== 'function' || typeof options.restartFix !== 'function' || typeof options.completeFix !== 'function' || typeof options.optionsTool !== 'function' || typeof options.onEvent !== 'function') throw new Error('Issue fixes require host workflow tools, private artifacts and metering');
  return runRepositoryAgent({ ...issue, head: issue.base }, {
    ...options,
    async createWorkflow({ workspace, signal }) {
      await options.restartFix(workspace ? { tag: 'PreparedCheckpoint', contents: { path: workspace.path, sha256: workspace.manifest.sha256 } } : { tag: 'Pristine' }, signal);
      signal.throwIfAborted();
      return {
        name: 'submit_fix', label: 'Submit verified fix',
        async prepare({ summary }, toolSignal) {
          const active = AbortSignal.any([signal, toolSignal].filter(Boolean));
          active.throwIfAborted();
          const reply = await options.completeFix(summary, active);
          active.throwIfAborted();
          return reply;
        },
      };
    },
  });
}
