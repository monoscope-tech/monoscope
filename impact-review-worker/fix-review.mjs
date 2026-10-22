import { z } from 'zod';
import { lockWorkspace, reviewRequest, runRepositoryAgent } from './review.mjs';

export async function reviewVerifiedFix(input, options) {
  const { artifacts, imageBuildId, ...request } = input;
  reviewRequest.extend({ runId: z.uuid(), issueId: z.uuid() }).parse(request);
  const result = artifacts.result;
  if (request.projectId !== result.projectId || request.runId !== result.runId || request.issueId !== result.issueId || request.base !== result.base || request.repository !== artifacts.repository || imageBuildId !== artifacts.baseline.imageBuildId) throw new Error('Peer review requires the same verified issue candidate');
  if (typeof options.validateReview !== 'function' || typeof options.onEvent !== 'function') throw new Error('Peer review requires host acceptance and metering');
  const attemptId = z.uuid().parse(options.attemptId);
  if (attemptId === result.attemptId) throw new Error('Peer review requires a separately persisted attempt');
  const evidence = options.preparedCheckout?.evidence;
  if (evidence?.tag !== 'VerifiedEvidence') throw new Error('Peer review requires host-verified evidence');
  const { regression, baseLog, headLog } = evidence.contents;
  return runRepositoryAgent({ ...request, head: result.head, gitToken: '' }, {
    ...options, attemptId, expectedImageBuildId: imageBuildId,
    async createWorkflow({ vm, cwd, signal }) {
      await lockWorkspace(vm, cwd, signal);
      const files = {
        '/run/monoscope-fix-review.json': JSON.stringify({ result, regression, baseline: artifacts.baseline, candidate: artifacts.candidate }),
        '/run/monoscope-fix-review-base.log': Buffer.from(baseLog, 'base64'),
        '/run/monoscope-fix-review-head.log': Buffer.from(headLog, 'base64'),
      };
      for (const [path, bytes] of Object.entries(files)) {
        await vm.fs.writeFile(path, bytes, { signal });
        if (!(await vm.exec(['/bin/chmod', '0444', path], { signal })).ok) throw new Error('Peer review evidence could not be protected');
      }
      return {
        name: 'submit_fix_review', label: 'Submit independent candidate review',
        async prepare(verdict, activeSignal) {
          const active = AbortSignal.any([signal, activeSignal].filter(Boolean)); active.throwIfAborted();
          const response = { result, decision: verdict };
          const checked = await options.validateReview(response, active); active.throwIfAborted();
          return Object.hasOwn(checked, 'Right') ? { Right: response } : { Left: checked.Left ?? 'Peer review host did not accept this result' };
        },
      };
    },
  });
}
