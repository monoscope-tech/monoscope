import { z } from 'zod';
import { lockWorkspace, reviewRequest, runRepositoryAgent } from './review.mjs';

const testArtifact = z.object({ script: z.string().min(1).max(65536).refine((text) => text.trim().length > 0), testPatch: z.string().min(1).max(262144), failureMessage: z.string().min(1).max(200).refine((text) => text.trim().length > 0) });

export function admitRegression(input, options) {
  const { artifact, ...request } = reviewRequest.extend({ runId: z.uuid(), issueId: z.uuid(), artifact: testArtifact }).parse(input);
  const attemptId = z.uuid().parse(options.attemptId);
  if (typeof options.validateAdmission !== 'function' || typeof options.onEvent !== 'function') throw new Error('Admission requires host acceptance and metering');
  return runRepositoryAgent({ ...request, head: request.base }, {
    ...options, attemptId,
    async createWorkflow({ vm, cwd, signal }) {
      await lockWorkspace(vm, cwd, signal);
      await vm.fs.writeFile('/run/monoscope-admission.patch', artifact.testPatch, { signal });
      const stats = await vm.exec(['/usr/bin/git', 'apply', '--numstat', '-z', '/run/monoscope-admission.patch'], { cwd, signal });
      const paths = stats.stdout.split('\0').filter(Boolean);
      if (!stats.ok || !paths.length || paths.length > 100) throw new Error('Regression patch requires between one and 100 paths');
      const applied = await vm.exec(['/usr/bin/git', '-c', 'core.hooksPath=/dev/null', 'apply', '--index', '/run/monoscope-admission.patch'], { cwd, signal });
      if (!applied.ok) throw new Error('Regression patch does not apply to its pinned base');
      await vm.fs.writeFile('/run/monoscope-admission.json', JSON.stringify(artifact), { signal });
      const protectedFiles = await vm.exec(['/bin/chmod', '0444', '/run/monoscope-admission.patch', '/run/monoscope-admission.json'], { signal });
      if (!protectedFiles.ok) throw new Error('Regression artifact could not be protected');
      await lockWorkspace(vm, cwd, signal);
      return {
        name: 'submit_admission', label: 'Submit regression admission',
        async prepare(verdict, toolSignal) {
          const active = AbortSignal.any([signal, toolSignal].filter(Boolean));
          active.throwIfAborted();
          const result = { artifact, decision: verdict };
          const accepted = await options.validateAdmission(result, active);
          active.throwIfAborted();
          return Object.hasOwn(accepted, 'Right') ? { Right: result } : { Left: accepted.Left ?? 'Admission host did not accept this result' };
        },
      };
    },
  });
}
