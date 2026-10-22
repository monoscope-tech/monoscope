import { createHash, randomUUID } from 'node:crypto';
import { mkdir, open } from 'node:fs/promises';
import { join, posix } from 'node:path';
import { fileURLToPath } from 'node:url';
import writeFileAtomic from 'write-file-atomic';
import { z } from 'zod';
import { createEditToolDefinition, createWriteToolDefinition } from '@earendil-works/pi-coding-agent';
import { agentCommand, lockWorkspace, reviewRequest, runRepositoryAgent } from './review.mjs';

const fixText = (max) => z.string().min(1).max(max).refine((value) => value.trim().length > 0 && !/[\x00-\x08\x0b-\x1f\x7f]/.test(value));
const fixRequest = reviewRequest.omit({ head: true }).extend({ runId: z.uuid(), issueId: z.uuid() });
const digest = (text) => createHash('sha256').update(text).digest('hex');
const reply = (text, isError = false) => ({ content: [{ type: 'text', text }], details: {}, isError });
const syncDirectory = async (path) => {
  const handle = await open(path, 'r');
  try { await handle.sync(); } finally { await handle.close(); }
};

export function fixIssue(input, options) {
  const issue = fixRequest.parse(input);
  const attemptId = z.uuid().parse(options.attemptId);
  if (!options.artifactDir || typeof options.admitTest !== 'function' || typeof options.recordStep !== 'function' || typeof options.validateFix !== 'function' || typeof options.onEvent !== 'function') throw new Error('Issue fixes require private artifacts, host admission, phase/result acknowledgements, and metering');
  return runRepositoryAgent({ ...issue, head: issue.base }, {
    ...options,
    async createWorkflow({ vm, cwd, request, root, artifactDir, executeRegression, signal }) {
      const operationSignal = (toolSignal) => AbortSignal.any([signal, toolSignal].filter(Boolean));
      const checked = async (command, toolSignal, stdin) => {
        const active = operationSignal(toolSignal);
        try {
          const result = await vm.exec(command, { cwd, signal: active, stdin });
          active.throwIfAborted();
          if (!result.ok) throw new Error(result.stderr.trim() || 'Fix workspace operation failed');
          return result;
        } catch (error) {
          if (active.aborted) await vm.close();
          throw error;
        }
      };
      const guestPath = (input) => {
        const path = posix.resolve(cwd, (input.startsWith('file://') ? fileURLToPath(input) : input.replace(/^@/, '')).replace(/^~(?=\/|$)/, '/home/review-agent'));
        return z.string().refine((value) => !value.includes('\0'), 'File paths cannot contain NUL bytes').parse(path);
      };
      await lockWorkspace(vm, cwd, signal);
      let reproduction, candidate, verified;
      const capturePatch = async (toolSignal) => {
        const active = operationSignal(toolSignal);
        active.throwIfAborted();
        await checked(agentCommand(['/usr/bin/git', 'add', '-A']), active);
        const execution = vm.exec(agentCommand(['/usr/bin/git', 'diff', '--cached', '--binary', '--no-ext-diff', '--no-textconv', '--no-color', '--no-relative', '--src-prefix=a/', '--dst-prefix=b/', request.base, '--']), { cwd, signal: active, stdout: 'pipe', stderr: 'pipe' });
        void execution.catch(() => {});
        const chunks = [];
        let length = 0;
        for await (const chunk of execution.output()) {
          length += chunk.data.length;
          if (length > 1024 * 1024) { await vm.close(); throw new Error('Candidate patch exceeds its 1 MiB limit'); }
          chunks.push(chunk.data);
        }
        if (!(await execution).ok) throw new Error('Candidate patch could not be captured');
        return Buffer.concat(chunks).toString('utf8');
      };
      const persist = async (parent, id, files) => {
        const collection = join(artifactDir, parent);
        const directory = join(collection, id);
        await mkdir(directory, { recursive: true, mode: 0o700 });
        await syncDirectory(artifactDir);
        await syncDirectory(collection);
        for (const [name, contents] of Object.entries(files)) writeFileAtomic.sync(join(directory, name), contents, { mode: 0o600 });
        await syncDirectory(directory);
        return directory;
      };
      const recordStep = async (step, toolSignal) => {
        const active = operationSignal(toolSignal);
        active.throwIfAborted();
        const acknowledgement = await options.recordStep(step, active);
        active.throwIfAborted();
        if (!acknowledgement || !Object.hasOwn(acknowledgement, 'Right')) throw new Error('Fix phase was not acknowledged by the host');
      };
      const recordExecution = (toolSignal) => (receipt) => recordStep({ tag: 'Observe', contents: receipt }, toolSignal);
      await recordStep({ tag: 'RestartReproduction' }, signal);
      return {
        name: 'submit_fix', label: 'Submit verified fix',
        tools: [
          ...[createEditToolDefinition, createWriteToolDefinition].map((defineTool) => ({
            ...defineTool(cwd),
            async execute(id, args, toolSignal, onUpdate, context) {
              const active = operationSignal(toolSignal);
              const path = guestPath(args.path);
              // Pi queues mutations via host realpath; only give that queue a private surrogate.
              const queuePath = join(root, 'file-tools', digest(path));
              const restore = (text) => text.replaceAll(queuePath, path);
              const run = (command, input) => checked(agentCommand(command), active, input);
              try {
                const result = await defineTool(cwd, { operations: {
                  access: async () => { await run(['/bin/sh', '-c', 'test -r "$1" && test -w "$1"', 'monoscope-edit', path]); },
                  readFile: async () => (await run(['/bin/cat', '--', path])).stdoutBuffer,
                  writeFile: async (_path, content) => { await run(['/bin/sh', '-c', '/bin/cat > "$1"', 'monoscope-write', path], content); },
                  mkdir: async () => { await run(['/bin/mkdir', '-p', '--', posix.dirname(path)]); },
                } }).execute(id, { ...args, path: queuePath }, active, onUpdate, context);
                return { ...result, content: result.content.map((item) => item.type === 'text' ? { ...item, text: restore(item.text) } : item),
                  details: { ...result.details, ...(result.details?.patch ? { patch: restore(result.details.patch) } : {}) } };
              } catch (error) {
                active.throwIfAborted();
                throw new Error(restore(error.message));
              }
            },
          })),
          {
            name: 'reproduce_issue', label: 'Admit and reproduce regression',
            async execute(_id, input, toolSignal) {
              const active = operationSignal(toolSignal);
              active.throwIfAborted();
              if (reproduction) return reply('A regression is already frozen for this run', true);
              const artifact = { script: input.script, failureMessage: input.failureMessage, testPatch: input.testPatch };
              if (Object.hasOwn(input, 'testPatchFile')) {
                const patch = await checked(agentCommand(['/usr/bin/head', '-c', '262145', '--', guestPath(input.testPatchFile)]), active);
                if (patch.stdoutBuffer.length > 262144) return reply('Regression patch file exceeds its 256 KiB limit', true);
                artifact.testPatch = patch.stdout;
              }
              artifact.testPatch = z.string().min(1).max(262144).parse(artifact.testPatch);
              const admission = await options.admitTest({ ...artifact, projectId: issue.projectId, issueId: issue.issueId, base: issue.base }, active);
              active.throwIfAborted();
              if (Object.hasOwn(admission, 'Left')) return reply(admission.Left, true);
              if (typeof admission.Right !== 'string' || !admission.Right.trim()) return reply('Host admission requires a recorded rationale', true);
              await persist('admissions', randomUUID(), { 'admission.json': JSON.stringify({ projectId: issue.projectId, runId: issue.runId, issueId: issue.issueId, attemptId, base: issue.base, regressionSha256: digest(JSON.stringify({ script: artifact.script, testPatch: artifact.testPatch, failureMessage: artifact.failureMessage })), rationale: admission.Right }) + '\n' });
              const result = await executeRegression(artifact, toolSignal, { revisions: [['base', request.base]], recordExecution: recordExecution(active) });
              if (result.isError) return result;
              const report = JSON.parse(result.content[0].text);
              if (report.results[0].status !== 'failed') return reply('The admitted regression did not fail on base; source edits remain locked', true);
              active.throwIfAborted();
              await vm.fs.writeFile('/run/monoscope-admitted-test.patch', artifact.testPatch, { signal: active });
              if (report.results[0].testPatchDisposition !== 'retained') await checked(['/usr/bin/git', '-c', 'core.hooksPath=/dev/null', 'apply', '--index', '/run/monoscope-admitted-test.patch'], active);
              await checked(['/bin/chown', '-hR', '1000:1000', cwd], active);
              reproduction = artifact;
              return reply('Regression reproduced and frozen. Source editing is now available.\n' + JSON.stringify(report));
            },
          },
          {
            name: 'propose_fix', label: 'Capture source fix',
            async execute(_id, { rootCause }, toolSignal) {
              const active = operationSignal(toolSignal);
              active.throwIfAborted();
              if (!reproduction) return reply('Reproduce the issue before proposing a fix', true);
              rootCause = fixText(240).parse(rootCause);
              verified = undefined;
              const patch = await capturePatch(active);
              const response = await options.proposeCandidate({ patch, rootCause });
              active.throwIfAborted();
              if (Object.hasOwn(response, 'Left')) return reply(response.Left, true);
              const captured = response.Right;
              const revision = captured.head;
              await recordStep({ tag: 'ProposeCandidate', contents: { revision, rootCause } }, active);
              request.head = revision;
              candidate = captured;
              return reply(JSON.stringify(captured));
            },
          },
          {
            name: 'verify_fix', label: 'Verify captured source fix',
            async execute(_id, _params, toolSignal) {
              const active = operationSignal(toolSignal);
              active.throwIfAborted();
              if (!candidate || !reproduction) return reply('Capture a candidate after reproduction first', true);
              verified = undefined;
              if (digest(await capturePatch(active)) !== candidate.patchSha256) return reply('Workspace changed; propose and verify a new candidate', true);
              const result = await executeRegression(reproduction, active, { recordExecution: recordExecution(active) });
              active.throwIfAborted();
              if (result.isError) return result;
              const report = JSON.parse(result.content[0].text);
              if (report.results[0].status !== 'failed' || report.results[1].status !== 'passed' || report.results[1].testPatchDisposition !== 'retained') return reply('Candidate verification did not establish a failing base and passing retained test.\n' + JSON.stringify(report), true);
              verified = { ...candidate, checkId: report.id, regressionSha256: report.regressionSha256 };
              return reply('Frozen regression fails on base and passes on the candidate with its test retained.\n' + JSON.stringify(report));
            },
          },
        ],
        async prepare({ summary }, toolSignal) {
          const active = operationSignal(toolSignal);
          active.throwIfAborted();
          if (!verified) return { Left: 'Capture and verify a source fix before submission' };
          if (digest(await capturePatch(active)) !== verified.patchSha256) return { Left: 'Workspace changed after verification; propose and verify a new candidate' };
          const result = { ...verified, summary };
          const acceptance = await options.validateFix(result, active);
          active.throwIfAborted();
          return Object.hasOwn(acceptance, 'Right') ? { Right: result } : { Left: acceptance.Left ?? 'Host did not accept this fix' };
        },
      };
    },
  });
}
