import { createHash } from 'node:crypto';
import { join, posix } from 'node:path';
import { z } from 'zod';
import { createEditToolDefinition, createWriteToolDefinition } from '@earendil-works/pi-coding-agent';
import { agentCommand, guestPath, reviewRequest, runRepositoryAgent } from './review.mjs';

const fixRequest = reviewRequest.omit({ head: true }).extend({ runId: z.uuid(), issueId: z.uuid() });
const digest = (text) => createHash('sha256').update(text).digest('hex');
export function fixIssue(input, options) {
  const issue = fixRequest.parse(input);
  z.uuid().parse(options.attemptId);
  if (!options.artifactDir || typeof options.bindGuest !== 'function' || typeof options.restartFix !== 'function' || typeof options.completeFix !== 'function' || typeof options.optionsTool !== 'function' || typeof options.onEvent !== 'function') throw new Error('Issue fixes require host workflow tools, private artifacts and metering');
  return runRepositoryAgent({ ...issue, head: issue.base }, {
    ...options,
    async createWorkflow({ vm, cwd, root, workspace, signal }) {
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
      await options.restartFix(workspace ? { tag: 'PreparedCheckpoint', contents: { path: workspace.path, sha256: workspace.manifest.sha256 } } : { tag: 'Pristine' }, signal);
      signal.throwIfAborted();
      return {
        name: 'submit_fix', label: 'Submit verified fix',
        tools: [
          ...[createEditToolDefinition, createWriteToolDefinition].map((defineTool) => ({
            ...defineTool(cwd),
            async execute(id, args, toolSignal, onUpdate, context) {
              const active = operationSignal(toolSignal);
              const path = guestPath(args.path, cwd);
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
        ],
        async prepare({ summary }, toolSignal) {
          const active = operationSignal(toolSignal);
          active.throwIfAborted();
          const reply = await options.completeFix(summary, active);
          active.throwIfAborted();
          return reply;
        },
      };
    },
  });
}
