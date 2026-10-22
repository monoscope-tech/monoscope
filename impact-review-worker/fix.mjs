import { constants } from 'node:fs';
import { open } from 'node:fs/promises';
import { createHash } from 'node:crypto';
import { join, posix } from 'node:path';
import { fileURLToPath } from 'node:url';
import { z } from 'zod';
import { createEditToolDefinition, createWriteToolDefinition } from '@earendil-works/pi-coding-agent';
import { agentCommand, reviewRequest, runRepositoryAgent } from './review.mjs';

const fixRequest = reviewRequest.omit({ head: true }).extend({ runId: z.uuid(), issueId: z.uuid() });
const digest = (text) => createHash('sha256').update(text).digest('hex');
export function fixIssue(input, options) {
  const issue = fixRequest.parse(input);
  z.uuid().parse(options.attemptId);
  if (!options.artifactDir || typeof options.bindGuest !== 'function' || typeof options.restartFix !== 'function' || typeof options.completeFix !== 'function' || typeof options.optionsTool !== 'function' || typeof options.onEvent !== 'function') throw new Error('Issue fixes require host workflow tools, private artifacts and metering');
  return runRepositoryAgent({ ...issue, head: issue.base }, {
    ...options,
    async createWorkflow({ vm, cwd, root, workspace, checkRegression, signal }) {
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
      options.bindGuest(async ({ tag, contents }, toolSignal) => {
        const active = operationSignal(toolSignal);
        try {
          active.throwIfAborted();
          if (tag === 'RunCheck') {
            const result = await checkRegression(contents, active);
            active.throwIfAborted();
            return result;
          }
          if (tag === 'CopyGuest') {
            const file = await open(contents.source, constants.O_RDONLY | constants.O_NOFOLLOW | constants.O_NONBLOCK);
            try {
              if (!(await file.stat()).isFile()) throw new Error('Invalid private artifact');
              await vm.fs.writeFile(contents.destination, file.createReadStream({ autoClose: false }), { signal: active });
            } finally { await file.close(); }
            active.throwIfAborted();
            return [];
          }
          const command = tag === 'ReadGuest' ? { arguments: ['/usr/bin/head', '-c', String(contents.maxBytes), '--', guestPath(contents.path)], maxBytes: contents.maxBytes, identity: 'agent' } : contents;
          const execution = vm.exec(command.identity === 'supervisor' ? command.arguments : agentCommand(command.arguments), { cwd, signal: active, stdout: 'pipe', stderr: 'pipe' });
          void execution.catch(() => {});
          const chunks = [];
          let length = 0;
          for await (const chunk of execution.output()) {
            length += chunk.data.length;
            if (length > command.maxBytes) { await vm.close(); throw new Error('Guest output limit exceeded'); }
            chunks.push(chunk.data);
          }
          active.throwIfAborted();
          if (!(await execution).ok) throw new Error('Guest workspace command failed');
          return Buffer.concat(chunks).toString('utf8');
        } catch (error) {
          if (active.aborted) await vm.close();
          throw error;
        }
      });
      await options.restartFix(workspace ? { tag: 'PreparedCheckpoint', contents: { path: workspace.path, sha256: workspace.manifest.sha256 } } : { tag: 'Pristine' }, signal);
      signal.throwIfAborted();
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
