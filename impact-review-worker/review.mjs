import { execFile } from 'node:child_process';
import { createHash, randomUUID } from 'node:crypto';
import { cp, mkdtemp, mkdir, open, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { basename, dirname, isAbsolute, join, posix, relative } from 'node:path';
import { constants, createReadStream } from 'node:fs';
import { RealFSProvider, ReadonlyProvider, VM, importImageFromDirectory, resolveImageSelector } from '@earendil-works/gondolin';
import { promisify } from 'node:util';
import { fileURLToPath } from 'node:url';
import {
  createAgentSession, createBashToolDefinition, createReadToolDefinition, createEditToolDefinition, createWriteToolDefinition,
  DefaultResourceLoader, loadSkills, SessionManager, SettingsManager,
} from '@earendil-works/pi-coding-agent';
import { z } from 'zod';
import { Type } from 'typebox';
import { agentCommand, guestTmpfs, loadPreparedWorkspace, runRegression, runRegressionFile } from './workspace.mjs';
export { agentCommand, guestTmpfs } from './workspace.mjs';

const exec = promisify(execFile);
const sha = z.string().regex(/^[a-f0-9]{40}(?:[a-f0-9]{24})?$/);
export const reviewRequest = z.object({
  projectId: z.uuid(), repository: z.string().min(1), base: sha, head: sha,
  gitToken: z.string(), prompt: z.string(), model: z.string().min(1),
  provider: z.string().min(1), thinkingLevel: z.enum(['off', 'minimal', 'low', 'medium', 'high', 'xhigh']),
});


export const guestPath = (input, cwd) => {
  const path = posix.resolve(cwd, (input.startsWith('file://') ? fileURLToPath(input) : input.replace(/^@/, '')).replace(/^~(?=\/|$)/, '/home/review-agent'));
  return z.string().refine((value) => !value.includes('\0'), 'File paths cannot contain NUL bytes').parse(path);
};

async function lockWorkspace(vm, cwd, signal) {
  for (const command of [
    ['/usr/bin/git', 'config', '--system', '--add', 'safe.directory', cwd],
    ['/bin/chown', '-hR', '0:0', cwd],
    ['/bin/chmod', '-R', 'u+rwX,go+rX,go-w', cwd],
    ['/usr/bin/git', 'update-index', '--refresh'],
  ]) if (!(await vm.exec(command, { cwd, signal })).ok) throw new Error('Workspace could not be protected');
}

export function modelUsage(message, pricing, attemptId, callId) {
  const usage = message.usage;
  const priced = pricing && [pricing.input, pricing.output, pricing.cacheRead, pricing.cacheWrite].some((rate) => rate > 0);
  const amount = Math.round(usage.cost.total * 1e9);
  return {
    id: randomUUID(), attemptId, ...(callId ? { callId } : {}), provider: message.provider, model: message.responseModel ?? message.model,
    responseId: message.responseId ?? null,
    inputTokens: usage.input, outputTokens: usage.output, cacheReadTokens: usage.cacheRead, cacheWriteTokens: usage.cacheWrite,
    costNanousd: priced && usage.totalTokens > 0 && Number.isSafeInteger(amount) && amount >= 0 ? amount : null,
    pricing, succeeded: !['error', 'aborted'].includes(message.stopReason),
  };
}

// Adapted from Gondolin's Pi example: model credentials remain outside repository tools.
export const review = (input, options) => runRepositoryAgent(input, options);

export async function runRepositoryAgent(input, { modelRuntime, guestImage, expectedImageBuildId, workspaceCache, artifactDir, preparedCheckout, attemptId = randomUUID(), skillPaths = [], validateDraft, createWorkflow, bindGuest, initializeAdmission, optionsTool, signal, onEvent = () => {} }) {
  const request = reviewRequest.parse(input);
  const runId = artifactDir ? z.uuid().parse(input.runId) : undefined;
  const resolvedImage = resolveImageSelector(guestImage);
  const pinnedImage = resolvedImage.source === 'path' ? importImageFromDirectory(resolvedImage.assetDir) : resolvedImage;
  if (expectedImageBuildId !== undefined && pinnedImage.buildId !== expectedImageBuildId) throw new Error('Configured guest image differs from the pinned issue-fix image');
  if (!preparedCheckout) throw new Error('A host-prepared checkout is required');
  const admission = initializeAdmission ? input.artifact : undefined;
  const peer = preparedCheckout.evidence.tag === 'VerifiedEvidence' ? preparedCheckout.evidence.contents : undefined;
  if (peer && typeof validateDraft !== 'function') throw new Error('Peer review requires host acceptance');
  const root = await mkdtemp(join(tmpdir(), 'monoscope-review-'));
  const checkout = preparedCheckout.directory;
  const cwd = '/workspace';
  const skillsDir = join(root, 'skills');
  const outputFiles = new Set();
  const modelCalls = new Set();
  const meteredRuntime = new Proxy(modelRuntime, {
    get(target, property) {
      if (property === 'streamSimple') return (...args) => {
        const call = { id: randomUUID(), attemptId, provider: args[0].provider, model: args[0].id };
        // The host acknowledges the durable start before the provider stream exists.
        const stream = Promise.resolve().then(() => onEvent({ type: 'model_started', receipt: call })).then(() => target.streamSimple(...args));
        const completion = stream.then((value) => value.result()).then((message) => { onEvent({ type: 'model_response', message, callId: call.id }); return message; });
        modelCalls.add(completion);
        void completion.then(() => modelCalls.delete(completion), () => modelCalls.delete(completion));
        return { async *[Symbol.asyncIterator]() { yield* await stream; }, result: () => completion };
      };
      const value = Reflect.get(target, property, target);
      return typeof value === 'function' ? value.bind(target) : value;
    },
  });
  let session, vm, closing, submitted, allocationStartedAt;
  const verifierSession = randomUUID();
  const close = () => closing ??= vm?.close();
  const abort = () => { void session?.abort(); void close()?.catch(() => {}); };
  try {
    const gitEnv = { PATH: process.env.PATH, GIT_ALLOW_PROTOCOL: 'file', GIT_TERMINAL_PROMPT: '0', GIT_CONFIG_NOSYSTEM: '1', GIT_CONFIG_GLOBAL: '/dev/null',
      GIT_CONFIG_COUNT: '2', GIT_CONFIG_KEY_0: 'core.hooksPath', GIT_CONFIG_VALUE_0: '/dev/null', GIT_CONFIG_KEY_1: 'core.fsmonitor', GIT_CONFIG_VALUE_1: 'false' };
    const git = (args, toolSignal) => exec('git', args, { cwd: checkout, env: gitEnv, signal: AbortSignal.any([signal, toolSignal].filter(Boolean)), timeout: 60000 });
    if (!isAbsolute(checkout) || !isAbsolute(preparedCheckout.repositorySkills ?? '') || typeof preparedCheckout.rootInstructions !== 'string' || request.gitToken || preparedCheckout.projectId !== request.projectId || preparedCheckout.repository !== request.repository || preparedCheckout.base !== request.base || preparedCheckout.head !== request.head) throw new Error('Host checkout identity mismatch');
    if ((await git(['rev-parse', 'HEAD'])).stdout.trim() !== request.head) throw new Error('Host checkout revision mismatch');
    onEvent({ type: 'checkout_prepared', cacheStatus: preparedCheckout.cacheStatus, durationMs: preparedCheckout.durationMs });
    await mkdir(skillsDir);
    await Promise.all([...skillPaths, fileURLToPath(new URL('./skills/superpowers', import.meta.url))].map((path, index) => cp(path, join(skillsDir, String(index)), { recursive: true, dereference: true })));
    const repositorySkills = preparedCheckout.repositorySkills;
    const skillRoots = ['.agents/skills', '.pi/skills'];
    const loaded = loadSkills({ cwd: root, agentDir: join(root, 'agent'), skillPaths: [skillsDir, ...skillRoots.map((path) => join(repositorySkills, path))], includeDefaults: false });
    const skills = loaded.skills.map((skill) => {
      const filePath = skill.filePath.startsWith(repositorySkills + '/')
        ? cwd + '/' + relative(repositorySkills, skill.filePath)
        : '/skills/' + relative(skillsDir, skill.filePath);
      return { ...skill, filePath, baseDir: dirname(filePath) };
    });
    const workspaceStarted = performance.now();
    const workspace = workspaceCache ? await loadPreparedWorkspace(request, pinnedImage, workspaceCache) : undefined;
    onEvent({ type: 'workspace_cache', status: workspace ? 'hit' : workspaceCache ? 'miss' : 'disabled', sha256: workspace?.manifest.sha256 ?? null, checkpointBytes: workspace?.manifest.bytes ?? 0, durationMs: performance.now() - workspaceStarted });
    if (workspaceCache?.dependencyArchive && !workspace) throw new Error('Configured dependency workspace has not been prepared for these pinned inputs');
    const checkoutCommands = (revision) => {
      const git = ['/usr/bin/git', '-c', `safe.directory=${cwd}`, '-c', 'core.hooksPath=/dev/null', '-C', cwd];
      return [
        ...(workspace ? [cwd, '/checkout/.git'].map((path) => ['/usr/bin/git', 'config', '--system', '--add', 'safe.directory', path]) : []),
        ...(workspace ? [[...git, 'fetch', '--quiet', '--no-tags', '--', '/checkout', revision]] : [['/bin/mkdir', '-p', cwd], ['/bin/cp', '-R', '/checkout/.', cwd]]),
        [...git, 'checkout', '--quiet', '--detach', revision],
      ];
    };
    allocationStartedAt = performance.now();
    vm = workspace ? await workspace.resume({ '/checkout': new ReadonlyProvider(new RealFSProvider(checkout)), '/skills': new ReadonlyProvider(new RealFSProvider(skillsDir)) }, abort) : await VM.create({
      sessionLabel: 'monoscope-impact-review', memory: '1G', cpus: 2,
      sandbox: { imagePath: pinnedImage.assetDir, netEnabled: false }, rootfs: { mode: 'cow' }, tmpfs: guestTmpfs,
      vfs: { mounts: { '/checkout': new ReadonlyProvider(new RealFSProvider(checkout)), '/skills': new ReadonlyProvider(new RealFSProvider(skillsDir)) } },
    });
    signal?.addEventListener('abort', abort, { once: true });
    signal?.throwIfAborted();
    await vm.start();
    onEvent({ type: 'sandbox_started', id: vm.id, hostPid: vm.getHostPid() });
    signal?.throwIfAborted();
    // Guest-owned files keep Git's ownership checks and all review writes inside the VM.
    for (const command of checkoutCommands(request.head)) {
      if (!(await vm.exec(command, { signal })).ok) throw new Error('Repository could not be prepared in the guest');
    }
    const owned = await vm.exec(['/bin/chown', '-hR', '1000:1000', cwd], { signal });
    if (!owned.ok) throw new Error('Workspace ownership could not be assigned');
    await vm.fs.mkdir('/run/monoscope-review-output', { recursive: true, mode: 0o755 });
    const identity = await vm.exec(agentCommand(['/usr/bin/id', '-u']), { signal });
    if (!identity.ok || identity.stdout.trim() !== '1000') throw new Error('Guest image requires the review-agent account and setpriv');
    if (peer) {
      await lockWorkspace(vm, cwd, signal);
      for (const [path, bytes] of Object.entries(peer.files)) {
        await vm.fs.writeFile(path, Buffer.from(bytes, 'base64'), { signal });
        if (!(await vm.exec(['/bin/chmod', '0444', path], { signal })).ok) throw new Error('Host evidence could not be protected');
      }
    }
    bindGuest?.(async ({ tag, contents }, toolSignal) => {
      const active = AbortSignal.any([signal, toolSignal].filter(Boolean));
      try {
        active.throwIfAborted();
        if (tag === 'RunCheck') {
          const result = await runRegressionFile(contents, pinnedImage, { artifactDir, checkout, signal: active });
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
        const command = tag === 'ReadGuest' ? { arguments: ['/usr/bin/head', '-c', String(contents.maxBytes), '--', guestPath(contents.path, cwd)], maxBytes: contents.maxBytes, identity: 'agent' } : contents;
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
    if (initializeAdmission) {
      const reply = await initializeAdmission(signal);
      if (Object.hasOwn(reply, 'Left')) throw new Error('Admission workspace was rejected');
    }
    const bash = createBashToolDefinition(cwd, {
      operations: {
        async exec(command, cwd, options) {
          const signals = [signal, options.signal, options.timeout ? AbortSignal.timeout(options.timeout * 1000) : undefined].filter(Boolean);
          const commandSignal = AbortSignal.any(signals);
          try {
            const proc = vm.exec(agentCommand(['/bin/bash', '-lc', command]), { cwd, signal: commandSignal, stdout: 'pipe', stderr: 'pipe' });
            for await (const chunk of proc.output()) options.onData(chunk.data);
            return { exitCode: (await proc).exitCode };
          } catch (error) {
            // Gondolin aborts the host wait; closing also terminates any guest process.
            if (commandSignal.aborted) await close();
            throw error;
          }
        },
      },
      exposeSessionEnvironment: false,
    });
    const tools = [{
      ...bash,
      async execute(...args) {
        const onUpdate = args[3];
        args[3] = (update) => {
          if (update.details?.fullOutputPath) outputFiles.add(update.details.fullOutputPath);
          onUpdate?.(update);
        };
        const result = await bash.execute(...args);
        const path = result.details?.fullOutputPath;
        if (path) {
          outputFiles.add(path);
          // Pi advertises this path to both tools; preserve it inside the guest too.
          const guestPath = '/run/monoscope-review-output/' + basename(path);
          await vm.fs.writeFile(guestPath, createReadStream(path), { signal });
          result.details.fullOutputPath = guestPath;
          result.content = result.content.map((block) => block.type === 'text' ? { ...block, text: block.text.replaceAll(path, guestPath) } : block);
          await rm(path, { force: true });
          outputFiles.delete(path);
        }
        return result;
      },
    }, createReadToolDefinition(cwd, {
      operations: {
        access: (path) => vm.fs.access(path, { mode: constants.R_OK }),
        readFile: (path) => vm.fs.readFile(path),
      },
    })];
    if (preparedCheckout.policy.fileTools === 'modify_files') tools.push(...[createEditToolDefinition, createWriteToolDefinition].map((defineTool) => ({
      ...defineTool(cwd),
      executionMode: 'sequential',
      async execute(id, args, toolSignal, onUpdate, context) {
        const active = AbortSignal.any([signal, toolSignal].filter(Boolean));
        const path = guestPath(args.path, cwd);
        // Pi queues mutations via host realpath; only give that queue a private surrogate.
        const queuePath = join(root, 'file-tools', createHash('sha256').update(path).digest('hex'));
        const restore = (text) => text.replaceAll(queuePath, path);
        const run = async (command, stdin) => {
          try {
            const result = await vm.exec(agentCommand(command), { cwd, signal: active, stdin });
            active.throwIfAborted();
            if (!result.ok) throw new Error(result.stderr.trim() || 'Review workspace operation failed');
            return result;
          } catch (error) {
            if (active.aborted) await vm.close();
            throw error;
          }
        };
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
    })));
    const executeRegression = async (artifact, toolSignal) => {
      const active = AbortSignal.any([signal, toolSignal].filter(Boolean));
      const result = await runRegression({ projectId: request.projectId, runId, attemptId, sessionId: verifierSession, base: request.base, artifact, revisions: [['base', request.base], ['head', request.head]] }, pinnedImage,
        { artifactDir, checkout, workspace, signal: active });
      for (const receipt of result.receipts) {
        onEvent({ type: 'regression_execution', receipt });
      }
      for (const measured of result.report.results) {
        await vm.fs.writeFile(measured.logPath, createReadStream(join(artifactDir, result.report.id, measured.side + '.log')), { signal: active });
      }
      return { content: [{ type: 'text', text: JSON.stringify(result.report) }], details: {}, isError: result.isError };
    };
    if (artifactDir && !createWorkflow && preparedCheckout.policy.tools.some((tool) => tool.name === 'run_regression')) tools.push({
      name: 'run_regression', label: 'Replay a frozen regression check',
      execute: (_id, params, toolSignal) => executeRegression(params, toolSignal),
    });
    const workflow = createWorkflow ? await createWorkflow({ workspace, signal }) : {
      name: admission ? 'submit_admission' : peer ? 'submit_fix_review' : 'submit_review',
      prepare: async (draft, toolSignal) => {
        const active = AbortSignal.any([signal, toolSignal].filter(Boolean));
        active.throwIfAborted();
        const result = admission ? { artifact: admission, decision: draft } : peer ? { result: peer.result, decision: draft } : draft;
        const reply = validateDraft ? await validateDraft(result) : { Right: [] };
        active.throwIfAborted();
        return Object.hasOwn(reply, 'Left') ? reply : { Right: result };
      },
    };
    tools.push({
      name: workflow.name, label: workflow.label ?? 'Submit review draft',
      parameters: Type.Unsafe(workflow.schema ? workflow.schema.toJSONSchema() : preparedCheckout.policy.tools.find((tool) => tool.name === workflow.name).parameters),
      async execute(_id, params, toolSignal) {
        const draft = workflow.schema ? workflow.schema.parse(params) : params;
        const reply = await workflow.prepare(draft, toolSignal);
        const isError = Object.hasOwn(reply, 'Left');
        if (!isError) submitted = reply.Right;
        return { content: [{ type: 'text', text: isError ? reply.Left : 'Draft accepted. Finish without further tool calls.' }], details: {}, isError };
      },
    });
    if (optionsTool) for (const definition of preparedCheckout.policy.tools) {
      if (tools.some((tool) => tool.name === definition.name)) continue;
      tools.push({ ...definition, label: definition.name, parameters: Type.Unsafe(definition.parameters),
        async execute(_id, args, toolSignal) {
          const active = AbortSignal.any([signal, toolSignal].filter(Boolean));
          active.throwIfAborted();
          const response = await optionsTool({ name: definition.name, arguments: args }, active);
          active.throwIfAborted();
          const isError = Object.hasOwn(response, 'Left');
          return { content: [{ type: 'text', text: JSON.stringify(isError ? response.Left : response.Right) }], details: {}, isError };
        },
      });
    }
    for (const tool of tools) {
      const definition = preparedCheckout.policy.tools.find((item) => item.name === tool.name);
      if (definition && !workflow.instructions) Object.assign(tool, { label: definition.name, description: definition.description, parameters: Type.Unsafe(definition.parameters), ...(definition.sequential ? { executionMode: 'sequential' } : {}) });
    }
    if (createWorkflow || admission || peer) for (const tool of tools) tool.executionMode = 'sequential';
    const settingsManager = SettingsManager.inMemory({ retry: { enabled: true, maxRetries: 1 } });
    const loader = new DefaultResourceLoader({
      cwd: root, agentDir: join(root, 'agent'), settingsManager,
      noExtensions: true, noSkills: true, noPromptTemplates: true, noThemes: true, noContextFiles: true,
      skillsOverride: () => ({ skills, diagnostics: loaded.diagnostics }),
      appendSystemPrompt: [workflow.instructions ?? preparedCheckout.policy.instructions],
    });
    await loader.reload();
    const model = modelRuntime.getModel(request.provider, request.model);
    if (!model) throw new Error('Review model is not configured');
    ({ session } = await createAgentSession({
      cwd, agentDir: join(root, 'agent'), modelRuntime: meteredRuntime, model,
      thinkingLevel: request.thinkingLevel, settingsManager, resourceLoader: loader,
      sessionManager: SessionManager.inMemory(cwd), tools: tools.map((tool) => tool.name), customTools: tools,
    }));
    session.subscribe((event) => {
      if (event.type === 'tool_execution_start' || event.type === 'tool_execution_end' && event.toolName !== workflow.name) submitted = undefined;
      onEvent(event);
    });
    signal?.throwIfAborted();
    await session.prompt(request.prompt);
    signal?.throwIfAborted();
    const final = session.messages.findLast((message) => message.role === 'assistant');
    if (final?.stopReason !== 'stop') throw new Error('Review agent did not finish successfully');
    if (!submitted) throw new Error('Review agent did not submit a validated draft');
    return submitted;
  } finally {
    signal?.removeEventListener('abort', abort);
    try { session?.dispose(); } finally {
      await Promise.allSettled([...modelCalls]);
      let outcome = 'released';
      try { await close(); } catch (error) { outcome = 'release_failed'; throw error; } finally {
        try {
          if (vm) onEvent({ type: 'agent_allocation', receipt: { id: vm.id, attemptId, durationMs: Math.ceil(performance.now() - allocationStartedAt), cpus: 2, memoryMiB: 1024, outcome } });
        } finally {
          await Promise.all([rm(root, { recursive: true, force: true }), ...[...outputFiles].map((path) => rm(path, { force: true }))]);
        }
      }
    }
  }
}
