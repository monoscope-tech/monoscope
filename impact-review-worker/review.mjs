import { execFile } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { cp, mkdtemp, mkdir, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { basename, dirname, isAbsolute, join, relative } from 'node:path';
import { constants, createReadStream } from 'node:fs';
import { RealFSProvider, ReadonlyProvider, VM, importImageFromDirectory, resolveImageSelector } from '@earendil-works/gondolin';
import { promisify } from 'node:util';
import { fileURLToPath } from 'node:url';
import {
  createAgentSession, createBashToolDefinition, createReadToolDefinition,
  DefaultResourceLoader, loadSkills, SessionManager, SettingsManager,
} from '@earendil-works/pi-coding-agent';
import { z } from 'zod';
import { Type } from 'typebox';
import { agentCommand, guestTmpfs, loadPreparedWorkspace, runRegression } from './workspace.mjs';
export { agentCommand, guestTmpfs } from './workspace.mjs';

const exec = promisify(execFile);
const sha = z.string().regex(/^[a-f0-9]{40}(?:[a-f0-9]{24})?$/);
export const reviewGrounding = 'Ground external API compatibility claims in the specified library/runtime version and its actual signature or implementation. For claims about changed incidental behavior, establish user impact through a concrete caller or documented contract. Do not turn an unverified contract assumption into a finding.';
export const regressionParameters = {
  script: Type.String({ minLength: 1, maxLength: 65536, description: 'Bash command exercising the actual repository code. A failed regression assertion must exit nonzero and emit failureMessage; merely catching and printing an error is not an assertion. Keep setup diagnostics on stdout/stderr, or print redirected logs when setup fails: verifier VMs are discarded after execution. Do not fabricate a failure or replace the implementation under test.' }),
  failureMessage: Type.String({ minLength: 1, maxLength: 200, description: 'Exact case-sensitive literal substring emitted by the failing assertion on stdout or stderr, not a prose summary or the expected successful output. Inspect the test or failure log first. Never use a setup/dependency error as the assertion.' }),
};
export const reviewRequest = z.object({
  projectId: z.uuid(), repository: z.string().min(1), base: sha, head: sha,
  gitToken: z.string(), prompt: z.string(), model: z.string().min(1),
  provider: z.string().min(1), thinkingLevel: z.enum(['off', 'minimal', 'low', 'medium', 'high', 'xhigh']),
});
export const reviewDraft = z.object({
  findings: z.array(z.object({
    location: z.object({ path: z.string(), line: z.number().int().positive(), side: z.enum(['base', 'head']) }),
    mechanism: z.string().min(1).max(1200).describe('Explain a concrete runtime regression: under the same conditions, what does base do and what does head do worse? A pre-existing limitation that head does not fix is not an introduced defect. Omit it unless the change creates a new failure or materially worsens the existing one.'), nextStep: z.string().min(1).max(600),
    evidenceKeys: z.array(z.string()).min(1).max(3),
  })).max(5),
  coverage: z.array(z.string().min(1).max(400)).max(10).describe('Missing evidence or verification only. Do not summarize completed investigation.'),
});


export async function lockWorkspace(vm, cwd, signal) {
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

export async function runRepositoryAgent(input, { modelRuntime, guestImage, expectedImageBuildId, workspaceCache, artifactDir, preparedCheckout, attemptId = randomUUID(), skillPaths = [], validateDraft, createWorkflow, optionsTool, signal, onEvent = () => {} }) {
  const request = reviewRequest.parse(input);
  const runId = artifactDir ? z.uuid().parse(input.runId) : undefined;
  const resolvedImage = resolveImageSelector(guestImage);
  const pinnedImage = resolvedImage.source === 'path' ? importImageFromDirectory(resolvedImage.assetDir) : resolvedImage;
  if (expectedImageBuildId !== undefined && pinnedImage.buildId !== expectedImageBuildId) throw new Error('Configured guest image differs from the pinned issue-fix image');
  if (!preparedCheckout) throw new Error('A host-prepared checkout is required');
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
    const executeRegression = async (artifact, toolSignal, { revisions = [['base', request.base], ['head', request.head]], recordExecution = () => {} } = {}) => {
      const active = AbortSignal.any([signal, toolSignal].filter(Boolean));
      const result = await runRegression({ projectId: request.projectId, runId, attemptId, sessionId: verifierSession, base: request.base, artifact, revisions }, pinnedImage,
        { artifactDir, checkout, workspace, signal: active });
      for (const receipt of result.receipts) {
        onEvent({ type: 'regression_execution', receipt });
        await recordExecution(receipt);
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
    const workflow = createWorkflow ? await createWorkflow({ vm, cwd, request, root, artifactDir, executeRegression, signal, rootInstructions: preparedCheckout.rootInstructions }) : {
      name: peer ? 'submit_fix_review' : 'submit_review',
      prepare: async (draft, toolSignal) => {
        const active = AbortSignal.any([signal, toolSignal].filter(Boolean));
        active.throwIfAborted();
        const result = peer ? { result: peer.result, decision: draft } : draft;
        const reply = validateDraft ? await validateDraft(result) : { Right: [] };
        active.throwIfAborted();
        return Object.hasOwn(reply, 'Left') ? reply : { Right: result };
      },
    };
    tools.push(...(workflow.tools ?? []));
    const submission = workflow.schema?.toJSONSchema() ?? preparedCheckout.policy.tools.find((tool) => tool.name === workflow.name)?.parameters;
    if (!submission) throw new Error(`Host policy does not define ${workflow.name}`);
    tools.push({
      name: workflow.name, label: workflow.label ?? 'Submit review draft',
      parameters: Type.Unsafe(submission),
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
        async execute(_id, args) {
          const response = await optionsTool({ name: definition.name, arguments: args });
          const isError = Object.hasOwn(response, 'Left');
          return { content: [{ type: 'text', text: JSON.stringify(isError ? response.Left : response.Right) }], details: {}, isError };
        },
      });
    }
    for (const tool of tools) {
      const definition = preparedCheckout.policy.tools.find((item) => item.name === tool.name);
      if (definition && !workflow.instructions) Object.assign(tool, { label: definition.name, description: definition.description, parameters: Type.Unsafe(definition.parameters), ...(definition.sequential ? { executionMode: 'sequential' } : {}) });
    }
    if (createWorkflow || peer) for (const tool of tools) tool.executionMode = 'sequential';
    const settingsManager = SettingsManager.inMemory({ retry: { enabled: true, maxRetries: 1 } });
    const loader = new DefaultResourceLoader({
      cwd: root, agentDir: join(root, 'agent'), settingsManager,
      noExtensions: true, noSkills: true, noPromptTemplates: true, noThemes: true, noContextFiles: true,
      skillsOverride: () => ({ skills, diagnostics: loaded.diagnostics }),
      systemPrompt: workflow.instructions ?? preparedCheckout.policy.instructions,
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
