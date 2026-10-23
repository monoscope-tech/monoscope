#!/usr/bin/env node
// Pi session host. Haskell owns the VM, workflow and policy; this process runs the model session, meters it, and
// forwards Pi's tools to the host. runPi is shared with the research runners, which use the same native guest operations.
import { createHash, randomUUID } from 'node:crypto';
import * as fs from 'node:fs';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { dirname, join, posix, relative, resolve } from 'node:path';
import { addAbortSignal } from 'node:stream';
import { fileURLToPath } from 'node:url';
import {
  createAgentSession, createBashToolDefinition, createEditToolDefinition, createReadToolDefinition, createWriteToolDefinition,
  DefaultResourceLoader, loadSkills, ModelRuntime, SessionManager, SettingsManager,
} from '@earendil-works/pi-coding-agent';
import { Type } from 'typebox';
import { Assert } from 'typebox/value';
import writeFileAtomic from 'write-file-atomic';
import { z } from 'zod';
import { jsonLines } from './jsonl.mjs';

export const cwd = '/workspace';
export const checkoutSchema = z.object({ guest: z.string().nullable(), envelope: z.record(z.string(), z.unknown()).nullable(), directory: z.string().min(1), repositorySkills: z.string().min(1), rootInstructions: z.string(), projectId: z.uuid(), repository: z.string(), base: z.string(), head: z.string(), cacheStatus: z.enum(['hit', 'miss', 'verified']), durationMs: z.number().int().nonnegative(), policy: z.object({ instructions: z.string(), tools: z.array(z.object({ name: z.string(), description: z.string(), parameters: z.record(z.string(), z.unknown()), sequential: z.boolean() }).strict()), builtins: z.array(z.string()) }).strict() }).strict();

export function parseSubmission(checkout, name, draft) {
  const schema = checkout.policy.tools.find((tool) => tool.name === name)?.parameters;
  if (!schema) throw new Error(`Host policy does not define ${name}`);
  Assert(schema, draft);
  return draft;
}

// Patch-only controls share execution and validation; each runner supplies its own review contract.
export async function reviewPatch(diff, { modelRuntime, model, preparedCheckout, submit, systemPrompt, prompt, thinkingLevel, signal, onEvent, validateDraft }) {
  const patches = diff.split(/(?=^diff --git )/m).filter(Boolean);
  const bounded = patches.slice(0, 30).map((patch) => patch.slice(0, 2000)).join('\n');
  onEvent({ type: 'patch_coverage', files: Math.min(30, patches.length), totalFiles: patches.length, characters: bounded.length });
  const response = await modelRuntime.completeSimple(model, { systemPrompt, messages: [{ role: 'user', content: prompt + bounded, timestamp: Date.now() }] }, { reasoning: thinkingLevel, signal });
  onEvent({ type: 'model_response', message: response });
  if (response.stopReason !== 'stop') throw new Error('Review did not complete');
  const draft = parseSubmission(preparedCheckout, submit, JSON.parse(response.content.filter((block) => block.type === 'text').map((block) => block.text).join('')));
  const checked = await validateDraft(draft);
  onEvent({ type: 'source_submission', draft, accepted: Object.hasOwn(checked, 'Right') });
  if (Object.hasOwn(checked, 'Left')) throw new Error(checked.Left);
  return draft;
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

// Pi writes truncated bash output to a host file; `publish` places it in the guest and names its guest path.
export function bashTool(exec, publish) {
  const definition = createBashToolDefinition(cwd, { operations: { exec }, exposeSessionEnvironment: false });
  return {
    ...definition,
    async execute(...args) {
      const result = await definition.execute(...args);
      const path = result.details?.fullOutputPath;
      if (path) {
        const guestPath = await publish(path);
        result.details.fullOutputPath = guestPath;
        result.content = result.content.map((block) => block.type === 'text' ? { ...block, text: block.text.replaceAll(path, guestPath) } : block);
        await rm(path, { force: true });
      }
      return result;
    },
  };
}

/**
 * Runs one Pi session over `tools` and returns the accepted submission. `workflow` names the submit tool and
 * prepares each candidate; tool contracts come from the host policy.
 */
export async function runPi({ preparedCheckout, request, modelRuntime, attemptId, tools, workflow, skillsDir, root, optionsTool, signal, onEvent }) {
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
  let session, submitted;
  const abort = () => void session?.abort();
  signal?.addEventListener('abort', abort, { once: true });
  try {
    const repositorySkills = preparedCheckout.repositorySkills;
    const loaded = loadSkills({ cwd: root, agentDir: join(root, 'agent'), skillPaths: [skillsDir, ...['.agents/skills', '.pi/skills'].map((path) => join(repositorySkills, path))], includeDefaults: false });
    const skills = loaded.skills.map((skill) => {
      const filePath = skill.filePath.startsWith(repositorySkills + '/') ? cwd + '/' + relative(repositorySkills, skill.filePath) : '/skills/' + relative(skillsDir, skill.filePath);
      return { ...skill, filePath, baseDir: dirname(filePath) };
    });
    const submission = preparedCheckout.policy.tools.find((tool) => tool.name === workflow.name)?.parameters;
    if (!submission) throw new Error(`Host policy does not define ${workflow.name}`);
    tools = [...tools, {
      name: workflow.name, parameters: Type.Unsafe(submission),
      async execute(_id, params, toolSignal) {
        const reply = await workflow.prepare(parseSubmission(preparedCheckout, workflow.name, params), toolSignal);
        const isError = Object.hasOwn(reply, 'Left');
        if (!isError) submitted = reply.Right;
        return { content: [{ type: 'text', text: isError ? reply.Left : 'Draft accepted. Finish without further tool calls.' }], details: {}, isError };
      },
    }];
    if (optionsTool) for (const definition of preparedCheckout.policy.tools) {
      if (tools.some((tool) => tool.name === definition.name)) continue;
      tools.push({ ...definition, label: definition.name, parameters: Type.Unsafe(definition.parameters),
        async execute(_id, args) {
          const response = await optionsTool({ name: definition.name, arguments: args });
          const isError = Object.hasOwn(response, 'Left');
          return { content: [{ type: 'text', text: isError ? response.Left : typeof response.Right === 'string' ? response.Right : JSON.stringify(response.Right) }], details: {}, isError };
        },
      });
    }
    const sequential = preparedCheckout.policy.tools.some((tool) => tool.sequential);
    for (const tool of tools) {
      const definition = preparedCheckout.policy.tools.find((item) => item.name === tool.name);
      if (definition) Object.assign(tool, { label: definition.name, description: definition.description, parameters: Type.Unsafe(definition.parameters) });
      if (sequential) tool.executionMode = 'sequential';
    }
    const settingsManager = SettingsManager.inMemory({ retry: { enabled: true, maxRetries: 1 } });
    const loader = new DefaultResourceLoader({
      cwd: root, agentDir: join(root, 'agent'), settingsManager,
      noExtensions: true, noSkills: true, noPromptTemplates: true, noThemes: true, noContextFiles: true,
      skillsOverride: () => ({ skills, diagnostics: loaded.diagnostics }),
      systemPrompt: preparedCheckout.policy.instructions,
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
    if (session.messages.findLast((message) => message.role === 'assistant')?.stopReason !== 'stop') throw new Error('Review agent did not finish successfully');
    if (!submitted) throw new Error('Review agent did not submit a validated draft');
    return submitted;
  } finally {
    signal?.removeEventListener('abort', abort);
    try { session?.dispose(); } finally { await Promise.allSettled([...modelCalls]); }
  }
}

// Pi's file and shell calls become host Guest operations, run as the review account in the host-owned VM.
export function hostTools(preparedCheckout, root, guestOperation) {
  const guest = async (tag, contents) => {
    const reply = await guestOperation({ tag, contents });
    if (Object.hasOwn(reply, 'Left')) throw Object.assign(new Error(reply.Left), tag === 'Access' ? { code: 'ENOENT' } : {});
    return reply.Right;
  };
  let guestLog;
  const tools = [bashTool(async (command, cwd, { onData, timeout }) => {
    const ran = await guest('Run', { command, cwd, timeout: timeout ?? null });
    guestLog = ran.log;
    if (ran.timedOut) throw new Error(`timeout:${timeout}`);
    onData(Buffer.from(ran.output, 'base64'));
    if (ran.truncated) onData(Buffer.from('\n[Output beyond 64 MiB was discarded]\n'));
    return { exitCode: ran.exitCode };
  }, async () => guestLog), createReadToolDefinition(cwd, { operations: {
    access: (path) => guest('Access', path),
    readFile: async (path) => Buffer.from(await guest('Read', path), 'base64'),
  } })];
  const guestPath = (input) => {
    const path = posix.resolve(cwd, (input.startsWith('file://') ? fileURLToPath(input) : input.replace(/^@/, '')).replace(/^~(?=\/|$)/, '/home/review-agent'));
    return z.string().refine((value) => !value.includes('\0'), 'File paths cannot contain NUL bytes').parse(path);
  };
  // Pi queues mutations by host realpath, so it only ever sees private stand-ins for guest paths.
  for (const [name, defineTool] of [['edit', createEditToolDefinition], ['write', createWriteToolDefinition]]) if (preparedCheckout.policy.builtins.includes(name)) tools.push({
    ...defineTool(cwd),
    async execute(id, args, toolSignal, onUpdate, context) {
      const path = guestPath(args.path);
      const queuePath = join(root, 'file-tools', createHash('sha256').update(path).digest('hex'));
      const restore = (text) => text.replaceAll(queuePath, path);
      try {
        const result = await defineTool(cwd, { operations: {
          access: () => guest('Writable', path),
          readFile: async () => Buffer.from(await guest('Read', path), 'base64'),
          writeFile: (_path, content) => guest('Write', [path, content]),
          mkdir: () => guest('Mkdir', posix.dirname(path)),
        } }).execute(id, { ...args, path: queuePath }, toolSignal, onUpdate, context);
        return { ...result, content: result.content.map((item) => item.type === 'text' ? { ...item, text: restore(item.text) } : item),
          details: { ...result.details, ...(result.details?.patch ? { patch: restore(result.details.patch) } : {}) } };
      } catch (error) {
        throw new Error(restore(error.message));
      }
    },
  });
  return tools;
}

async function main() {
  const controller = new AbortController();
  for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, () => controller.abort());
  const signal = AbortSignal.any([controller.signal, AbortSignal.timeout(420000)]);
  let root;
  let pending = Promise.resolve();
  try {
    const messages = jsonLines(addAbortSignal(signal, process.stdin), 16 * 1024 * 1024);
    const envelope = JSON.parse((await messages.next()).value);
    const request = envelope.tag === undefined ? envelope : envelope.tag === 'FixTask' ? envelope.contents.request : envelope.contents.fix.request;
    const outbox = fs.realpathSync(process.env.IMPACT_REVIEW_OUTBOX_DIR);
    const exchange = (tag, contents) => {
      const reply = pending.then(async () => {
        process.stdout.write(JSON.stringify({ tag, contents }) + '\n');
        const response = JSON.parse((await messages.next()).value);
        if (typeof response.Left !== 'string' && !Object.hasOwn(response, 'Right')) throw new Error('Invalid query response');
        if (['Call', 'Usage'].includes(tag) && Object.hasOwn(response, 'Left')) throw new Error('Receipt was rejected');
        return response;
      });
      pending = reply.catch(() => controller.abort());
      return reply;
    };
    const checkoutReply = await exchange('Checkout', []);
    if (Object.hasOwn(checkoutReply, 'Left')) throw new Error('Host checkout was rejected');
    const preparedCheckout = checkoutSchema.extend({ guest: z.string() }).parse(checkoutReply.Right);
    // Receipts are durable before transmission so a lost host can replay them.
    const persistReceipt = (tag, receipt) => {
      writeFileAtomic.sync(join(outbox, `${receipt.id}.json`), JSON.stringify(tag === 'Usage' ? receipt : { tag, contents: receipt }) + '\n', { mode: 0o600 });
      const fd = fs.openSync(outbox, 'r');
      try { fs.fsyncSync(fd); } finally { fs.closeSync(fd); }
      return exchange(tag, receipt);
    };
    const attemptId = z.uuid().parse(request.attemptId);
    root = await mkdtemp(join(tmpdir(), 'monoscope-review-agent-'));
    const modelRuntime = await ModelRuntime.create({ authPath: join(root, 'auth.json'), modelsPath: process.env.IMPACT_REVIEW_MODELS ?? null, refreshOnCreate: false });
    const result = await runPi({
      preparedCheckout, request: { ...request, head: preparedCheckout.head }, modelRuntime, attemptId, signal, root, skillsDir: preparedCheckout.guest,
      tools: hostTools(preparedCheckout, root, (operation) => exchange('Guest', operation)),
      optionsTool: (invocation) => exchange('Tool', invocation),
      workflow: {
        name: preparedCheckout.policy.tools.find((tool) => tool.name.startsWith('submit_'))?.name,
        async prepare(draft) {
          const result = preparedCheckout.envelope ? { ...preparedCheckout.envelope, decision: draft } : draft;
          const reply = await exchange('Validate', result);
          return Object.hasOwn(reply, 'Left') ? reply : { Right: result };
        },
      },
      onEvent(event) {
        if (event.type === 'model_started') return persistReceipt('Call', event.receipt);
        if (event.type === 'model_response') {
          const message = event.message;
          void persistReceipt('Usage', modelUsage(message, modelRuntime.getModel(message.provider, message.model)?.cost ?? null, attemptId, event.callId)).catch(() => controller.abort());
        }
        if (event.type === 'tool_execution_start' || event.type === 'tool_execution_end') {
          process.stderr.write(JSON.stringify({ event: event.type, tool: event.toolName, failed: event.isError ?? false }) + '\n');
        }
      },
    });
    await pending;
    signal.throwIfAborted();
    process.stdout.write(JSON.stringify({ tag: 'Result', contents: result }) + '\n');
  } catch {
    await pending;
    process.stderr.write(signal.aborted ? 'Impact review cancelled or timed out\n' : 'Impact review did not produce a validated result\n');
    process.exitCode = 1;
  } finally {
    process.stdin.destroy();
    if (root) await rm(root, { recursive: true, force: true });
  }
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) await main();
