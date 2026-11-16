# LLM & agent observability

Monoscope's AI view: conversations, agent runs, tool calls, tokens and cost, from
any OpenTelemetry producer, beside the HTTP/DB spans of the same trace.

## Principles

1. **OTel first, every dialect accepted.** We store the GenAI semantic conventions
   (`gen_ai.*`, semconv-genai 1.42-dev). Every other dialect in the wild (OpenInference,
   OpenLLMetry/Traceloop, Vercel AI SDK `ai.*`, Langfuse, Logfire, Claude Code) is
   normalised onto those keys **once, at ingest**, so every surface — explorer, KQL,
   dashboards, monitors, alerts, the AI assistant — reads one vocabulary. Raw keys are
   kept (additive, as `migrateHttpSemanticConventions` already does for HTTP/DB).
2. **Cost is an attribute, not a report.** `gen_ai.usage.cost` (USD) is stamped on every
   priced span at ingest, with `gen_ai.usage.cost_source` = `reported` | `estimated`.
   A producer-reported cost wins. That makes `summarize sum(attributes.gen_ai.usage.cost)`
   work in every existing surface with no new query path.
3. **Reuse the product.** Traces, the span drawer, Table, dashboards and the AI dock are
   extended, not forked. The AI page is a list + detail over the same spans.
4. **Signals before LLM judges.** Deterministic, free detectors (tool errors, truncation,
   refusals, loops, context pressure, retry storms) run on every conversation; LLM
   analysis is one click away through the existing assistant.

## Ingest normalisation (`Pkg.GenAI`, pure, doctested)

Hooked into `OtlpServer.keyValueToJSON`, so spans, span events and log records (Claude
Code exports `claude_code.api_request` as a *log*) all pass through it. A key-prefix
guard keeps the non-AI hot path to one scan.

| Canonical | Accepted sources (first wins) |
|---|---|
| `gen_ai.operation.name` | `openinference.span.kind`, `traceloop.span.kind` + `llm.request.type`, Vercel `ai.operationId` |
| `gen_ai.provider.name` | `gen_ai.system`, `llm.provider`, `llm.system`, `ai.model.provider` |
| `gen_ai.request.model` / `response.model` | `llm.model_name`, `llm.request.model_name`, `ai.model.id`, `ai.response.model`, Claude Code `model` |
| `gen_ai.usage.input_tokens` | `gen_ai.usage.prompt_tokens`, `llm.token_count.prompt`, `ai.usage.promptTokens`/`inputTokens`, `input_tokens` |
| `gen_ai.usage.output_tokens` | `gen_ai.usage.completion_tokens`, `llm.token_count.completion`, `ai.usage.completionTokens`/`outputTokens`, `output_tokens` |
| `gen_ai.usage.cache_read.input_tokens` | `gen_ai.usage.cache_read_input_tokens`, `llm.token_count.prompt_details.cache_read`, `cache_read_tokens` |
| `gen_ai.usage.cache_write.input_tokens` | `gen_ai.usage.cache_creation.input_tokens`, `gen_ai.usage.cache_creation_input_tokens`, `llm.token_count.prompt_details.cache_write`, `cache_creation_tokens` |
| `gen_ai.usage.reasoning.output_tokens` | `gen_ai.usage.reasoning_tokens`, `llm.token_count.completion_details.reasoning` |
| `gen_ai.usage.cost` (+ `cost_source`) | `llm.cost.total`, `gen_ai.cost.total_tokens`, `cost_usd`; else estimated |
| `gen_ai.input.messages` / `output.messages` | JSON strings parsed to arrays; OpenInference `llm.{input,output}_messages.N.message.*`, OpenLLMetry `gen_ai.{prompt,completion}.N.*`, Vercel `ai.prompt.messages`/`ai.response.text`, `input.value`/`output.value` |
| `gen_ai.conversation.id` | `session.id`, `langfuse.session.id`, `traceloop.association.properties.session_id`, `ai.telemetry.metadata.sessionId` |
| `gen_ai.agent.name` | `agent.name`, `traceloop.workflow.name`, `ai.telemetry.functionId` |
| `gen_ai.tool.name` / `call.id` / `call.arguments` / `call.result` | `tool.name`, `ai.toolCall.{name,id,args,result}`, `tool_name`, `tool_use_id` |
| `gen_ai.response.finish_reasons` | `llm.finish_reason`, `ai.response.finishReason`, `gen_ai.completion.0.finish_reason`, `stop_reason` |

Indexed message keys are folded into one messages array and dropped (otherwise the
dot-nesting stores them as `{"0":{…}}` objects nobody can query).

### Pricing

`static/model_prices.json`: a trimmed, generated copy of LiteLLM's MIT price table
(`scripts/update-model-prices.sh`), embedded at compile time. Lookup normalises the model id
(lowercase, no provider path, Bedrock region/vendor prefix or `-v1:0` revision, no `@` pin),
then tries the id without a date suffix, the longest known family prefix, and the nearest dated
release of a bare alias. Cost:

```
cached   = cache_read + cache_write
uncached = input − cached   if input ≥ cached   (GenAI conventions: input includes the cache)
         = input            otherwise          (Anthropic-native: input excludes it)
cost     = (uncached·in + cache_read·read + cache_write·write + output·out) / 1e6, ≥ 0
```

Claude Code's counts are exclusive by definition and are made inclusive at ingest. Only
inference operations (`chat`, `text_completion`, `generate_content`, `embeddings`) are priced:
agent spans may carry aggregate usage, and pricing them would count every call twice.
Dashboards that sum *tokens* must filter on those operations for the same reason.

Unknown models carry tokens but no cost; the UI says "unpriced" rather than `$0`.

## Surfaces

### Everywhere spans already render
- **Summary pills** (`generateSpanSummary`): model, `1.2k→340 tok`, `$0.0042`, tool name,
  finish reason when abnormal. The log explorer, live tail and waterfall pick these up.
- **Span drawer → "Conversation" tab** for any gen_ai span: messages as a transcript
  (system collapsed, tool calls with arguments, tool results), usage/cost header.
- **Trace header:** total LLM cost and tokens when the trace has gen_ai spans.

### LLM & Agents page (`/p/:pid/llm`, `/llm/conversations/:key`, `/llm/dashboard`)
- **Overview:** the first widgets of `llm-observability.yaml` (model calls, cost, error rate,
  tokens), rendered by the shared `Widget`, so the page and the dashboard cannot disagree.
- **Tabs.** *Conversations*; *Topics* (opening messages clustered with `Pkg.Drain`);
  *Signals* (plain-language signals); *LLM calls* is the Log Explorer with
  `attributes.gen_ai.operation.name != null`, so calls get every Explorer feature for free.
- **Promoted columns.** `gen_ai.operation.name`, `request.model`, `conversation.id`,
  `agent.name`, `tool.name`, `usage.input_tokens`, `usage.output_tokens` and `usage.cost` are
  written at ingest to `attributes___gen_ai___*` columns on both stores (TimeFusion storage
  widened 2026-11-16; Postgres migration 0286). KQL resolves `attributes.gen_ai.*` to the
  column automatically, so they filter and aggregate like any first-class field. Rows ingested
  before the promotion only have the `attributes` blob.
- **Totals** are aggregated in SQL per conversation key (GROUP BY over the window), not over a
  capped span sample.
- **Conversations table** (shared `Table`): title (first user message), agent, models,
  turns, tool calls, tokens, cost, errors/signals, duration, last seen. Grouping key is
  `gen_ai.conversation.id`, falling back to the trace id; a trace adopts the id any of its
  spans carries (frameworks put it only on the root agent span). Facets: agent, model,
  signal.
- **Conversation detail:**
  - *Transcript*: each new message once (LLM inputs repeat the history; we diff), with
    per-step model, latency, tokens, cost chips, tool calls inline with their results.
  - *KPI tiles*: cost (and unpriced calls), tokens in/out/cached, model calls with average
    latency, tool calls and failures, errors, wall and active time.
  - *Timeline* (default): the AI spans as a waterfall nested under their parent span, on the
    conversation's own time axis, coloured by kind (agent, model call, tool; errors red), with
    tokens, cost and the running total per step; each row opens the span drawer.
  - *Cost by model* and *Tools* cards beside it; "Open trace" goes to the full trace, whose
    header links back with "View conversation".
  - *Signals*: deterministic findings with the step that caused them.
  - *Ask AI*: opens the assistant dock seeded with this conversation.
- **Dashboard** (`llm-observability.yaml`, read-only template): cost by model, tokens by
  type, calls and p95 latency by model, error rate, top tools and tool errors, cost by agent,
  most expensive conversations.
- **Setup** (zero state): one-paste snippets for OTel GenAI (Python/Node/Go/Java/.NET),
  OpenLLMetry, OpenInference, Vercel AI SDK, Pydantic AI/Logfire, LangChain/LangGraph,
  OpenAI Agents SDK, CrewAI, Mastra, Google ADK, Strands, Agno, Bedrock/Azure/Vertex,
  Claude Code CLI, Claude Console push, MCP, plain OTLP/HTTP.

### Plain-language signals (Raindrop-style)
A signal is a name plus a description ("The user repeats a request or gives up"), stored in
`apis.llm_signals`. `EvaluateLLMSignals pid` (on creation, on "Judge new conversations now", and
hourly for active projects) scans the day once, fetches each unjudged conversation's spans in
one query, and asks the small model once per signal about up to 20 conversations (opening
request, latest message history, failed tools). A key the judge answers twice is dropped
(`Drain.answeredOnce`: transcripts are user-written). Verdicts land in
`apis.llm_signal_matches` with a one-sentence reason. Judged and built-in signals are one
`Flag` list on a conversation, so matches show as pills (reason on hover) and in the Signal facet next to the built-in ones. Names are unique per
project, built-in labels included. A conversation is judged once; one that keeps growing
afterwards is not re-judged (follow-up: re-judge on new turns).

### Signals (deterministic)
| Signal | Rule |
|---|---|
| Tool error | `execute_tool` span with error status |
| Truncated | finish reason `length` / `max_tokens` |
| Refusal / filtered | finish reason `content_filter` / `refusal`, Claude Code `api_refusal` |
| Loop | same tool + arguments ≥ 3 times in a conversation |
| Context pressure | input tokens ≥ 80 % of the model's context window |
| LLM error | inference span with error status |

## Dogfooding

Monoscope's own assistant is an agent: the copilot dock, issue analysis, Slack
investigations, routines and the log-pattern judges all call `Data.Effectful.LLM`.
- `runLLMReal` calls the OpenAI client directly (langchain-hs discards `usage`) and wraps each
  call in a `chat {model}` span with the canonical attributes and real token usage.
- The agent loop opens `invoke_agent monoscope-assistant` (with `gen_ai.conversation.id`)
  and an `execute_tool {name}` span per tool call.
- `projects.ai_usage` is billed from the provider's reported usage (`ELLM.Reply.usage`); the
  bytes/4 estimate remains only for golden replays that carry none.
- Our self-telemetry project therefore shows every assistant conversation, its cost and its
  tools — the first customer of the feature.

### Content capture
Our own spans record prompts and completions only when
`OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT` is `SPAN_ONLY`/`SPAN_AND_EVENT` (the GenAI
conventions' switch). Production leaves it off — our prompts carry customers' telemetry — so
the self-telemetry project shows tokens, cost, models and tools there, and transcripts in dev.

## Tests
- Doctests: dialect normalisation per dialect, model-id normalisation, cost formula,
  provided-cost precedence, transcript diffing.
- Integration (`Opentelemetry.GenAISpec`): ingest one conversation mixing dialects through the
  real OTLP path; assert normalised attributes and cost on the stored rows, the conversations
  list, topics, the LLM-calls Explorer query, a plain-language signal judged by a stubbed
  model, the trace's cost totals, Claude Code sessions, and billing from reported usage.
- Dogfood: run the assistant against golden LLM responses with an in-memory span exporter,
  replay its spans through ingest, and assert the AI page shows the conversation.

## Follow-ups (not in this change)
- Once rows from before 2026-11-16 age out of retention, move `selectUnits`, `selectCalls` and
  `conversationScope` from the `kind;neutral⇒ai` summary marker and attribute-blob reads to the
  promoted columns. Until then a window crossing that date counts older conversations in the
  table but not in the overview widgets or LLM calls, which read the columns.
- Signal matches as issues/alerts (route a signal through the issues pipeline).
- Resolve `gen_ai.*_ref` content URIs.
- Normalise per dialect in a pre-pass (detector + alias table + fixups) so new dialects do not
  touch the shared derivation; stamp an `inference` marker so KQL filters stop listing operations.
- Interpret `LLM` above `Tracing` so inference spans use the `Tracing` effect, and share one
  TLS client across calls.
