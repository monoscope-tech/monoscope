# Slack Agent consolidation plan

Status: implementation in progress. Interactive project writes require an in-app, per-message grant; confirmed writes remain unavailable. The native Slack Agent has not passed sandbox acceptance or gone live.

This plan narrows the [Slack Agent roadmap](2026-09-08-slack-agent.md) to the unfinished conversation and tool experience. The [implementation log](2026-09-08-slack-agent-implementation.md) records earlier work. Current code, tests, and live behavior take precedence over old status notes in that log.

## Goal and user contract

Monoscope answers a mention in a channel or a message in its Agent view. An issue alert opens the same investigation that users can continue in Slack or Monoscope. The agent can discover and use every meaningful project operation exposed by Monoscope's MCP server and CLI, subject to the linked user's permissions.

The first acceptance question comes from the reported screenshot: `@Monoscope what metrics can you access`. In a newly joined private channel, a linked user must receive a visible response. The response must list real metric names from the authorized project's catalog, with scope and recency stated. An unlinked user must receive a private account-link action. Both paths must produce a visible outcome.

“Slack Agent” means Slack's native Agent experience, configured through the Slack app's Agent features. The `APP` badge in a message does not prove that `agent_view` is enabled. The [manifest template](../docs/slack/agent-manifest.json) has not been applied or accepted in a live workspace.

## Current state and gaps

| Area | Current server path | Gap to close |
| --- | --- | --- |
| Slack intake | `Pages.Bots.Slack` verifies signed events, saves a receipt, and queues `processSlackEvent`. | Trace a first private-channel mention to a visible outcome. The screenshot does not identify the failing boundary. |
| Conversation | Slack thread binding, issue conversation IDs, history, turn checkpoints, and reply reservations exist. | Make the issue conversation the stable record across Slack and Monoscope. Prove recovery after retries, interruption, and follow-ups. |
| Native Agent | `agent_view` exists in a template; the worker calls `agents.sessions.setStatus`. | Apply the manifest in a sandbox and prove the Agent view, session lifecycle, stop action, and channel mentions. |
| Tools | `Pkg.AI.allToolDefs` and `executeToolCall` are a small hand-written list. `Web.MCP.allTools` derives API tools and adds composites. | Use one project capability catalog for agent discovery and execution. Remove duplicate tool contracts. |
| Metrics | MCP exposes `query_metrics`. `Telemetry.getMetricCatalogPage` serves the web catalog. | Add a bounded metric-discovery operation. Querying a known metric cannot answer which metrics exist. |
| Authorization | Slack identity resolution rechecks active project membership. | Carry the member's permission into each tool call. Membership alone cannot authorize administrative writes. |

The first-mention failure and the metric-discovery gap are separate defects. A missing metric tool can produce an incomplete answer; it does not explain the absence of any reply. Production logs or a controlled Slack replay are needed to locate that failure.

## Feature tree

1. **Reliable entry and delivery**
   - Mention in a public or private channel; direct message; Agent view prompt.
   - Private account link, reconnect notice, working state, answer, or actionable failure.
   - Durable retry without duplicate answers or abandoned `processing` status.
2. **One issue conversation**
   - Bind an alert root, Slack thread, incident episode, issue, and in-app chat to one project-scoped conversation.
   - Keep user messages, evidence, tool outcomes, progress, decisions, and final answers in order.
   - Accept follow-ups and stop requests while work is active; resume from a saved boundary.
3. **Complete project tools**
   - Discover tools by task, then load the selected schema and execute it.
   - Cover telemetry, metric catalog and queries, issues, incidents, monitors, dashboards, endpoints, log patterns, project settings, repository evidence, and other MCP/API project operations.
   - Map CLI project operations to the same server capabilities. CLI-local login, configuration, completion, and terminal rendering remain client functions.
   - Apply project scope, current membership, permission, action intent, and required confirmation at execution time.
4. **Evidence-led replies**
   - Show the query window, metric or event source, links, observed facts, hypotheses, and missing evidence.
   - Keep progress in the originating thread. Report later action outcomes there.
5. **Native Agent experience**
   - Agent view, suggested prompts, session status, stop behavior, and threaded replies.
   - Add streaming or richer task progress only after the durable answer path passes acceptance.

Ambient channel suggestions and autonomous fixes are later features. They require their own noise, authority, and outcome rules.

## Server design

### Keep one conversation worker

Extend the existing signed receipt, thread lock, turn checkpoint, and reply-publication path. Do not add a second Slack-specific agent executor. A queue item identifies stored work; persisted state decides what the worker runs. The worker owns input ordering, model work, tool calls, and delivery for one conversation at a time.

Treat Slack as an input and delivery adapter. The issue conversation owns history and tool evidence. A later Slack message joins that conversation by the stored thread and issue binding, not by text matching. A new mention in an unrelated thread starts a new conversation. A notification root with an issue ID binds to that issue's conversation.

Record each input and output boundary. A retry must distinguish “Slack accepted the reply” from “Slack did not accept it.” Unknown delivery outcomes use the existing reservation and reconciliation mechanism before another post.

### Share the capability contract

Use the MCP/API operation catalog as the coverage baseline. Each operation needs a stable name, description, input schema, result shape, project scope, and action class. Agent discovery returns short matching summaries; the selected operation returns its full schema. The model must not receive the entire MCP catalog on every turn.

`Web.MCP.runTool` currently dispatches some operations through an in-process Servant application under `ATBaseCtx`; the Slack worker uses `ATBackgroundCtx`. Directly calling that function from the worker is not a safe consolidation plan. Share operation definitions and underlying domain actions through an explicit adapter. Keep one authorization and result contract for MCP and Agent callers, with their distinct principals supplied at the boundary.

Add metric discovery from `Telemetry.getMetricCatalogPage`. Return bounded pages with name, type, unit, description, labels, last-seen time, and active status. Accept the catalog's service filter. State when a page is incomplete. Do not enumerate metric names by scanning raw datapoints; the existing catalog is the intended bounded source.

Generate a coverage inventory from the MCP registry and CLI command tree. Every project operation must map to a shared capability, or have a documented client-only reason. A parity test must fail when a new MCP project operation lacks an Agent policy. A missing policy must not grant access by default.

### Authorize each action

Scheduled routines retain the member who installed or resumed them and recheck that member before tools and publication. Migration `0212` pauses legacy routines with no recorded requester; a member must explicitly resume them. Agent tool execution preserves the caller's clock, HTTP, UUID, and LLM interpreters, and member writes retain their attribution.

Resolve the Slack user to a project member before work and again before each tool and reply. Include the member's permission, not only the project ID. Read tools use the current user's allowed project scope. Writes use the same domain permission rules as the product operation.

An explicit user request is required for a write. Destructive or administrative actions, such as key management, member changes, and deletion, require a reviewable confirmation bound to the operation, arguments, project, and requester. Recheck permission after confirmation and before execution. Tool results must state what the server actually changed.

The tool catalog can describe an unavailable action without executing it. The agent explains the missing permission or required confirmation. It must not use a service credential to bypass the linked user's role.

### Implementation touchpoints

- `Pages.Bots.Slack` owns signed intake, identity prompts, native session status, history backfill, and reply publication.
- `Pkg.AI` owns the model loop, checkpoints, Agent access checks, and current tool dispatch.
- `Web.MCP`, `Web.Routes`, and `Web.ApiHandlers` define the existing project API and MCP coverage baseline.
- `Models.Telemetry.Telemetry` supplies metric catalog pages. `Models.Apis.Integrations` resolves the linked Slack principal.
- `test/integration/Pages/Bots/WorkflowsSpec.hs` exercises signed Slack handlers and workers. Add regressions there for visible first-turn outcomes and issue continuity.

## Delivery sequence and acceptance

| Slice | Work | Evidence required before the next slice |
| --- | --- | --- |
| 1. First mention | Reproduce the screenshot path with a signed `app_mention` in a newly joined private channel. Inspect receipt, identity, scopes, startup status, thread history, model start, and publication. Add the failing regression before the fix. | Linked and unlinked users each get the expected visible outcome. Retry produces one answer. A failure records a boundary and leaves no silent pending turn. |
| 2. Metric answer | Expose the paged metric catalog and metric query through the Agent tool path. Keep project scope fixed by the linked identity. | “What metrics can you access?” names seeded metrics, reports recency and pagination, and never claims that an empty or failed read means no metrics exist. |
| 3. Tool parity | Add shared discovery and execution contracts, a CLI/MCP coverage inventory, and permission policy. Move existing Agent tools onto that contract without changing their result meaning. | Representative read and write operations agree across MCP, CLI, and Agent. Cross-project, revoked-member, lower-role, and missing-confirmation cases fail before side effects. |
| 4. Issue conversation | Use the existing issue binding, stored history, checkpoints, and delivery reservations for Slack and in-app follow-ups. | An alert, diagnosis, human reply, tool result, stop, retry, and recovery form one ordered issue conversation without duplicate posts. |
| 5. Native rollout | Apply the reviewed manifest to a sandbox installation, reconnect scopes, and run the Agent acceptance matrix. Keep notification delivery intact. | The Agent view, suggested prompts, DM, channel mention, session status, stop, and thread follow-up work in Slack. Verify private-channel history and rate limits on the actual installation. |

Use `build.log` and `build-test-dev.log` watchers for Haskell feedback. Keep integration regressions at the signed handler, worker, database, and Slack HTTP boundary. Use model fakes only at the LLM effect boundary. Before any pull-request update, run the repository's Haskell reviews and `make ci-signoff` as `AGENTS.md` requires.

## Live diagnosis and rollout gates

For the reported no-reply case, collect the workspace, channel, message timestamp, event ID, installation ID, and receipt ID. The screenshot contains none of these identifiers. Inspect whether Slack delivered `app_mention`, whether Monoscope queued and ran the receipt, and which external call last succeeded. Do not label a guessed failure as the root cause.

The sandbox matrix covers a linked member, an unlinked user, a revoked member, an admin, a lower-role member, a new private channel, an existing issue thread, a DM, and the Agent view. Repeat each case after a duplicate event, worker interruption, Slack rate limit, and ambiguous publication outcome. Verify that the original alert transport still delivers notifications.

Slack's Agent migration can be irreversible. Review the live manifest against the template before changing it. Record the workspace plan, app settings, granted scopes, event subscriptions, and observed Agent behavior. Code and OAuth scopes alone do not prove native Agent availability.

## Prior art and decisions

- [Slack's Agent guide](https://docs.slack.dev/ai/developing-agents/) defines the native response loop, Agent features, session status, and streaming methods.
- [Junior's conversation worker](https://github.com/getsentry/junior/blob/0dfd0e805789103be20ad20338b2117407f9c397/packages/junior/src/chat/task-execution/README.md) uses durable input, one lease per conversation, and delivery state. Monoscope can apply these rules in its existing server worker.
- [Junior's MCP search](https://github.com/getsentry/junior/blob/0dfd0e805789103be20ad20338b2117407f9c397/packages/junior/src/chat/tools/skill/search-mcp-tools.ts) separates tool discovery from execution. Monoscope needs that split for full tool parity.
- [Polylane's Slack flow](https://polylane.com/product/slack/) shows mentions, threaded follow-ups, a live checklist, and outcomes in the originating thread. Its [investigation model](https://docs.polylane.com/investigation/investigations) keeps one issue's evidence and fix outcome in one thread. These are product references, not claims about its internal implementation.

The recommended launch boundary is reliable requested conversations and authorized tools. Ambient replies, autonomous fixes, and automatic deployment follow only after the requested path passes live acceptance.
