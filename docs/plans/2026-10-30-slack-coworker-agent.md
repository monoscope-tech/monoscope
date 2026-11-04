# Slack coworker agent

Goal: Monoscope behaves like an on-call teammate in Slack. It answers in threads and DMs, takes real actions after asking first, remembers what the team taught it, reads the team's Slack history when it helps, and follows up on its own work.

## Where we are (review, 2026-10-30)

Already solid; keep it:

- **Ingress** (`Pages/Bots/Slack.hs`): signed events go to the `apis.slack_events` receipt, then to `ProcessSlackEvent`. Per-thread advisory locks, rate limits, idempotent reply batches with history reconciliation, stop button (`agent_session_stopped`), `agents.sessions.setStatus`, App Home identity linking, and per-step revalidation of the requester (`AI.requireAgentAccess`).
- **Agent** (`Pkg/AI.hs`): ~20 hand-written read tools, Slack-only incident tools (`get_incident_context`, `get_related_incidents`, `get_investigation_history`), runbook/code/deploy tools, and a progress task list (`SlackProgress.hs`).
- **MCP server** (`Web/MCP.hs`): every `/api/v1` operation, writes included (`mute_monitor`, `create_monitor`, `apply_dashboard`, issue ack/archive, teams, members, share links), plus three composites. Only external clients use it.
- **Wiki/memory** (`Models/Projects/ProjectMemory.hs`): `Author = Member | Agent` already exists, but no agent tool reads or writes it.

Gaps that keep it from being a coworker:

1. **It cannot act.** Slack runs `processAIQuery`, which is hard-wired to `InteractiveReadOnly`. `send_to_slack` and any write are unreachable from Slack.
2. **There are two tool registries.** `allToolDefs`/`executeToolCall` duplicates read routes that MCP already exposes (`list_issues`, `get_issue`, `list_monitors`, `get_dashboard`, …), and these will drift.
3. **The agent experience is half wired.** We persist `assistant_thread_started` but never call `assistant.threads.setSuggestedPrompts`, so the screenshot's "Dynamic" prompts have nothing behind them. Thread titles (`agents.sessions.rename`) are never set. Replies arrive as one batch after the whole loop; there is no streaming.
4. **It has no memory between threads,** and `documentConversation` is `Nothing` for Slack.
5. **It is blind to Slack itself.** It cannot see what the team already said about a deploy or an incident.

## Phase 1: one tool registry, with confirmed actions

- Make the agent's tool belt `MCP.allTools` (OpenAPI + composites) plus the agent-only tools that have no REST route (incident thread context, runbooks, artifacts, wiki). Delete the duplicated hand-written read tools from `allToolDefs`/`executeToolCall`. The MCP descriptions then serve both Claude Desktop users and our own agent.
- Classify each tool once in `Web/MCP.hs`: GET/HEAD → `ReadOnly`, everything else → `Mutating` (also emit the MCP `readOnlyHint`/`destructiveHint` annotations). The agent sees `Mutating` tools only when `actionsAllowed`.
- Dispatch goes through `MCP.runTool` against `apiV1Server` with `Authenticated principal.projectId`. This is the same "never wider access" path the MCP endpoint uses. Run `requireAgentAccess` before and after each call, as `executeToolCall` does today.
- **Token budget:** `callOpenApi` truncates at 64 KB, but the agent's buffer is ~8k tokens. The agent-facing dispatch caps `content` at a few KB (or renders `structuredContent` through the existing `formatQueryResults`-style formatters) and injects a default `limit`. Without this, the dedup is a regression.
- **First implementation check:** composites are `ATBaseCtx`, and building the Wai `Application` needs `logger`/`tp`/`env`, but `executeToolCall` runs polymorphically under `ATBackgroundCtx`. OpenAPI tools are reachable through `callOpenApi :: … -> IO`; composites need a context shim. Settle this before the PR.
- **Ask before acting.** A `Mutating` call from Slack does not execute. It persists a `pending_action` row (tool, args, requester Slack user + Monoscope user, thread, expiry) and posts a Block Kit card ("Mute *checkout p95* for 1h? Approve / Cancel"). `slackActionsH` only persists a receipt and enqueues a job, then acks within 3 s, like `slackEventsPostH`. It never runs the tool inline, because `forkBackground` dies with the request. The job must come from the same Slack user (or a project admin), re-run `requireAgentAccess`, execute once (claim the row), and post the result into the thread. The agent loop resumes on the next turn with the outcome in history.
- The Slack path becomes `InteractiveWithActions`; the web chat already has the mode. Scheduled routines keep `ScheduledRoutine _ allowed`, and a routine's writes post the same approval card in its channel.
- Audit: record approver, tool, and args on the pending row. Have mutating REST handlers write activity "via Slack by @user" where an activity feed exists (issues).
- Migration: `apis.slack_pending_actions` (next free number on master).
- **Verify:** an integration spec drives a thread message, then "mute monitor X" (LLM golden). Assert that an approval card is posted and the monitor is unchanged. Simulate a click from another user: refused. Simulate the requester's click: monitor muted, and a second click is a no-op. Separately, `tools/list` and the agent tool list come from one source (doctest on classification).

## Phase 2: Slack-native agent experience

- **Dynamic suggested prompts:** on `AssistantStarted`/`AssistantContextChanged`, build up to 4 prompts from project state (open issues by recency, an active incident, the channel's last alert) and call `assistant.threads.setSuggestedPrompts`. Unlinked users get "Link your Monoscope account".
- **Thread titles:** after the first reply, call `agents.sessions.rename` with the conversation title (`AI.ensureConversationTitle` already produces one). Respect `agent_session_title_changed` (the user renamed it), so we never overwrite it.
- **Streaming:** `chat.startStream` → `appendStream` → `stopStream`, with `task_display_mode=plan`, replaces the current progress snapshots. `SlackProgress.Task` already matches Slack's task chunks. Constraints: a stream lives at most 10 min, and channel streams need `recipient_user_id`/`recipient_team_id`. Keep the durable reply batch as the source of truth: stream as a preview, then on stop attach the final blocks (charts, approval cards). Before building, confirm that `startStream`/`stopStream` accept `metadata`: reconciliation (`readPublicationHistory`) matches on `metadata.event_type = monoscope_investigation_reply`. Record the `ts` from `startStream` in the publication claim before the first `appendStream`, as `claimReplyPart` does today. Otherwise the crash fallback to `chat.postMessage` double-posts.
- **Agent description** in the app config: what it can do (investigate telemetry, issues, monitors, dashboards; act after approval) and what it can't (no code changes, no deploys).
- **Entry points:** keep @mention, DM, and bound thread. Add a message shortcut, "Ask Monoscope about this", on any message (an alert, a customer report), which opens a thread seeded with that message.
- **Verify:** event fixtures for `assistant_thread_started` assert the prompt payload. A stream fixture asserts start/append/stop order and the fallback on a `stopStream` failure.

## Phase 3: Slack MCP server, so the agent can read the team's Slack

- Be an MCP **client** of `https://mcp.slack.com/mcp`, using each linked user's **user token** (`oauth.v2.user.access`, `search:read.public` plus private/im/mpim only if the user opts in). Bot tokens cannot search. Extend the existing identity-link step: link account → optionally "let Monoscope search Slack as you". Store the token encrypted per (team, Slack user) under the existing `apiKeyEncryptionSecretKey`.
- Proxy the Slack MCP tools that read Slack (search messages, read channel/thread history, read canvas) into the agent's registry, next to Phase 1's tools. Exclude Slack's write tools: the bot already posts through our own audited path.
- **Audience rule:** never quote content into a channel whose audience can't see its source. A reply in a channel searches public channels plus the current channel only. The requester's full scope (private channels, DMs) is used only in their DM with the bot.
- Scope: the token is the **requester's**, so the agent only sees what that human can see. Never fall back to another user's token. Slack content is evidence, not instructions; the system prompt already says this, and it applies here too.
- Prerequisite: Slack allows MCP only for **directory-published or internal** apps. Confirm the Monoscope app's listing status before building.
- **Verify:** a golden-recorded MCP session (initialize, tools/list, search call). A spec asserts that an unlinked requester gets no Slack tools, and that a revoked token removes them.

## Phase 4: memory

- Agent tools `read_wiki` / `search_wiki` / `write_wiki` over `ProjectMemory`, with author `Agent attemptId`. Revisions and permission checks already exist.
- Writes are `Mutating`, so in Slack they use the Phase 1 approval card ("Save this as runbook `payments/retries`?"). Routines may append to their own page without approval.
- Read path: inject page titles under `runbooks/` and `team/` into the system prompt, and fetch bodies on demand.
- **Verify:** a spec where a thread teaches a fact, the user approves the save, and a new thread retrieves it.

## Phase 5: proactive follow-up, opt-in per channel

- On an incident alert in a channel with "auto-investigate" on, the bot replies in the alert thread with a first-look summary (the same agent with `get_incident_context`). Today that only happens when someone asks.
- If the bot was investigating a thread and the incident recovers or resolves, post one closing update in that thread, reusing `reconcileIncidentDeliveries`.
- Never act unasked: proactive runs are `InteractiveReadOnly`, and suggested actions go out as approval cards for a human.
- **Verify:** a monitor fires in a fixture channel with auto-investigate on → exactly one summary reply. Off → none. Recovery → one closing reply.

## Slack app config (outside the repo)

- Turn on Agent experience (done), fill in the agent description, and set suggested prompts to Dynamic (done; Phase 2 serves them).
- Add scopes `search:read.*` as **user** scopes (Phase 3). Bot scopes are unchanged: `assistant:write` and `chat:write` already cover streaming.
- Add an interactivity message shortcut (Phase 2). The Slack MCP toggle is on (done; it needs the publish status above).
- Check a Slack app manifest into `docs/` so scope drift is reviewable, because `startInstallGetH`'s scope string is the only record today.

## Open questions

1. **"mcpo":** I read it as "MCP too", meaning the agent uses our MCP tools and Slack's MCP server. If you meant [`mcpo`](https://github.com/open-webui/mcpo), the MCP→OpenAPI proxy, it does the inverse of what we need: our tools already *start* as OpenAPI and `Web/MCP.hs` derives MCP from them in-process, so a proxy would add a hop and give nothing new.
2. Who can approve an action: only the requester, or any project admin in the channel?
3. Should Phase 5 auto-investigate be on by default for new installs?
4. Slack private-channel/DM search (Phase 3): keep the audience rule above, or public channels only everywhere?

Order: Phase 1 → 2 → 4 → 5 → 3 (3 is gated on Slack app publication).
