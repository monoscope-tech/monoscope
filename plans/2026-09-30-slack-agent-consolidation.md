# Slack Agent consolidation plan

Status: resumed on 2026-10-29; rebased onto master `efefb3b83`. Implementation in progress. Interactive project writes require an in-app, per-message grant; confirmed writes remain unavailable. The native Slack Agent has not passed sandbox acceptance or gone live.

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


## Routine integration (2026-10-29)

The product record is an installed routine, its runs, and its artifacts. Slack and
PR webhooks remain delivery adapters with their existing deduplication, leases,
and publication checks. They must not become separate invisible AI systems.

1. Rebase while preserving master’s repository setup, documents, saved views,
   cancellation, and shared effects. Verify with the long-lived watchers and the
   affected API, bot, routine, and PR-review integration tests.
2. Extend the existing routine schedule algebra with event triggers. Add Slack
   investigation and production-impact-review templates alongside scheduled
   templates. External AI work requires an explicitly activated template owned by an
   authorizing member. Keep existing review history visible; show a clear
   activation action for connected integrations. Pausing an installation suppresses
   future work without disconnecting notification or repository access.
3. Record every triggered execution in the routine run ledger, including source,
   requester, input, status, result, and errors. Preserve the source workers’
   retry/publication guarantees. Keep run artifacts as snapshots so later edits
   do not rewrite past evidence. Scheduled routines use the same artifact record.
4. Expose installed event routines, editable instructions, run details, artifacts,
   and conversation follow-ups in the AI UI. Link Slack replies and PR reviews to
   their visible record. Repository review settings control the same activation.
   Verify meaningful complete flows, narrow layouts, keyboard operation, and both
   themes using emil-design-eng and all six better-interface domain reviews.
5. Run hs-distill, hs-evasion-review, and hs-lob-review repeatedly; apply valuable
   findings, then simplify. Only after implementation and focused verification are
   complete, run final local signoff and the required image check, push the branch,
   and update the pull request. No production deployment is requested.

Acceptance requires signed Slack intake through a visible run and retained
artifact; PR webhook through an authorized review run and artifact; disabled,
revoked, duplicate, stopped, failed, and retried work handled visibly; project
isolation; and a user able to inspect and iterate from the routine UI. Tests must
first reproduce missing behavior at handler/worker level. Live Slack sandbox
acceptance remains a separate external installation gate, and must be reported
honestly if the installation is unavailable.

### Verified implementation checkpoints

- Rebased on `origin/master` at `efefb3b83`; retained the newer repository,
  document, dashboard, authorization, and effect work.
- Signed private-channel Slack requests and PR review publication now produce
  routine runs and artifacts. Their original focused regressions failed because
  no runs existed, then passed after integration.
- Event templates appear as installed routines without requiring an interval.
  The routines page, sidebar, conversation run history, and run-detail route use
  the trigger kind. Instructions can be edited for future runs; the detail page
  shows the snapshot used by a past run.
- Handler/model regression tests pass for activation, cancellation, replay,
  retry attempts, artifact retention, saved instructions, and reader permissions.
  Cancellation initially ignored event runs; artifact capture initially compared
  two clock domains. Both failures were reproduced before their fixes. Completion
  now snapshots the conversation document workspace, including unchanged evidence.
- Missing and paused Slack installations return private activation guidance and
  do not invoke the model. The legacy slash-command entry point is also being
  moved onto the activated template; its missing-run regression is reproduced.

Still required: finish and verify slash-command integration; broaden signed bot,
PR, and routine verification; review queued/revoked/failed work and project
isolation; inspect the rendered UI in both themes and narrow layouts; complete
all skill review loops and final simplification; run final local signoff and
image checks; push and update PR #619. Live Slack installation acceptance remains
unverified. These checkpoints are progress evidence, not completion signoff.

### Integration and browser evidence

- Full bot workflows: 64 examples, 0 failures. The metric catalog test now has
  a distinct editor who activates the routine and a linked reader who requests
  the investigation, so it exercises reader access instead of disabling the
  installation by demoting its owner.
- PR review integration: 3 examples, 0 failures, including signed webhooks,
  publication, retry, repository disable, and evidence-sharing settings.
- Slash commands now use the same activated Slack template and run ledger. The
  dedicated missing-run regression failed first and now passes with an artifact.
- Browser inspection uses `withLiveServer` and an ephemeral test database. The
  harness now includes production's hashed-asset middleware. A local Playwright
  capture checked real HTTP instruction saving/reloading, keyboard disclosure,
  prior-result immutability, and narrow overflow. Light/dark desktop and narrow
  screenshots are in ignored `scripts/local/routine-ui/`. Temporary browser
  capture instrumentation was removed from the integration spec afterward.
- Visual review changed cancellation reasons to neutral text and recent-run
  timestamps to execution time. Resizing screenshots now wait for the sidebar's
  existing CSS transition to settle rather than capturing its intermediate frame.
- Further failing regressions demonstrated that a direct schedule request could
  coerce an event template into an interval, and that a failed retry could inherit
  its predecessor's result artifact. Both now pass: event triggers stay fixed,
  and a run's result is stored atomically under its own run ID. Conversation
  document snapshots remain separate from the latest-result document.

The complete Agentic suite, remaining UI states and review loops still need
verification after these latest fixes. No final signoff, branch push, or PR
update has been performed during this integration work.

Latest watcher result: all 28 Agentic examples pass after the event-trigger and
per-run result fixes. The full bot workflow suite is being rerun against that
final artifact-writing change before the next review pass.

### Review loop continuation

The first Haskell and interface passes found and fixed further concrete gaps:
PR publication recovery, malformed stored-result handling, duplicate result
messages, artifacts lost on publication failure, scheduled instruction snapshots,
retention after conversation deletion, polling disclosure state, unnamed mobile
actions, and overlapping repository settings at 320px. Each behavioral gap has
an integration or browser regression. The permanent browser regression is
`e2e/tests/routines.spec.ts`; temporary capture instrumentation was removed.

Latest verification: 73 API/MCP examples, 22 CLI lifecycle examples, the focused
PR recovery/publication regression, and the permanent Playwright routine flow
pass. `make cli-build` passes. The UI report is
`plans/2026-10-29-routine-interface-review.md`. Final signoff is not started.

The broader automation audit is still open. In particular,
`BackgroundJobs.enhanceIssuesWithLLM` changes issue titles, recommendations,
criticality and error analysis outside the routine ledger. It must be evaluated
and integrated as a visible, controllable workflow before claiming that all
requested autonomous AI work uses routines. Query translation, conversation
labels and embedding/grouping internals are component operations rather than
standalone investigation workflows; do not mistake their function names for
separate agent executions or silently claim uninspected scope is complete.


### Automation audit and current verification

The branch now includes master commit `5f30465e3`. The rebase preserves its document
limits, document refresh handling, dashboard views menu, and Live Tail fixes.
Local routine changes were restored and their conflicts resolved. The build
watcher reports successful compilation. Server startup still fails with the
existing production database credentials; browser tests use an isolated test server.

Issue enrichment now requires the **Issue enrichment** template. Each eligible
issue uses its issue conversation, a source lease, and the shared run ledger.
The worker records instructions, assessment, artifacts, failures, retry attempts,
and usage. Duplicate deliveries skip completed work. Interrupted work resumes
its existing attempt. Pausing during a model call prevents metadata publication.

The regression first exposed an unactivated model call. It then exposed a dropped
recommended action and acceptance of malformed model output. The fixes preserve
the three-line description and reject invalid descriptions and severity responses.
The focused regression passes, including duplicate concurrent delivery, retry,
worker recovery, and pause during a model call. Before the malformed-output
extension, all 32 Agentic examples passed. All three PR integration examples
pass with shared routine authorization and usage accounting.

Repository analysis was inspected: it reads repository metadata and proposes
service mappings without an LLM call. Query translation, conversation titles,
embeddings, and pattern grouping remain implementation steps of existing product
operations. They are not separate investigation or review workflows. Existing
Discord and WhatsApp prompt handlers are interactive adapters outside this Slack
launch; this change does not introduce autonomous work through those adapters.

Still required: repeat the full affected test groups after these changes, repeat
the Haskell and interface review loops, and complete final simplification.
Then run local signoff and the required image check, push, and update PR #619.
Live Slack workspace acceptance remains unverified.


### Repeated review results

The hs-distill pass shared routine completion authorization and usage accounting
between PR reviews and issue enrichment. The issue model functions accept that
completion operation. This removes their direct dependence on application settings.
The pass also removed redundant wrappers and sequencing. Focused HLint reports
no hints in the five changed routine implementation modules.

The hs-evasion-review pass found malformed model descriptions and severity
responses accepted as valid metadata. A failing regression reproduced this.
The parser now requires the stated fields, severity values, and nonnegative counts.
The same pass checked caller identity, source types, member eligibility, lease
ownership, JSON boundaries, and interpreter preservation. No new warning
suppression, unsafe escape, or nested production interpreter was added.

The next pass found the routine result writer bypassing master's conversation
document limit. Its regression produced 501 documents against a limit of 500.
The writer now preserves the per-run result without exceeding the conversation
limit. Another regression reproduced a rejected Slack follow-up marked successful.
Rejected delivery now records failure and retains the generated artifact.

The hs-lob-review pass retained native disclosures, inline behaviors, and HTMX
forms and polling. It found no useful client-side demotion after the earlier
polling and layout fixes. The interface review clarified external reply delivery
and includes issue-enrichment activation. The browser flow passed in 19.9 seconds.

Current verification: 131 bot examples, 73 API/MCP examples, three PR review
examples, and 62 repository examples pass. The wiki browser initially used stale
frontend assets; rebuilding them and refreshing the compiled manifest resolved
that failure. Repository readiness now tests explicit routine activation.
`make post-css` passes. CLI lifecycle verification and final signoff remain.

No simplify skill is installed. The final manual simplification pass removed
redundant sequencing and reused existing domain operations. Further record/type
frameworks, extra source queues, and replacement client state would increase the
implementation without improving the requested behavior. They were rejected.


### Final focused verification and installation correction

The latest browser flow passes in 20.3 seconds against fresh frontend bundles.
All 35 error-pattern examples pass. The runtime-error pipeline now explicitly
activates enrichment and executes the queued jobs with the LLM effect interpreter.
It checks metadata changes and retained analysis artifacts. All 45 issue-page
examples and 22 CLI lifecycle examples pass.

The installed formatter was 0.21; CI pins 0.19. The changed files now use the
pinned formatter, and `make fmt-check` passes. The remote branch's additional
commit only sorts imports and formatting; those changes are preserved.

Current Slack references exposed an invalid inherited manifest field:
`metadata.event_subscriptions`. Slack's published v0.5.0 manifest schema rejected
it. The template now omits this field and passes that schema. The obsolete
metadata overlay was removed. Custom metadata does not require registration;
optional publisher schemas use top-level `events` and require org-wide deployment.
See [Slack metadata](https://docs.slack.dev/messaging/message-metadata/) and the
[published schema](https://github.com/slackapi/manifest-schema/blob/v0.5.0/schemas/manifest.schema.1.0.0.json).
The permanent installation regression failed before the correction and passes now.

That regression reads the shipped JSON file, so the integration fingerprint now
includes it. The CI definition and runner were updated together. `make ci-selftest`
passes, including declared inputs, fingerprints, capabilities, and release safety.
The native session method, stop event, and Agent view fields were checked against
current Slack references. Live app validation and sandbox acceptance remain unverified.

The repeated Haskell and interface passes have no remaining material finding in
the implemented diff. Final local signoff, image verification, branch push, and
PR update are still required. No merge or production deployment is requested.


### Pre-signoff checkpoint

All 132 bot examples pass after the manifest correction. The repeated reviews
and final simplification found no further material change to make. The reviewed
implementation is ready for local signoff and image verification. Results from
those commands and publication will be recorded in PR #619.

Live Slack validation remains a separate installation gate. No connected Slack
workspace tool is available in this session, and no live manifest was changed.

### Daylight-saving regression and final verification

The final audit reproduced a weekly routine losing its next execution across
a daylight-saving transition. The seven-day UTC search missed the next local
wall time. A regression failed before the fix. It covers the autumn offset
change and a nonexistent spring wall time, which must skip one week.

The search now covers fifteen days. Both cases pass, and all 34 Agentic examples
pass. Focused HLint reports no hints. The repeated distillation, evasion, and
locality reviews found no further material change in this fix. The final manual
simplification retained the existing bounded search and table-driven integration test.

The earlier signoff passed compilation, doctests, integration shards, CLI, unit,
UI, release, lint, frontend, and weeder checks. A complete browser rerun passed
161 tests with one existing skip, using two workers after parallel dashboard
timeouts. That signoff and its pending image were stopped before this fix.
The corrected commit requires a fresh final signoff and production image.
