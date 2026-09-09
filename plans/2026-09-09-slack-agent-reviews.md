# Slack agent reviews

Scope: the Slack-agent worktree changes, including new incident storage, signed ingress, delivery workers, charts, and their tests.
Unrelated production-sweep and chart-research documents were excluded.

Each round applied these three instruction files:

- `/Users/tonyalaribe/.claude/commands/hs-distill.md`
- `/Users/tonyalaribe/.claude/commands/hs-evasion-review.md`
- `/Users/tonyalaribe/.claude/commands/hs-lob-review.md`

## Round 1

### hs-distill

| File | Reuse / combinators | Derives | Consolidation |
| --- | --- | --- | --- |
| `src/Models/Apis/Incidents.hs` | Use `Traversable` to validate wire timestamps through the delivery record. | Derived JSON, database, and traversal instances. | One parameterized representation serves wire and validated deliveries. |
| `src/BackgroundJobs.hs` | Existing monitor evaluation, widget signing, and notification dispatch remain shared. | New exception instance is derived. | Job declarations move to `BackgroundJobs.Types` to remove an import cycle. |
| `src/BackgroundJobs/Types.hs` | Existing job constructor names and JSON shapes retained. | Both JSON instances derived. | No duplicate worker implementation. |
| `src/Data/Effectful/Notify.hs` | Shared Slack transport and outcome classification. | Response JSON derived. | Image fallback reviewed again in round 2. |
| `src/Models/Apis/Issues.hs` | Shared insert SQL and transactional history seeding. | No new manual instance. | Removed session-lock acquisition/release across pooled calls. |
| `src/Models/Apis/Monitors.hs` | Shared transactional evaluation insert. | No new manual instance. | No required change. |
| `src/Models/Apis/Integrations.hs` | Existing installation lookup retained. | No new manual instance. | No required change. |
| `src/Pkg/Mail.hs` | Existing Block Kit builders retained. | No new manual instance. | Snapshot retention stays with message construction. |
| `src/Pages/Bots/Slack.hs` | Existing raw-body route and background queue reused. | Replaced handwritten payload decoder with derived tagged JSON. | Removed extra callback wrapper. |
| `src/Pages/Bots/Utils.hs` | Shared history-seeding transaction. | No new manual instance. | Removed ineffective pooled session-lock plumbing. |
| `src/System/Config.hs`, `src/Web/Routes.hs` | Existing environment and raw-JSON mechanisms. | No new manual instance. | No required change. |
| Haskell integration tests | Multi-step database, handler, and worker tests remain in Hspec. | No manual instances. | Shared signed-root fixtures use the actual signed ingress handler. |

### hs-evasion-review

Fixed two blocking findings:

1. `SlackTimestamp` had a derived decoder that bypassed its smart constructor. The wire delivery now decodes timestamps as `Text`. A derived traversal validates them before returning a domain delivery. Invalid stored values condemn the claim transaction and raise a derived exception.
2. `observeSlackRoot` accepted arbitrary identifiers and relied on a caller-authentication comment. It is now private. Public capture requires a saved signed receipt and checks author app, receiving app, workspace, channel, root correlation, installation, and timestamp.

No warning suppression, new manual instance, synthetic measurement, or untyped internal UUID was introduced.

### hs-lob-review

ECharts rendering calculations require JavaScript. No new client event handlers, visibility state machines, or hoisted Tailwind classes were found.
The Haskell regression tests exercise database transactions and handler flows; they are not single-expression pure tests.

## Round 2

### hs-distill

Re-read the changed transport, message builder, timestamp traversal, root capture, and their callers after round 1.
Two image-removal helpers duplicated traversal and silently discarded chart blocks. They now share `replaceImages` and the existing `overBlocks` traversal.
The replacement preserves block IDs and supports top-level and attachment block lists. No manual instance was needed.
Estimated reduction: about ten implementation/comment lines after adding the fallback notice and doctests; correctness is the primary benefit.

### hs-evasion-review

Fixed a missing state in the user-visible output: a rejected or oversized chart disappeared without explanation.
The retry now renders a chart-unavailable notice. Oversized URLs also emit an attention log.
Rechecked claim rollback, lease matching, immutable onset evidence, and signed-receipt capture. No additional blocking type compromise was found in these changes.

### hs-lob-review

The fallback is a Slack Block Kit context block. It needs no browser behavior.
Pure image-rewrite checks are Haddock doctests on the exported helper. Existing multi-step integration tests remain in Hspec.
No demotion or styling-locality fix was required.

## Round 3

### hs-distill

Re-read the final changed functions and their consumers. Removed the unused timestamp-operation alias and redundant exception import introduced during round 1.
The new representations retain derived JSON and traversal instances. There are no added handwritten instances.
No further reuse, combinator, or consolidation change was required.

### hs-evasion-review

Rechecked the final diff for warning suppression, sentinel readings, widened internal IDs, constructor abuse, and unsafe root-capture entry points.
No new suppression or manual instance was found. Root-capture tests reject a foreign app, workspace, channel, malformed timestamp, and conflicting timestamp; valid replay is idempotent.
The job test loads the persisted payload and uses the background dispatcher rather than only calling a helper.

### hs-lob-review

Found and fixed an export-scope mismatch: PNG bounds scaling claimed to apply only to unstacked charts, but omitted the stack guard.
The renderer now excludes stacked charts. A regression verifies that their configured bounds remain intact.
The numeric ECharts work remains at the JavaScript tier. No CSS, native HTML, or HTMX replacement was indicated.

## Validation and deployment boundary

Validation commands and final results are recorded in `2026-09-08-slack-agent-implementation.md`.
These rounds review the implemented incident and ingress foundation. They do not certify completion of the full Slack-agent plan.
Native Agent sessions, explicit user/project binding for agent tools, interrupted-run checkpoints, controlled live Slack acceptance, and the remaining plan gates are still required before deploying the complete agent.
No live Slack configuration or deployment was performed during these reviews.

## Validation follow-up

The standalone integration build exposed an undeclared `async` import that the combined development target allowed. Both tests now use the existing `UnliftIO.Async` dependency.
The signed-root fixture used a fresh UUID pool for every event. It now uses `runAsBase`, which retains the test-resource UUID sequence.
Unmatched incident-root observations emit an attention log instead of silently discarding the capture result.
Final native runs passed 11 incident, 26 monitor, and 18 workflow examples. The build, 1,536 doctests, 308 unit tests, and 922 frontend tests passed.
Weeder's existing findings, missing HLint/TimeFusion capabilities, and the concurrent workflow fingerprint change are recorded in the implementation log. These remain deployment gates.

## Event classification and assistant context follow-up

All three skills were applied in three further passes over `Pages/Bots/Slack.hs`,
the workflow regression, and migration 0156.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Reuse Aeson's parser and derive all wire codecs; keep the original event object instead of rebuilding it. | Separate human input, mentions, bots, edits, assistant starts, context changes, and unknown events. Reject malformed assistant timestamps and empty identities. | No client behavior added; the database/handler regression belongs in Hspec. |
| 2 | Remove obsolete metadata/profile records and the redundant effect constraint. | Fix silent owner-conflict handling: fail the job and retain its pending receipt. Verify that older context cannot replace newer context. | No tier demotion or styling-locality change is needed. |
| 3 | Re-read classifier, receipt persistence, worker dispatch, and signed-root SQL consumers. No further consolidation required. | Verify that raw event preservation retains fields required by root authentication; context storage grants no project authority. Existing legacy user-message authorization remains an open plan requirement. | The regression checks replay, storage, chronology, and owner conflicts; no pure one-expression spec was added. |

This implements compatibility context storage, not the complete native Agent
session lifecycle. Slack's current lifecycle uses `agents.sessions.setStatus`
and `agent_session_stopped`; progress, stop handling, and explicit user/project
binding remain required. See [Slack's session guide](https://docs.slack.dev/ai/agent-sessions/).

## Installation authorization follow-up

Three passes applied all three skills to the installation model, Slack handlers,
routes, settings/onboarding links, and workflow regression.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Use a typed UUID request ID, derived row decoding, and `renderSimpleQuery`. | Replace the caller-selected project ID in OAuth state with a server-stored, expiring request. | Keep the installation redirect on ordinary anchor links. |
| 2 | Consolidate the two copies of Slack OAuth scopes in the start handler; remove the now-unused renderer configuration argument and nested `do`. | Move the callback under cookie authentication. Match the initiating user and recheck active admin membership before consuming the request. | No client script or styling indirection was introduced. |
| 3 | Re-read all changed consumers, including the unchanged installation side effects. No further derive or reuse changes required. | The database regression covers wrong users, expiry, revocation, replay, and concurrent consumption. No warning suppression or manual instance was added. | The regression requires database state transitions and concurrency, so Hspec is appropriate. |

This secures installation authorization. It does not yet establish personal Slack
identity bindings or authorize the legacy workspace-selected investigation path.

## Personal linking and investigation entry follow-up

Each requested skill ran in three passes over `Integrations.hs`, `Issues.hs`,
`Pages/Bots/Slack.hs`, routes, migrations 0158–0160, and the workflow tests.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Reuse signed receipts, `slackApi`, derived database/form codecs, and UUID v5. No hand-written instances. | Personal identity comes from the stored signed event; the browser only selects an authorized project. Private links are sent with `chat.postEphemeral`. | Account linking uses native forms, labels, and a select. No JavaScript is required. |
| 2 | Keep Slack coordinates as a JSON tuple when generating a project-scoped conversation ID. Reuse the existing HTTP recorder and LLM fixture. | Recheck live account, project, installation, and membership state. Fix thread creation to recheck access after principal resolution. Preserve the thread project when the user's default changes. | Styling remains inline at each element. The multi-step DB/handler tests stay in Hspec. |
| 3 | Re-read changed functions and their unchanged AI/history consumers. No further consolidation required. | Validate incoming human timestamps and identities before queuing. Test expiry, replay, concurrent consumption, foreign workspaces, identity reassignment, and revocation. Legacy slash/actions authorization, per-tool rechecks, and conversation-role handling remain open plan gates. | The linking UI remains at the HTML tier; no behavior was moved into a higher tier. |

The HTTP recorder returns `{}`, so the private-prompt regression deliberately
checks that the worker treats it as a failed Slack acknowledgement and keeps the
receipt pending. It verifies the exact method, recipient, channel, and stored
single-use link without sending a live Slack message. The authenticated form then
binds the identity, and the authorized replay creates an investigation conversation.
The final database pass added an index for the incident-thread lookup performed
on incoming messages. CI must cover that final migration as well.

## Investigation access checkpoints

Three passes applied all three skills to the AI loop, bot query pipeline and
callers, history model and callers, and the workflow/agentic regressions.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Reuse the live Slack principal resolver; derive the access-denied exception. Require an explicit access mode at bot-query call sites. | Preserve the initiating Slack and Monoscope identities through model/tool execution; deny before and after data use and before delivery. | No client behavior added; provider-interposition tests belong in Hspec. |
| 2 | Remove a duplicate access check immediately before the loop's first check. | Fix the existing global conversation-history lookup: the function now requires a project ID, and every caller supplies it. Verify the selected installation still belongs to the receiving workspace. | History-isolation checks require database setup and remain with the integration flow. |
| 3 | Re-read all changed functions and their callers. No new manual instance or warning suppression. | Tests revoke access during both a tool-calling model response and a final model response. Exactly one denying DB query runs afterward; the tool query does not run. History tests use the same conversation ID in two projects. | No styling or behavior migration needed. |

`ServiceAccess` retains each caller's existing authorization policy. The legacy
Slack slash-command path still uses it and remains a deployment gate until signed
requests and personal authorization are applied there. Signed-event investigations
always construct `SlackAccess` from the resolved principal.


## Signed form ingress

All three requested skills ran in three passes over the Slack handlers, public
routes, and workflow regression.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Extract the existing signature guard and reuse derived `FromForm` decoders. | Verify original bytes before parsing slash commands, actions, and external-option requests. Missing secrets fail closed. | No client-side behavior added. |
| 2 | Derive `Accept` via `FormUrlEncoded`; retain only the necessary raw-byte `MimeUnrender` implementation. | Read all three route declarations and server bindings. External options must accept Slack's form envelope, not JSON. | Keep the multi-step handler regression in Hspec. |
| 3 | Re-read the complete wrapper, signature guard, route bindings, and decoder consumers; no further consolidation. | Check tampering, absent signatures, expiry, malformed signed forms, unavailable secrets, and escaped values. Dispatch count proves rejected forms never reach the handler. No suppression or hand-written derivable instance. | No styling or behavior-tier changes. |

The signature boundary is now shared across Slack ingress. Personal authorization
for legacy slash commands and dashboard actions remains unfinished; signed payloads
do not by themselves grant Monoscope project access.


## Personal authorization for slash commands

Three passes applied all three skills to the command handler, changed model
queries, fixtures, and workflow/Slack regressions.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Reuse `resolveSlackPrincipal`, `requireAgentAccess`, and the existing permission enum. Remove workspace-fallback plumbing. | All command branches require a live personal binding. Investigations carry `SlackAccess` into background work; channel changes require admin. | No client behavior added; private responses use Slack's existing message format. |
| 2 | Reuse the dashboard query helper with a typed project ID; remove newly unused imports. | Scope dashboard reads and channel updates to one project. Fix webhook/channel desynchronization when changing the default channel. Update every caller of the changed signatures. | Keep the multi-step database authorization flow in Hspec. |
| 3 | Re-read the complete command branches and model consumers. No manual derivable instances, string packing, or warning suppressions. | Regression covers unlinked users, view/edit denial, revocation, dashboard isolation, and another project's channel/webhook preservation within the same workspace. Dashboard action authorization remains a separate open gate. | Tests use an explicit linked-user fixture; unlinked event fixtures remain unlinked. Fix the missing-dashboard test to fail if the handler unexpectedly succeeds. No frontend tier changes. |


## Dashboard action authorization and metadata

Three passes applied all three skills to the modal entry point, action decoder,
widget selection and sharing, reused dashboard loaders, and the recorded-HTTP
workflow regression.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Derive `SlackDashboardContext` codecs and reuse the principal resolver, access guard, and signed widget URL generator. | Remove the packed channel/project/template/URL tuple. Bind the modal to its requesting user and project; recheck access on every action and before output. | Slack's existing selection events handle interaction; no client script added. |
| 2 | Move the existing template catalog and schema loader into `Models.Projects.DashboardTemplates` for reuse without a page-module cycle. Remove the unused option text field and duplicate obsolete picker. | Load dashboard IDs only inside the authorized project. Accept widget definitions only from the saved schema or known templates. Regenerate chart URLs at preview and submission. | Keep one widget selector after dashboard selection and a stable block/action ID for updates. |
| 3 | Re-read all changed functions and their unchanged dashboard/URL consumers. JSON instances remain derived; no new suppression. | Widget options identify exact definitions, so changed widgets invalidate stale selections. Tests reject old packed metadata, a different owner, foreign dashboard IDs, and revoked users without HTTP output; valid preview/share paths emit recorded requests. | The multi-step database and handler flow stays in Hspec. No behavior-tier or styling indirection introduced. |


## Role-preserving conversation history

Three passes applied all three skills to bot query dispatch, persisted history,
Slack backfill decoding, and workflow regressions.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Reuse `runAgenticChatWithHistory`; replace the redundant `BotThread` wrapper with the existing typed conversation ID. Remove the string-formatting helper and duplicate user insertion. | Send historical messages with their original roles, never appended to the system prompt. Save the complete raw assistant answer, including explanation-only replies. | No client-side behavior added; the worker/model/database flow belongs in Hspec. |
| 2 | Keep Slack wire decoding derived. Reuse `ChatRole` and existing app identity configuration. | Only this Slack app's messages become assistant history. Bound backfill before the triggering message and filter its exact timestamp. Select the latest 200 DB messages, then restore chronological order; timestamp individual inserts rather than assigning an entire seed transaction one timestamp. | Keep the multi-turn and long-history assertions in their existing workflow suite. |
| 3 | Re-read changed functions and the unchanged history-aware AI loop and other bot callers. No manual instance or warning suppression. | Record two worker turns and inspect exact model roles/content, full saved answers, one current question, and absence of historical injection text from the system prompt. Verify retention of the newest 200 messages and project isolation. | No styling or behavior-tier changes. |

This preserves user/assistant conversation context. Durable tool-message replay,
run checkpoints, and pagination beyond Slack's first backfill page remain plan
work; the bounded model window does not delete stored database messages.


## Complete Slack backfill pagination

Three passes applied all three requested skills to the paging loop, shared thread
backfill behavior, derived response types, and worker regressions.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Derive the response envelope and cursor metadata. Reuse the access guard and the existing transactional history seeder. | Follow every returned cursor while retaining the event-time boundary. Reject `ok=false`, missing message arrays, repeated cursors, and incomplete pages without cursors. | No client behavior added. |
| 2 | Extract the existing GET-body test interposer for reuse in linked-worker and multi-page fixtures. | Stop on failed backfill before any model call or user-message insertion. Do not swallow seeding failures. All page failures retain an empty history for retry. | Multi-page, revocation, and retry checks stay in the database/worker Hspec flow. |
| 3 | Re-read the changed functions and unchanged seeding/receipt consumers. No manual instance or warning suppression. | Require explicit `AgentAccess` and project inputs to paging; revalidate before and after each request. Tests cover second-page API failure, repeated/missing cursors, mid-fetch revocation, and a successful replay of the same receipt followed by another turn. | No styling or behavior-tier changes. |


## Native session processing status

Three passes applied all three requested skills to signed-event investigation
startup/cleanup, the status request types, and recorded-HTTP worker tests.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Derive the status enum and request record JSON; reuse `slackApi` and the existing access guard. | Use the current `agents.sessions.setStatus` API with explicit channel and thread. Require a successful startup acknowledgement before work. | Use Slack's native loading state; no custom client code. |
| 2 | Generalize the existing HTTP response fixture instead of duplicating effect forwarding. Use `bracket_` for startup/cleanup. | Recheck project access before startup. Test cleanup on API-page failures and mid-fetch revocation, and reject failed startup before the model runs. | Keep lifecycle and failure checks in the worker integration flow. |
| 3 | Re-read all modified functions and their unchanged receipt/history consumers. No manual instance or warning suppression. | Fix cleanup-error replay: log an unsuccessful reset without rerunning a completed answer. Verify the receipt's replay emits no requests and no second answer. Stop events, concurrent-run coordination, and durable status reconciliation remain explicit plan gates. | No styling or behavior-tier changes; native status is scoped to the existing thread. |


## Durable investigation stops

All three requested skills were applied in three passes to the stop migration,
receipt ingress/worker, access types, backfill exception handling, and tests.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Derive the stop payload and exception instances. Reuse the timestamp validator, access guard, async race, and existing thread table. | Connect the previously incomplete storage/access additions to signed ingress and the worker. Interrupt blocked work rather than only checking after model completion. | Use Slack's native stop interaction and threaded confirmation; no browser behavior added. |
| 2 | Remove a redundant MVar import; use Relude and the existing recorded-HTTP/provider fixtures. | Authorize from the stored signed receipt against current membership, account, project, and workspace installation. Keep the cutoff monotonic and validate timestamps before storage. | Keep the multi-step cancellation, replay, and authorization checks in Hspec. No styling indirection or client-tier escalation. |
| 3 | Preserve backfill exceptions with `onException`, removing the broad exception-to-503 conversion. Re-read changed functions and their existing callers; no manual instance or warning suppression. | Fix cancellation swallowed during history retrieval. Regress blocked model cancellation, stop during backfill, ignored unlinked users, old-stop ordering, successful newer work, and completed-receipt replay. | Recheck the final diff: no JS, hyperscript, CSS, or Lucid behavior changes; no simple pure assertions needing relocation. |

Remaining plan limitations: concurrent questions can still race native status
updates; cleanup and confirmation delivery lack durable reconciliation. This
change does not establish live Slack acceptance or authorize deployment.


## Concurrent Slack investigations

Three passes applied all three skills to worker dispatch, the thread lock, and
concurrent workflow regressions.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Reuse PostgreSQL advisory locks, `withResource`, `withTransaction`, and `race_`; derive the busy exception. | Do not reuse the scheduler's separately pooled session-lock calls. Pin the lock transaction to one checked-out connection for the worker's lifetime. The regression first failed because a duplicate worker completed concurrently. | No browser behavior or custom Slack interaction added; use the existing worker and native status flow. |
| 2 | Decode event kind once and pass it into dispatch. Keep the lock helper local to the worker. | Cover startup, cleanup, confirmation, and receipt completion under the same lock. Recheck pending state after acquisition. Busy workers fail for retry; stop events bypass the investigation lock. | Keep the multi-step concurrency and retry flow in Hspec, using the existing recorded-HTTP fixture. |
| 3 | Re-read final dispatch and unchanged authorization, cancellation, and history consumers. No manual instance, warning suppression, or redundant state table. | Check lock connection liveness and interrupt work on loss. Tests terminate only the lock backend in their isolated fixture database, verify no model answer is sent, then retry successfully. A separate channel proceeds while the first worker is blocked. | No JS/CSS/Lucid changes, styling indirection, or misplaced simple pure assertions. |

This coordinates live workers. Ordered scheduling of delayed events, durable
model/tool checkpoints, and reconciliation across ambiguous external deliveries
remain separate plan work.


## Agent installation scopes and manifest

All three requested skills were applied in three passes to installation storage,
OAuth decoding, the investigation capability check, integration copy, and fixtures.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Extend the existing derived `TokenResponse` and `SlackData` records and token upsert. Reuse the admin reconnect route and private Slack response method. | Add the missing Agent scope to OAuth, but persist returned grants rather than infer them from the requested list. Keep unknown historical grants nullable. | Use the existing reconnect link and inline paragraph styling; no new browser behavior. |
| 2 | Centralize the required-scope predicate for the worker and integrations page. Normalize comma-separated OAuth scopes at the boundary. | Prevent native work when required grants are missing or unknown. Preserve webhook/channel credentials and require current project access before the private upgrade prompt. | Keep capability information beside the existing connection status and reconnect action. |
| 3 | Re-read scope storage, positional row decoding, notification consumers, and installation call sites. All JSON/row instances remain derived; no warning suppression. | Regression first reproduced the old-install model call. Then verify no model/status call, unchanged webhook, and OAuth round-trips for partial/full grants. The manifest is a template, not proof of live Agent availability; current app-home/context/title support remains incomplete. | The OAuth/database/worker flow stays in Hspec. Static native prompts add no client glue or styling indirection. |


## Native context and title events

All three requested skills ran in three passes over classification, signed ingress,
context persistence, authorized session updates, and workflow regressions.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Derive both wire records and reuse timestamp validation and stored signed receipts. | Add explicit context/title constructors rather than leave subscribed events in the unknown branch. Preserve opaque navigation objects without interpreting them as project authority. | Use native Slack events; no custom browser behavior. |
| 2 | Extend the existing authorized session-update query for titles instead of duplicating its membership joins. Decode event kind once with the envelope workspace. | Give titles explicit columns and timestamp ordering; isolate navigation by workspace/channel/user, including newer empty context. Require title/envelope workspace agreement before queueing. | Keep concurrent/replayed DB and handler flows in Hspec; no simple pure tests displaced from their functions. |
| 3 | Re-read new types and existing stop, authorization, and conversation consumers. No handwritten instance or suppression. | Fix misleading negative-test envelopes, including the earlier malformed-stop fixture. Test malformed timestamps, workspace mismatch, stale and revoked title changes, context clearing/isolation, unchanged stop state, and zero conversation/HTTP output. | No JS/CSS/Lucid changes, client-tier escalation, or styling indirection. |


## Messages-tab onboarding

All three requested skills were applied in three passes to App Home decoding,
worker locking/dispatch, link creation/delivery, and personal-link regressions.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Derive the App Home record; reuse signed-receipt link creation and the existing link text/transport. | Handle Messages-tab visits explicitly without converting them into investigation messages. SQL only permits App Home link requests for the Messages tab. | Use the native tab event and existing private link flow; no custom browser behavior. |
| 2 | Generalize the existing pinned-connection lock for onboarding while preserving investigation lock keys. | Serialize opens by workspace/conversation/user. Add explicit delivery time instead of treating receipt completion as proof of delivery; only an acknowledged send can suppress a repeat prompt. | Keep the full opening/retry/expiry/link-consumption flow in Hspec. |
| 3 | Remove the now-unused workspace lookup wrapper and redundant test import. All new codecs remain derived, with no suppression. | Verify rejection retry, replay, repeat-open suppression, another user's independent prompt, expiry renewal, successful personal linking, and zero AI conversations. A missing installation fails instead of marking an undelivered prompt complete. | No JS/CSS/Lucid changes or styling indirection; other tabs deliberately retain their event without onboarding. |


## Manual error resolution and incident delivery

All three requested skills ran in three passes over the resolution model,
handler, activity trigger, error-state consumers, message builder, and regression.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Reuse the incident transaction/outbox, Slack blocks, and derived error-pattern codecs. Add the actor to the existing record and schema. | The regression reproduced a resolved error with an active incident episode. Commit state, operator activity, and existing-root delivery intent together; roll back on outbox failure. | Keep the existing HTMX resolution action and server response; Slack receives native message blocks. No new client behavior. |
| 2 | Share Slack escaping and use the existing `hostPath` helper. Remove the handler's separate activity insert. | Distinguish manual and automatic resolution with an explicit actor column. Check current membership/account/project and edit-or-assignee permission in the transaction; lock the pattern and membership row. Clear the actor on recurrence and automatic transitions. | Keep complex handler/database/concurrency/notification assertions in Hspec. Styling remains inline and unchanged. |
| 3 | Re-read the final diff and the aggregated error-pattern row decoder. No handwritten instances or suppressions were added. | Preserve manual attribution on unchanged-state calls; preserve the automatic activity clock when a previous `resolved_at` remains. Test stale transitions, revoked membership, rollback, concurrent clicks, legacy errors, recurrence, and derived row decoding. Existing roots receive replies/updates; resolving a legacy error does not invent a root. | Recheck all response branches and unchanged resolution controls: no JS/hyperscript escalation, hoisted classes, or misplaced simple pure assertions. |

| File | Reuse / combinators | Derives | Consolidation / bloat |
| --- | --- | --- | --- |
| `src/Models/Apis/Incidents.hs` | Existing transaction helpers, incident event recorder, `forM` and `find`. | Result uses stock `Eq`/`Show`; IDs retain domain types. | No second outbox or transport. |
| `src/Models/Apis/ErrorPatterns.hs` | Existing state/ingestion/decay queries. | Extended record retains derived JSON and row codecs. | Unchanged-state writes become no-ops. |
| `src/Pages/Anomalies.hs` | Existing route, authorization, toast/rendering, and URL helper. | No instances added. | Remove the separately committed state/activity operations. |
| `src/Pkg/Mail.hs` | Existing Slack section/context builders and shared escaping. | Existing derived `SlackPayload`. | One payload serves both resolution reply and root update; existing worker retains the original snapshot. |
| `test/integration/IncidentDeliverySpec.hs` | Existing fixture, handler, clock, and notification interpreter. | No fixture codec instances. | One multi-step lifecycle regression exercises the relevant boundaries. |

No further LoC reduction is proposed. Remaining plan work includes connecting
runtime-error alert creation/reminders to this outbox, truthful missing-telemetry
handling, and live Slack acceptance. The model regression seeds an issue episode;
it does not claim the legacy runtime-error notification path already creates it.


## Stable runtime-error incident identity

Three passes applied all three requested skills to source identity, event storage,
manual resolution, snapshot retention, and the lifecycle regression.

| Pass | hs-distill | hs-evasion-review | hs-lob-review |
| --- | --- | --- | --- |
| 1 | Extend the existing source sum and episode/outbox records. Derive JSON and the Hasql value codec via `Aeson`; reuse the existing source-lock and delivery code. | Error-pattern IDs are a separate constructor and database source kind. Issue IDs can change during escalation and must not become a new episode identity. Validate project, runtime issue type, and matching error hash. | No client interaction added. The existing resolution route and native Slack blocks remain sufficient. |
| 2 | Reuse the same resolution recorder for legacy issue sources and new error sources. | Lock the error row before the advisory source lock, matching resolution's lock order. Reject new active events for resolved or merged patterns while retaining replay deduplication. Preserve the episode's original issue link. | Keep lifecycle, transport, and authorization assertions together in Hspec; no pure one-line Spec tests added. |
| 3 | Keep missing issue links explicit as `Maybe`, and reuse the existing message builder with a project fallback. | Handle a deleted original issue without silently dropping the error episode from resolution. Stop restoring obsolete action links from the initial snapshot; retain onset and chart, and use current links. Test recurrence, merged suppression, wrong project/hash, absent initial issue, replay after closure, and deleted-issue resolution. | The fallback is labeled “Open project issues”; no custom JS, hyperscript, styling binding, or fabricated investigation action. |

| File | Reuse / combinators | Derives | Consolidation / bloat |
| --- | --- | --- | --- |
| `src/Models/Apis/Incidents.hs` | Existing query helpers, source/event recorder, locks, and resolution transaction. | Source JSON and Hasql codecs are derived. | One resolution loop handles both source types; no parallel delivery table. |
| `src/Pages/Anomalies.hs` | Existing URL and response helpers; map a present issue ID to its URL. | No instances. | One base URL supplies the incident or project fallback. |
| `src/Pkg/Mail.hs` | Existing message builder and `maybe`. | Existing derived payload. | Remove action blocks from retained historical snapshots. |
| `test/integration/IncidentDeliverySpec.hs` | Existing database, clock, handler, and notification fixtures. | No fixture instances. | One multi-step scenario verifies identity across changing/deleted issue records. |

No further LoC reduction is proposed. The legacy notification claim/dispatch
paths still need an atomic outbox integration. This prerequisite does not claim
that normal ingestion already creates error-source episodes.
