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
