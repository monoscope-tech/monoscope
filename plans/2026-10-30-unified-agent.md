# Unified Monoscope agent

Status: implementation in progress, 2026-10-31. Open-PR reconciliation is the first milestone.
This document controls future agent work on this branch.
It combines the Slack consolidation work and the Slack coworker proposal.
It incorporates the requested default of autonomous background work without repeated permission prompts.
This document does not claim that unfinished features or live Slack acceptance are complete.

## Product contract

Monoscope investigates telemetry, uses project tools, remembers team knowledge, and follows up on its work.
Slack, the web app, schedules, and project events provide inputs to the same system.
Every execution has a project, an authorizing member, a conversation, a run, and retained evidence.

Background routines act automatically within their configured authority.
Users do not approve every tool call or routine run.
An enabled routine grants continuing authority until someone pauses it or changes its policy.
The authorizing member's current role limits that authority.
Slack identity linking establishes identity and project access. It does not replace routine authorization.

The first launch covers reliable requested investigations and autonomous project operations.
Optional channel investigations, recovery follow-ups, and Slack search extend the same system later.
Code changes and deployments remain outside this scope.

## Sources and current baseline

- [Slack consolidation plan](2026-09-30-slack-agent-consolidation.md): historical decisions, regressions, and implementation evidence.
- Coworker proposal: `../monoscope/docs/plans/2026-10-30-slack-coworker-agent.md` in the sibling checkout.
  Its phases cover actions, native Slack features, Slack search, memory, and proactive work.
  The sibling file is currently an untracked proposal, not a separate implementation to merge.

This branch already contains:

- Signed Slack receipts, thread locks, checkpoints, reply reservations, history reconciliation, and stop handling.
- MCP-derived tool discovery, schema loading, and a shared tool adapter supplied by the application host.
- Explicit tool policies, current member authorization, member attribution, and metric catalog discovery.
- Activated event routines for Slack, PR reviews, and issue enrichment.
- Run history, instruction snapshots, retained artifacts, and a conversation detail panel.
- Versioned project wiki storage with member and agent authors.

Slack event tool calls retain the conservative Observe ceiling. Administrator actions remain unavailable until their authority and disclosure work is complete.
The new authority policy replaces those limitations after the execution and recovery tests pass.
Current native Slack behavior still needs controlled workspace acceptance.
The silent private-channel mention needs a traced reproduction. Metric discovery alone does not explain that failure.

## Consolidation rules

1. Keep one capability catalog for API, MCP, CLI server operations, and agent tools.
2. Keep one operation implementation and authorization contract for each capability.
3. Keep one agent executor for interactive and background conversations.
4. Keep one run ledger for schedules, Slack, PR reviews, enrichment, and proactive work.
5. Keep one authority evaluator for every agent entry point.
6. Keep one conversation and artifact interface across entry points.
7. Keep Slack protocol and delivery code in the Slack adapter.
8. Remove replaced code in the same implementation slice.

Consolidation means fewer implementations, not fewer domain types or weaker guarantees.
Transport receipts, tool checkpoints, and run summaries represent different facts.
They can reference one run without duplicating their records or losing their recovery guarantees.
Do not build a universal workflow framework before a second real caller needs it.

## Shared architecture

```text
Slack / web / schedule / project event
                 |
       receipt or trigger adapter
                 |
    conversation + routine + run
                 |
 shared executor and authority evaluator
                 |
     capability catalog and dispatcher
                 |
        existing domain operations
                 |
  checkpoints + results + artifact snapshots
                 |
        Slack / web / GitHub delivery
```

### Capabilities

Use the existing OpenAPI/MCP catalog as the baseline.
Add agent-only capabilities through the same catalog contract.
These include incident context, repository evidence, conversation artifacts, and wiki access.
Do not add fake REST routes solely to register agent tools.

Each capability describes its schema, project scope, required role, effect, and retry behavior.
Effects describe reads, project changes, external publication, and administration.
HTTP methods do not determine effects. `POST /events/query` is a read.
MCP annotations and agent discovery derive from the same descriptors.
Transport-specific serialization stays outside domain operations.

Keep task search and schema loading so each model turn receives a small tool set.
Move existing tools onto the shared dispatcher without changing their result meaning.
Delete duplicated definitions and dispatch cases after their replacement tests pass.

The current `ProjectTools` adapter preserves the caller's effect interpreters.
Extend that adapter instead of introducing service credentials or a second interpreter stack.
Keep member identity through API handlers and activity records.
CLI login, local configuration, completion, and terminal rendering remain client operations.

### Authority and autonomous execution

Use a typed execution policy with three modes:

| Mode | Behavior |
| --- | --- |
| Observe | Read evidence and store conversation results. Do not change project resources. |
| Autonomous | Execute enabled capabilities automatically within current member permissions. |
| Review selected actions | Execute ordinary capabilities automatically. Request approval for explicitly selected capability classes. |

Autonomous is the product default for newly enabled background routines.
Do not impose mandatory approval solely because an operation changes state or requires an administrator.
An administrator-owned routine can execute enabled administrative capabilities automatically.
A reader cannot gain write authority through Slack, a prompt, or an administrator's conversation reply.
External destinations and resource scope remain part of the routine configuration.

Resolve authority from the actor, routine policy, operation descriptor, and current project access.
Instructions, Slack history, wiki content, and tool output cannot grant authority.
Retain operation-level preconditions from the existing product APIs.
Before each tool and delivery boundary, evaluate current authority and cancellation.

Replace scattered `actionsAllowed`, invocation-mode exceptions, and confirmation branches with this evaluator.
Invocation mode still describes execution context. It must not become an independent authorization system.
Keep routine owner authorization distinct from the requesting member's identity.
Interactive requests use the current member. Scheduled work uses the routine's authorizing member.
Slack event work also retains the linked requester and current membership requirements.

Persist the effective policy snapshot with each run for audit.
Apply current restrictions before each operation. A historical snapshot cannot preserve revoked access.
If configuration changes increase authority, apply that increase to future runs.
If configuration changes reduce authority, apply that restriction to active work immediately.

Migrate existing installations without silently increasing their authority.
Preserve existing read-only behavior until an editor explicitly adopts the autonomous policy.
New installations show their automatic actions once during activation.
Keep pause, stop, policy changes, and action history accessible in the existing routine UI.

### Execution, recovery, and optional review

Keep the existing conversation worker and durable checkpoints.
Introduce a persisted operation identity for every mutating tool step.
Retries reuse that identity rather than creating another action.
Record proposed arguments, actor, effective policy, outcome, and external publication references under the run.

Use operation-specific recovery:

- For changes inside one database, commit the change and its execution record in one transaction where possible.
- For external APIs, use supported idempotency keys or reconcile the observed result before retry.
- For ambiguous outcomes, retain an explicit unknown state and expose it in the run.
- For operations without safe retry or reconciliation, stop automatic replay and report the unresolved result.

A claimed row alone does not establish exactly-once effects.
Do not claim that every external operation supports exactly-once execution.
Independent threads can proceed while one operation awaits reconciliation.

Optional approvals extend this same operation record.
Do not create a separate Slack action executor or approval-only run ledger.
Bind approval to the operation, immutable arguments, project, requester, expiry, and configured approver policy.
Slack and web controls submit decisions to the same queue and evaluator.
Approval never bypasses the execution actor's current role.
Repeated clicks cannot create another operation.
Persist waiting work without holding a worker or thread lock during human review.
Resume through the existing executor after a decision.

### Results, conversations, and memory

Keep complete evidence in existing checkpoints and run artifacts.
Give the model bounded result summaries, explicit truncation markers, and pagination or artifact references.
Keep complete MCP envelopes out of model history.
The combined branch now caps model tool messages at 8,192 characters and retains complete envelopes as evidence.
Keep that regression while consolidating the remaining operation responses.
Use existing query formatters and bounded operation responses before adding new formatters.

One conversation contains the user inputs, tool outcomes, decisions, and replies across Slack and the web app.
Run artifacts retain immutable snapshots. Conversation documents remain the editable workspace.
Project wiki pages hold durable knowledge across conversations.
These records have different lifetimes and must not become interchangeable stores.

Add wiki read, search, and write capabilities over `ProjectMemory`.
Use its existing revision checks, path types, limits, and agent author identity.
Bind wiki attribution to the existing agent attempt, not a cast routine-run ID.
Autonomous writes follow the same policy as other project changes.
Optional review uses the same operation record and controls.
Do not add a separate exception that lets routines append to arbitrary pages.
Load relevant titles and bounded bodies on demand. Wiki content remains evidence, not execution authority.

### Slack experience and delivery

Treat Slack as an input and output adapter for the shared conversation.
Keep mentions, DMs, slash commands, incident threads, and native Agent sessions.
Add suggested prompts, generated titles, and a message shortcut through existing signed receipt handling.
Respect a user's renamed title and existing project bindings.
Unlinked users receive a private account-link action.
Missing or paused installations receive private activation guidance.

Keep the durable reply publication path as the delivery authority.
Add streaming only after interruption and ambiguous-start recovery work in a real workspace.
Slack documents metadata on `chat.stopStream`, but not in the `chat.startStream` argument list.
Do not assume that saving the returned timestamp closes the lost-acknowledgement window.
Keep progress and stream presentation separate from stored execution state.
Optional approvals can use Slack cards or the existing web conversation panel.

### Proactive work and Slack evidence

Model automatic incident investigations and closing updates as event routines.
Channel opt-in selects a destination and trigger. It does not create another background scheduler.
Use the same policy, executor, run ledger, cancellation, and delivery reconciliation.
Default proactive analysis to Observe. Projects can explicitly enable autonomous remediation.
Do not infer recovery from missing errors or missing measurements.

Add Slack search as another evidence capability after app eligibility and user authorization work.
Use the requesting user's token, encrypted with the existing secret mechanism.
Never substitute another user's token.
Token revocation must remove that user's Slack capabilities.

Enforce destination visibility before Slack content enters model context.
Current-channel evidence is the initial channel-safe scope.
Broader evidence requires a demonstrated audience rule, including Slack Connect and restricted conversations.
The user's private DM with Monoscope can use their opted-in personal scope.
Do not reuse private evidence in another audience's conversation or shared artifact.
Keep Slack write tools excluded from the remote MCP catalog. Publish through Monoscope's audited delivery path.

## Implementation sequence

First complete the [open-PR integration prerequisite](2026-10-30-agent-pr-integration.md).
The existing impact-agent stack already supplies wiki tools, attempts, receipts, usage, and retained artifacts.
Reuse that work instead of implementing these systems again.
Reconcile the divergent Haskell runtime before adding another executor or worker protocol.
Preserve specialized review and fix records through typed links to the shared run ledger.
The imported code does not extend Slack authority to code changes or deployments.

Each slice includes implementation, replacement cleanup, and focused regression evidence.
Complete each slice before final signoff. Do not queue expensive signoff during implementation.

| Slice | Work | Removal and reuse | Required evidence |
| --- | --- | --- | --- |
| 1. Consolidate capabilities | Inventory API/MCP/CLI coverage. Unify descriptors, policy metadata, discovery, dispatch, and bounded model results. | Remove duplicate read definitions and dispatch cases. Reuse metric catalog and query formatters. | Catalog coverage, equivalent results, project isolation, preserved effects, bounded large responses. |
| 2. Consolidate authority | Add typed policies and member-scoped autonomous execution. Add activation UI and conservative migration. | Replace hard-coded write bans and scattered mode predicates. Preserve event-run cancellation and owner checks. | Automatic editor write, automatic enabled admin action, denied reader write, revoked access, pause, stop, policy reduction, unchanged legacy authority. |
| 3. Make actions recoverable | Persist operation identities and outcomes. Add transactional and external recovery where supported. | Extend tool checkpoints and the existing run ledger. Reuse domain mutation handlers and member activity records. | Crash before execution, crash after mutation, duplicate job, repeated tool call, external timeout, ambiguous outcome, correct attribution. |
| 4. Complete requested Slack work | Trace the silent private-channel mention. Add prompts, titles, and shortcut intake. | Reuse signed receipts, identity linking, conversation binding, and publication reservations. | Linked/unlinked visible outcomes, automatic requested action and result, retry without duplicate effects, preserved user title, incident continuity. |
| 5. Consolidate shared memory | Integrate the impact stack's wiki capabilities and relevant knowledge discovery. | Reuse versioned wiki storage and agent attempt identity. Remove duplicate schemas, dispatch, and memory-specific authorization modes. | Save knowledge automatically, retrieve it in another thread, stale revision, cross-project denial, correct author and run evidence. |
| 6. Add selective review | Add optional per-class review and web/Slack decision controls. | Extend the same operation state and executor. No parallel approval worker. | No prompts in Autonomous mode, selected review waits, expiry, unauthorized click, repeated click, revoked actor, pause during wait. |
| 7. Add streaming | Extend delivery records for stream start, append, completion, and reconciliation. | Reuse publication claims and progress events. Remove replaced snapshot presentation after acceptance. | Real Slack start/append/stop, lost acknowledgements, worker crash, rate limit, stop, final artifact links, no duplicate final reply. |
| 8. Add proactive routines | Add channel opt-in, incident analysis, and closing updates. | Reuse event routines and incident delivery identities. No new scheduler. | Opt-out silence, one analysis per event, one closing update, authorized remediation, pause, replay, correct recovery evidence. |
| 9. Add Slack evidence | Add user OAuth, remote MCP reads, revocation, and audience enforcement. | Reuse HTTP effects, capability descriptors, and encrypted credential storage. | App eligibility, MCP session, requester scope, revoked token, Slack Connect, private/public audience separation, evidence provenance. |

Slices 2 and 3 form one release boundary. Automatic writes must not ship before their recovery behavior passes.
Slice 6 is optional product policy. It does not block autonomous launch or become a mandatory approval layer.
Streaming and Slack search remain separate release boundaries.

## Verification and rollout

Write failing behavior regressions before bug fixes and missing-feature implementations.
Exercise signed handlers, real domain operations, the database, and delivery boundaries.
Fake nondeterminism through the existing effect interpreters.
Use table-driven cases for permission, policy, retry, and entry-point coverage.

Use `build.log` and `build-test-dev.log` for development feedback.
Change `.test-args` to focus the existing watcher.
For shared-package changes, use the repository's documented watcher restart procedure.
For UI changes, exercise the existing conversation panel in both themes, at narrow widths, and with keyboard controls.

Before each release boundary:

1. Run the focused regressions and catalog coverage tests.
2. Remove replaced implementations and temporary adapters.
3. Record any remaining duplicate path and its concrete reason.
4. Repeat the required Haskell reviews until no material finding remains.
5. Run the final simplification pass.
6. Run local signoff after implementation and focused verification finish.
7. Run the local image check for affected assets or frontend builds.
8. Update the pull request only after the required local results pass.

Use the local OrbStack builder as repository instructions require.
Keep applied migration filenames and contents unchanged.
Choose new migration filenames after inspecting current master.
Group related, unapplied schema changes into one migration per coherent implementation slice.
Do not squash imported PR migrations without evidence that no supported database has applied them.
The local migration ledger confirms that agent migrations `0257`–`0260` have already run.
Their filenames and checksums must remain stable. Fewer migrations are a goal for new work, not a reason to break upgrades.
Report unavailable checks without claiming that they passed.
No production deployment or live app configuration change is authorized by this plan.

Native Slack acceptance requires a controlled workspace and recorded app configuration.
The coworker proposal reports Agent configuration changes. This branch contains no acceptance evidence for those claims.
Record actual grants, subscriptions, app eligibility, session behavior, and publication results before rollout.

## Completion criteria

- One operation descriptor governs schemas, policy, discovery, and execution behavior across callers.
- One executor and run ledger support all agent entry points.
- Autonomous routines complete enabled work without repeated human permission prompts.
- Current roles, project boundaries, pause, stop, and reduced authority constrain active work.
- Mutating retries preserve operation identity and expose ambiguous outcomes.
- Every visible action has an actor, run, result, and evidence link.
- Slack and web users inspect and continue the same conversation.
- New features reuse existing state before introducing another store or worker.
- Each slice records the duplicate implementations it removed.

## Implementation evidence

The [visual review](../docs/reviews/2026-10-30-unified-agent.html) shows the architecture, request flows, source excerpts, and remaining work.
It describes code snapshot `9198633ba`, including shared completion and typed capability policies.
The review now uses one main diagram, a guided request walkthrough, and collapsed code comparisons.
Text contrast exceeds 7:1 for the tested color pairs. The layout passes browser checks at 390, 768, and 1280 pixels.

The related PR runtime integration passes 33 review integration examples and fifteen worker examples.
Operation names and agent permission policies now live together in the API tool catalog.
MCP read hints, schema discovery, and agent execution use each tool's policy.
The separate name-indexed agent policy registry has been removed.
Twenty MCP examples pass, including POST-read annotations and privileged reads.
The agent regression also denies an unclassified operation with a previously known name.
Fifteen duplicate resource-read schemas, their fallback dispatch, and their response formatters have been removed.
Discord and WhatsApp now receive the host-supplied catalog through a viewer-read projection.
Their project integrations do not gain administrative reads or writes without a linked member.
The API issue response includes typed activity records so agent issue reads retain their history.
Resource reads use canonical API pagination and response shapes.
Query-only contexts retain the smaller telemetry tool set without a second resource API.
Bounded model results preserve both ends of large evidence, including the latest investigation status.
API tools and 23 typed agent-only operations now use one descriptor structure and one model adapter.
The separate OpenAI schema builders no longer exist.
Discovery, schema access, and dispatch use the same catalog.
A common role gate applies before each operation. Conversation documents retain their existing write behavior in Observe mode.
An unclassified API entry blocks a local operation with the same name.
All 151 bot integration examples and all 75 API v1 examples pass after this consolidation.
Operation retry metadata and recoverable action execution remain incomplete. Slice 1 is not complete.

Slack investigations, event runs, and scheduled runs now use the same checkpoint store and executor.
Each scheduled invocation carries its run ID rather than its installation ID.
Event input and its first checkpoint commit in one transaction under the run lock.
Checkpoint initialization verifies the project, conversation, and requesting member against the run.
Retries reuse completed reads and the original model request.
Scheduled runs now use the same active-run and membership checks as event runs.
The regression first demonstrated a repeated project read after interruption.
Both event and scheduled recovery cases now pass, including conversation isolation and a single retained user message.
All 153 bot integration examples pass, including existing Slack checkpoint rollback and conflict cases.
All 75 API v1 integration examples also pass after the checkpoint migration.
General web conversations now use the shared run ledger and checkpoint executor.
Both web forms submit a request ID. Legacy clients receive a new ID for each submission.
A run retains its input, origin, authorizing member, and typed Observe or Autonomous policy.
Retries cannot change that identity or increase its policy.
The common role gate still applies to each tool.

A new request creates its conversation and optional routine in one transaction.
Routine creation reuses the existing routine SQL and retains the defining instructions.
Retries do not change the routine or restore a deleted conversation.
The normalized assistant message and run completion also commit together.
These transactions reuse existing routine, completion, and result-message SQL.
The unused web action wrapper no longer exists.

The web regression first demonstrated a repeated project read after interruption.
Additional failing cases exposed routine reactivation and conversation resurrection during retry.
Three handler regressions now cover existing conversations, new conversations, and new routines.
They cover completion rollback, retained reads, one answer, immutable input and policy, pause, deletion, and revoked access.
All 156 bot integration examples and all 75 API v1 examples pass after these changes.
New interval JSON doctests remain part of the final doctest verification.

Issue chat and its global conversation route now retain the same request identity.
Both routes use the shared checkpoint executor and completion function in `Pkg.AI`.
The issue adapter retains its issue prompt and read-only policy.
Its separate answer parser and message-save path no longer exist.
The panel renders canonical stored history and returns a fresh form after success.
A failed request retains its current form and request identity.

Five handler regressions cover general chat, new conversations, new routines, issue chat, and the issue conversation route.
The issue cases first failed because retries repeated a completed project read.
The regressions also cover completion rollback, issue context, fresh form identities, deletion, and revoked access.
All 158 bot examples, all 45 issue examples, and all 75 API v1 examples pass.
The focused doctest attempt did not execute examples because the local package database lacks the `monoscope` library registration.
Final doctest verification remains pending.
Web, issue chat, and scheduled results now share `completeRun` and one answer normalizer.
Typed completion outcomes distinguish publication, quiet evidence, failure, timeout, and cancellation.
The old interactive completion, scheduler cleanup, cancellation query, and response-target wrapper have been removed.
Scheduled publication, retained report, run status, artifact snapshots, and lease advancement commit in one transaction.
A failed database write rolls them all back; recovery reuses the checkpointed model answer.
Lease checks reject an older worker before it writes any result.
Cancellation, pause, and withdrawn owner access suppress publication at completion.
Scheduled recovery retains the original authorizing member, instructions, and typed execution policy.
Increasing installation authority affects future runs; it does not elevate an in-flight run.

The regression first exposed publication and scheduler advancement after a failed run-status write.
Six completion cases cover run-status failure, scheduler-lease failure, stale workers, cancellation, pause, and revoked access.
All 163 bot examples, all 45 issue examples, and all 75 API v1 examples pass after this consolidation.
The formatter and diff checks pass. Scope reviews found no remaining material distillation, type-evasion, or client-behavior issue in this slice.
Event-specific completion and legacy stranded leases still require consolidation and recovery checks.
This transaction does not make external actions or project mutations exactly-once.

The catalog now supplies one interpreter for minimum roles, project writes, and wiki write grants.
Typed denial reasons are rendered at the dispatcher boundary; the wiki handler no longer checks roles separately.
The policy sum describes reads, conversation evidence, project knowledge, project changes, external publication, and administration.
Read capabilities can require a privileged role. Write capabilities have intrinsic editor or administrator minimum roles.
The old independent role/action record and separate action enum no longer exist.
A reader-level project-write descriptor cannot be constructed.
The boolean `actionsAllowed` predicate and its discovery-specific member exceptions have been removed.
Service invocations retain viewer reads and conversation evidence, but cannot acquire administrator reads or project-write authority.
Every interactive, scheduled, and event tool boundary binds the current member and conversation to the stored run.
The scoped background check reuses the same SQL as cancellation and completion checks.

Seven new regressions first exposed service-role bypass and missing actor or conversation binding.
They now pass, including valid reads after each rejected mismatch and ordinary service reads after denied privileged access.
All 170 bot examples, all 75 API examples, and all 45 issue examples pass with the final policy sum.
The formatter and diff checks pass. The repeated scope reviews found no material distillation, type-evasion, or client-behavior finding.
The two new policy-floor doctests remain part of final doctest verification.
The shared evaluator still interprets legacy routine action and wiki-grant fields.
Persisted installation policies, conservative adoption, default autonomous activation, and selected review remain unfinished.
Administrator writes retain the existing unavailable behavior until operation recovery is implemented; no new approval prompts were added.
A checkpoint does not establish exactly-once mutation or external publication.
Operation identities and safe write recovery remain necessary before autonomous writes reach release.


### Durable operation recovery (current implementation slice)

Mutating capabilities now use one typed operation ledger under the existing run.
A pending checkpoint supplies the stable operation position; the model does not supply its identity.
The proposal retains the original tool call, authorizing role, execution policy, and capability policy.
Current authority is checked again before accessing a retained result or starting a change.
The shared executor distinguishes a retained response from an uncertain outcome in its result type.
An uncertain outcome ends the model turn and suppresses subsequent mutations.
Identical function-and-argument mutations within one source request reuse the original response, even when the model changes the tool-call ID.
A new user request can intentionally perform the same operation again.

Artifact writes compose their existing transaction with the operation claim and outcome.
A failed outcome write rolls back the artifact change and claim together.
Opaque API and external writes retain the claim before execution and the response afterward.
Recovery does not repeat a claim whose response is missing; a legitimate late response can improve the retained record.
This is an explicit uncertain result, not a claim of universal exactly-once execution.
Slack publication retains the native response and configured channel without retaining authorization headers.
Tool schema discovery derives the recovery contract from the actual execution representation.
There is no second retry-policy registry or approval executor.

Run details show action status and bounded proposal/outcome previews using native details and HTMX refresh.
Preview disclosure checks the viewer's current role against the catalog policy.
Older chat findings and tool-result views still need a complete disclosure audit before sensitive administrator operations are enabled.
Legacy calls without a durable cursor remain a compatibility path and must be retired before broad autonomous activation.
Event attempt/root continuity, indexed root lookup, and cross-attempt concurrency still require explicit integration before event writes are enabled.
Run completion converts outstanding operation records to uncertain in the same fenced SQL statement. A stale worker cannot change an operation; an original worker can still retain a legitimate late response. No cleanup worker or additional migration is required.

One migration, `0261_agent_operations.sql`, contains this slice's operation schema.
Previously applied migration files remain unchanged because migration history is checksummed.
Nine new handler/DB regressions cover checkpoint failure, uncertain responses, repeated model proposals, atomic rollback, Slack publication, concurrent recovery, terminal run state, and recovery discovery.
All nine recovery regressions pass. The expanded focused suites pass after the terminal-state fix: 179 bot examples, 75 API v1 examples, and 45 issue examples. The timeout regression first failed on a retained OperationRunning state, then passed with the shared completion change.
A real Chrome test against the current WAI application and an isolated test database passes.
It covers uncertain-to-retained outcome refresh, a current-role downgrade, unchanged conversation URL, and preserved follow-up draft.
The test first exposed stale disclosure after refresh; disabling link prefetch fixes the issue without extra browser code.
The permanent end-to-end regression remains in `e2e/tests/routines.spec.ts`; the temporary browser-launch bridge was removed from the integration suite.
Slack recovery tests use the HTTP interpreter, not a controlled live workspace.
No Slack connector is exposed in this session, so live Slack acceptance remains unverified.
The formatter and diff checks pass. Repeated slice reviews found no remaining material distillation, type-evasion, or client-behavior findings.
Final whole-branch review, doctests, release signoff, and PR update remain pending.


### Persisted routine authority (current implementation slice)

Scheduled installations now have one typed `ExecutionPolicy` shared with their run snapshots, form decoder, database codec, and operation audit.
The legacy `allow_actions` and `memory_granted_by` columns, wiki-specific grant functions, denial reason, and boolean action wrapper are removed.
Project knowledge writes use the same Autonomous policy and current role check as other project changes.
Destination changes only select delivery; they do not grant authority or transfer the authorizing member.

New scheduled activations default to Autonomous, with disclosure in the routine catalog, schedule control, and new-routine composer.
Existing installations migrate to Observe, including previously enabled legacy action grants, until an editor explicitly adopts the unified policy.
Rescheduling and resuming preserve the stored policy.
The conversation has a typed Observe/Autonomous control guarded by current editor membership.
Saving it updates in place with a success toast and preserves the follow-up draft.

An authority revision fences withdrawn grants without rewriting the run's original policy or actor.
Reducing Autonomous to Observe advances that revision. Changing the authorizing member also advances it.
Runs retain the revision they started with, including through scheduler recovery.
Restoring Autonomous does not restore writes to an already restricted run; future runs capture the new authority.
The active-run and completion predicates reuse the same member-scoped policy query.

Eight focused authority cases pass, including automatic dashboard creation through the real shared operation, policy reduction/restoration, future-run authority, reader denial, pause, project knowledge, and actor/conversation binding. Rescheduling or resuming the active routine cannot restore withdrawn writes.
A standalone migration check passed on an isolated old-schema clone with both legacy Observe and legacy action-enabled routines.
It verifies conservative adoption, retained historical run policy, old-run restriction, removed legacy columns, and the new Autonomous default.
The browser regression first reproduced draft loss on policy save, then passed after the handler returned an in-place result.
The permanent browser case also retains the earlier action-history refresh and role-restriction checks.
The final focused suites pass: 180 bot examples, 75 API v1 examples, and 45 issue examples. The formatter and diff checks pass. Repeated slice reviews found no material distillation, type-evasion, or client-behavior findings. The production Haskell change adds eight net lines, including the policy UI, while removing the separate grant paths. Whole-branch review, doctests, live Slack acceptance, release signoff, and PR update remain pending.

Event execution still uses its conservative Observe ceiling until stable request-root continuity and event authority are integrated.
Legacy invocation constructors, administrator capability enablement and disclosure, and selected action review remain unfinished.
This slice does not claim that the entire authority milestone is complete.


### Request roots and event recovery (31 October)

Each run now has a non-null `root_run_id` referencing the existing run ledger.
Interactive requests and scheduled instants start as their own roots.
An event recovery creates another attempt under the original request root.
An explicit restart creates a new root and captures the current instructions, input, and authority.
No request table, scheduler, or second checkpoint store was added.

Event initialization locks the installation only while creating or resolving the attempt.
Concurrent duplicate workers receive the same attempt. After failure, concurrent recovery workers receive one new attempt under the same root.
Recovery keeps the original actor, conversation, input, instructions, policy, and authority revision.
A different actor or conversation cannot silently take over the request.
The shared executor loads retained event input before constructing the model request.
It resolves run checkpoints through the scoped root lookup while retaining each attempt's separate history.

Operation identities follow the root checkpoint position; operation evidence still names the attempt that executed it.
Known responses are reused within the request. Unresolved actions from the same source also block blind repetition after an explicit restart.
Historical attempts migrate as their own roots, preserving existing checkpoint keys.
The root foreign key, non-null constraint, and root/source indexes are in one coherent migration, `0263_agent_request_roots.sql`.
Applied migrations remain unchanged.

The regression first exposed a repeated project read after a failed event attempt.
Recovery now reuses the original model request and completed reads across attempts.
A second case interrupts an artifact append after commit but before checkpoint advancement; recovery retains the single append and response.
The cases also verify original input despite changed worker arguments, unchanged instructions and actor, sticky policy restriction, and foreign actor/conversation rejection.
A concurrent initialization case verifies one initial attempt, one recovery attempt, and the shared root.
The existing explicit-restart regression verifies a fresh root and retained prior artifacts.

The focused suites pass: 183 bot examples, 75 API v1 examples, and 45 issue examples.
The native review publication/recovery/retry integration case also passes.
An isolated old-schema migration check verifies retained checkpoint identity, null/unknown root rejection, and project cleanup with child attempts.
Both permanent browser scenarios pass against the current WAI application and an isolated database: policy/action history and the full routine lifecycle, including mobile themes, deleted conversations, and repository settings. The longer lifecycle capture needed a 180-second local test budget; it completed in 78 seconds with no product-code change. The repository CI timeout remains unchanged. The temporary browser launcher was removed. The formatter and diff checks pass; repeated slice reviews found no material distillation, type-evasion, or client-behavior findings. Final whole-branch review, doctests, live Slack acceptance, release signoff, and PR update remain pending.

Event actions still use the conservative Observe ceiling until invocation authority, role/scope ceilings, and publication audience controls are complete.
Service recovery adapters must retain the request's original authorizing member when installation ownership changes.
The next slice addresses scheduled worker lease fencing. Sensitive history disclosure and native completion consolidation remain release work.
This slice establishes request continuity; it does not claim the full unified agent is complete.

### One stored invocation context and worker lease (31 October)

Web, issue chat, Slack commands, Slack events, and scheduled jobs now load the same opaque `RunContext` from the existing run ledger.
It retains the original actor, project, conversation, source, policy, input, instructions, request root, and worker start time.
The public type has read-only field access, with no exported constructor, record updates, generic reconstruction, or decoder.
Callers can no longer assemble an interactive, scheduled, or event invocation from independent IDs and policy values.
The legacy `InteractiveWithActions` constructor is removed; the remaining modes are the read-only compatibility adapter and a stored run.

One context check now guards model boundaries, tool execution, and answer publication.
Interactive claims cannot load a background run. Background claims must provide their actual start time.
The same worker start time is checked under the run-row lock when claiming a mutation.
A recovered scheduler worker can retain the same run ID and checkpoint without granting its predecessor a fresh lease.
Already claimed external actions may still return late and improve the operation evidence; recovery never blindly repeats them.

The scheduled-worker regression first reproduced a real dashboard creation by the old worker after lease recovery.
It now checks rejection of stale and wrongly classified claims, no old-worker mutation, rejection of a direct uncheckpointed write, and one dashboard from the recovered worker.
The actor/conversation matrix also reproduced five missing publication checks across interactive, scheduled, and event runs.
The common context guard now rejects all six combinations before publishing an assistant message.
The existing wiki conflict case uses a distinct stale proposal, since an identical proposal correctly reuses its retained response within a request.

Event actions retain the existing conservative Observe ceiling in the common authority evaluator, including historical Autonomous snapshots.
Fresh event runs persist that ceiling; recovery still retains the original snapshot.
Enabling event autonomy remains dependent on the planned role/scope and publication audience work.
At this snapshot, old read-only chat adapters could still save conversation documents without a run. The following slice removes that compatibility path.
Native adapter completion and sensitive history disclosure are also unfinished.

No migration or new Haskell module was needed for this slice.
Applied migration files remain immutable, including those already used in local development.
SQL consolidation uses shared query fragments and the existing run/checkpoint/operation tables instead of adding another ledger.

The final focused suites pass: 184 bot examples, 75 API v1 examples, and 45 issue examples. The native review publication/recovery/retry case passes. Both permanent Chrome scenarios pass against the current WAI application and an isolated test database, including the full routine lifecycle and the policy/action-history flow. They completed in 107 seconds under the existing 180-second local browser budget; the repository CI timeout is unchanged. The temporary browser-launch bridge was removed. Formatter and diff checks pass. Repeated slice reviews found and fixed the missing publication guard and redundant context reconstruction, with no remaining material slice findings. Whole-branch review, doctests, controlled live Slack acceptance, release signoff, and PR update remain pending.

### Durable document writes (31 October)

Every catalogued write, including conversation artifacts, now requires both a stored run context and a durable checkpoint position.
The last uncheckpointed `ConversationWrite` fallback is removed.
Read-only compatibility adapters retain document reads, but no longer advertise document writes or receive write instructions in their model prompt.
Stored Observe runs can still retain conversation evidence through the normal checkpoint and atomic operation transaction; no extra approval is introduced.

The regression first reproduced a document saved through a direct uncheckpointed tool call.
It now checks both the legacy adapter and a stored run without a checkpoint, verifies the unchanged artifact list, and checks discovery and prompt behavior.
Existing document append, capacity, truncation, and rendering tests now execute writes through the real shared loop.
Their fixture reads the full response from the operation ledger and requires exactly one returned operation per request.
The legacy Slack-only artifact restriction was removed from that fixture; authenticated Slack artifact recovery is covered by the event-artifact integration case.

No schema changes or new Haskell modules were required.
Native enrichment and review still have specialized mutation and completion paths outside this dispatcher; those remain the next consolidation boundary.
The trace identifies separate artifact, chat, and finish calls in Slack, enrichment, and impact review. Their replacement must preserve the distinction between a completed computation and confirmed transport delivery.
The suites pass: 185 bot examples, 75 API v1 examples, and 45 issue examples, plus the native review publication/recovery case. The final prompt distillation also passes all three document regressions. Formatter and diff checks pass. Repeated slice reviews found no remaining material distillation, type-evasion, or client-behavior findings. The previous slice's browser acceptance remains the latest browser run; final release checks and live Slack acceptance remain pending.

### One completion interpreter for durable runs (31 October)

Interactive chat, schedules, Slack commands, Slack investigations, issue enrichment, and impact review now call `completeRun` with the same opaque stored context.
The separate `CompletionTarget` constructors and public `finishRoutineRun` / `insertRunResultMessage` entry points are removed.
Member claims bind the actor and conversation; native event claims bind the event reference and worker start time and recover the stored actor.
Native model calls and progress reports also use the stored context instead of independently supplied project, run, and instruction values.

The result algebra separates success/no-findings from conversation publication/retention.
This preserves a published no-findings review, a quiet scheduled result, and a Slack answer already stored before delivery without inventing another conversation message.
Agent evidence contains the typed `AgenticChatResult`; its answer and tool JSON derive from that one value in the completion interpreter.
Native reports carry an optional native payload, preserving the absence of structured findings without a synthetic model response.
Failure notices distinguish run-only diagnostics from conversation notices.
A single typed admission query distinguishes allowed work, unavailable member access, and explicit stops; model boundaries and completion share that query.
Temporary access loss can retain a recoverable delivery failure, while cancellation, pause, and deletion stay cancelled.

Final report snapshots, conversation publication, terminal status, outstanding-operation uncertainty, and any schedule advance commit together.
Native delivery acknowledgements and pre-delivery evidence retain their own records.
A matched scheduled run whose installation disappeared closes as cancelled without publishing or advancing another schedule; an older worker cannot close a replacement lease.
The enrichment regression first exposed a report already visible after the final status write failed. It now rolls that publication back and retries to one report and one answer.
Additional failing-first regressions cover deleted installations and explicit stop/pause/delete becoming retryable failures.
The historical snapshot assertion now targets the run inspector, since the completed retry correctly publishes its newer answer in the current conversation. The browser scenario also checks that separation.

Native business mutations still need operation-ledger and policy admission, and native initialization still needs recovery through installation ownership changes.
Sensitive history disclosure, event action scopes, selected review, remaining Slack features, and controlled live workspace acceptance remain unfinished.
No schema migration or new Haskell module was required for this slice.
Verification: 190 bot, 75 API v1, and 45 issue integration examples pass. The eleven completion regressions pass after the final cleanup. Both Chrome scenarios pass against the current WAI application and isolated test database, including policy controls, action history, retained drafts, and run-inspector boundaries. The native review/SDK suite passed 33 examples during this slice; its publication/recovery regression passed again after the final admission change. Formatting and diff checks pass. Whole-branch review, live Slack acceptance, and release signoff remain pending.


### Native request recovery after owner changes (31 October)

A routine owner change used to strand a running native event: the recovery adapter supplied the new installer, while the saved request required its original member. This affected both issue enrichment and PR reviews.

- `EventRequest` carries a member for Slack messages and commands. Native review and enrichment request constructors take only their source identity; callers cannot substitute a new member during recovery.
- The existing initialization query derives a new native request's owner from the locked installation. Continued attempts retain the root request's actor, input, instructions, and authority version.
- Slack requests retain exact-member matching. Native recovery still checks both the current installer and the original actor's current access through the shared authority query.
- Explicit restarts create a new root with the current owner and instructions, even when the adapter holds an older installation snapshot.
- No new migration, alternate executor, or recovery table was added.

Both native-source regressions failed before the fix. They cover resuming a running attempt, retrying a failed attempt, retained request evidence, revoked original-member access, and an explicit restart. The focused regressions pass, followed by 192 bot examples, 33 native review/SDK examples, 75 API v1 examples, and 45 issue examples. Formatting and diff checks pass; repeated slice reviews find no material distillation, constraint-evasion, or client-behavior issues. Full-branch review, live Slack acceptance, and release signoff remain pending.

The next disclosure slice must cover the same evidence at every boundary: `AIThreads` currently gates operation previews, while the Findings/Tool results panels, run artifacts, conversation messages, and model history still use less specific access checks. Sensitive reads already have catalog role requirements, so blocking administrator mutations alone does not complete this work. Retained answers and documents derived from a sensitive read must carry that requirement too; hiding a tool preview alone is insufficient. Use one provenance representation and evaluator across model input, tool results, documents, and publication, including role changes during a run. Reuse the capability catalog for legacy evidence; do not introduce a second list of privileged tool names. Keep any required append-only schema changes together in one coherent migration.


### Shared evidence requirements and a pure capability catalog (31 October)

A handler regression reproduced a private administrator-derived answer remaining visible after the member became a reader. Additional failing regressions exposed copied native instructions losing their source restriction, a private installation being hidden from activation lookup, and an answer being returned after access changed during title generation.

- `Pkg.AgentCatalog` now owns the shared descriptor, API aliases and policies, local operation algebra and schemas, and composite operation algebra and schemas. MCP and the conversational executor interpret those same descriptors. Evidence classification uses that catalog; it has no separate list of privileged operation names.
- `disclosurePermission` separates the role required to perform a mutation from the audience of its result. Ordinary project mutations retain project-readable results. Restricted reads and administrative results retain their stronger audience.
- One append-only migration, `0264_agent_evidence_scope.sql`, stores requirements on conversations and retained runs. Database triggers preserve their strongest recorded requirement across updates, new attempts, deletion, and recreation. PostgreSQL's historical permission-enum order differs from the domain order; a shared rank function and exhaustive integration comparisons make that distinction explicit.
- A conversation's requirement rises before a restricted tool executes. Existing messages, model checkpoints, documents, run snapshots, titles, and future answers remain within that conversation requirement. This deliberately uses one conservative conversation scope rather than independent per-message rules.
- Model, tool, title, cached-answer, web-history, document, navigation, run-inspector, Slack delivery, wiki, GitHub review, and shared-issue publication boundaries use that requirement. Current role and delivery audience both apply. The existing project-view Slack audience does not acquire administrator evidence merely because the linked requester is an administrator.
- Historical tool metadata and checkpoints are parsed through the existing wire types. Copied routine-instruction dependencies propagate the strongest known role; malformed evidence, retired capabilities, and missing historical sources remain administrator-only. Classification retains already recorded restrictions.
- Native event initialization copies the installation's requirement along with its instructions and retains any stronger original-request requirement. A private existing installation is still found during activation, so hiding its presentation does not create an orphan conversation or cause a duplicate-key failure.
- Web sudo and public-demo behavior remain supported. Background project-member access still requires actual membership. Session reads and atomic interactive completion share the same permission query.
- `CompletionResult` now carries a `RunTermination`, whose constructors exclude running work. Callers distinguish an actually cancelled transaction from a published answer. Title generation checks access before and after its model call and before returning a result; the former exported single-caller title wrapper is removed.
- Trace logs retain tool names and result sizes, without raw arguments or response previews. Full evidence remains in the protected records.

The first handler failure, native instruction failure, activation failure, historical-instruction failure, and title-return failure were reproduced before their fixes. Ten focused disclosure regressions pass. They also cover role changes during inference, cached-answer and document access, late completion cancellation, navigation titles, project isolation, legacy unknown schemas, scope preservation, and ordinary denied reads that must not make a public conversation private. The broader suites pass 201 bot, 75 API v1, 45 issue, and 33 native review/SDK examples. All three Chrome scenarios pass against the current WAI application and an isolated database, including the new role-downgrade case. The temporary browser launcher is removed. Formatting and diff checks pass. Final whole-branch review, doctests, release signoff, and controlled live Slack acceptance remain pending.

A subsequent isolated PostgreSQL 16 benchmark reproduced the scope-update cost with 5,000 retained runs: the original trigger took 15.0 seconds. One indexed SQL lookup now supplies both the inheritance trigger and the Haskell permission check. The same update took 0.29 seconds; 50,000 runs took 3.0 seconds, with the measured individual lookup below 0.1 ms. These are temporary-table measurements, not production latency guarantees. Partial indexes also cover pending historical classification. The existing role-order and recreation integration test now checks both editor and administrator requirements against attempted downgrades. The coherent append-only follow-up is `0265_agent_evidence_lookup.sql`; `0264` was already applied and remains unchanged. The ten focused disclosure examples and all 201 bot examples pass after the lookup change. Formatting and diff checks pass. Repeated slice reviews found no remaining material distillation, constraint-evasion, or client-behavior findings; whole-branch review and release signoff remain pending.

This work adds no approval prompts. Native business mutations still need the common operation ledger, event autonomy remains conservatively capped, optional selected review and broader Slack audiences remain unfinished, and controlled live Slack acceptance and final release signoff are still pending.


### Requester-bound busy-thread followups (31 October)

The existing interrupted Slack investigation regression now includes 21 earlier replies from other linked readers and administrators. Before the fix, those replies reached the model as user instructions inside the original requester's run. The regression failed on that exact cross-requester instruction injection.

Pending-followup selection now requires the run's stored requesting member before applying the existing 20-message limit. The fresh Slack-principal check also requires that same member, so a relinked identity cannot enter the run between selection and acceptance. The original requester's steering still survives interruption and replay exactly once. Other members' receipts remain unprocessed for their own requests; their earlier replies cannot starve the requester's later followup. No new migration, permission prompt, executor, or message store was added.

The focused regression and all 201 bot examples pass after the two-line runtime change. Formatting and diff checks pass; repeated slice reviews found no remaining material distillation, constraint-evasion, or client-behavior findings. Existing already-accepted historical steering must still be considered at the event-autonomy transition: this change prevents new cross-requester acceptance, and does not rewrite old model checkpoints. The temporary event Observe ceiling remains until the effective owner/requester authority and transition behavior are verified.


## External references

- [Slack MCP overview](https://docs.slack.dev/ai/slack-mcp-server/): eligibility and user authorization requirements.
- [Stream start](https://docs.slack.dev/reference/methods/chat.startStream/): stream arguments and recipient requirements.
- [Stream completion](https://docs.slack.dev/reference/methods/chat.stopStream/): final blocks and metadata.

External API assumptions require another review during their implementation slice.
