# Unified Monoscope agent

Status: implementation in progress, 1 November 2026. This plan controls the complete branch scope; the checklist below does not replace the product contract.

Consolidate Slack, web, scheduled, and event work around one capability catalog, authority evaluator, executor, operation ledger, conversation, and artifact experience. Newly enabled background routines act autonomously within their member's current authority. Optional selected review must not become a mandatory permission loop.

Prefer small algebraic interfaces, opaque proofs, precise effect rows, established libraries, and internal eDSLs that reduce total maintained code. Keep invalid states out of constructors. Remove replaced implementations in the same slice. Keep fewer integrated pages; inspect work through the existing conversation and run panel.

The [visual review](https://chatgpt.com/space/page_9c75b6b56b0481918e666edd7a89a919) is the review destination. Its [repository artifact](../docs/reviews/2026-10-30-unified-agent.html) contains the same diagrams, walkthroughs, code excerpts, and roadmap.

## Current implementation and remaining work

All twenty selected incoming PR heads remain included. A fresh inventory found 28 open PRs with no selected head missing. The [integration plan](2026-10-30-agent-pr-integration.md) defines that selection. Published PR 619 and remote master `3c34f3c83` are ancestors of this branch. The latest local changes have not been pushed or deployed.

| Requirement | Current implementation | Work still required |
| --- | --- | --- |
| Shared capabilities | One typed API/agent descriptor governs discovery, policy, schema, dispatch, and bounded model responses. CLI server operations use the public API; client-only operations remain local. | Final whole-branch catalog and package-boundary verification. |
| Autonomous authority | Observe, Autonomous, and selected review share the current-role evaluator, immutable actor grants, original role ceilings, and monotonic restrictions. New routine activation defaults to Autonomous; adopted legacy read-only work stays Observe. | Final whole-branch role, revocation, pause, stop, and legacy-adoption regression run. |
| Recoverable operations | One ledger retains immutable proposals, decisions, action identities, receipts, and uncertain outcomes. Domain mutations and receipts commit together where possible; external actions reconcile before retry. | Whole-branch interruption coverage and arbitrary native host-loss acceptance; no universal exactly-once claim. |
| Requested conversations | Web, issue chat, slash commands, Slack events, schedules, enrichment, and incidents queue existing jobs and use shared execution/completion. Origins, request IDs, titles, and replies survive retry. | Trace and verify the silent private-channel mention in the controlled Slack workspace. |
| Shared memory | Wiki read/search/write use versioned ProjectMemory, the common capability policy, transactional edit recipe, and operation ledger. Native attempts retain their actual IDs and revisions link to the shared writing run. | Final cross-entry-point and author-attribution regression run. |
| Optional selected review | Web and signed Slack controls use the same decision state and continuation queue. Decisions, expiry, revocation, pause, deletion, and terminal cleanup preserve the original request. | Final whole-branch continuation acceptance. |
| Native review and fix integration | PR analysis, fixes, admission, and independent review share retained SDK/VM sessions and artifact validation. Native fixes also share run admission, authority, wiki receipts, and atomic completion. | Complete arbitrary host-loss coverage and Linux/KVM acceptance described below. |
| Slack streaming | Durable final publication and uncertain-acknowledgement reconciliation already exist. | Implement streaming after real-workspace interruption and ambiguous-start recovery acceptance. Verify start/append/stop, crash, rate limit, cancellation, artifact links, and one final reply. |
| Proactive work | Channel opt-in, incident analysis, measured recovery/resolution reports, and authorized remediation reuse event routines, the common worker, and incident outbox. Analysis defaults to Observe. | Controlled Slack acceptance and final opt-out, replay, closing-source, pause, and authority regressions. |
| Slack evidence | Requester-bound OAuth, authenticated encryption, refresh/revocation, remote MCP reads, typed audiences, and retained provenance use shared systems. Provider writes stay excluded. | Verify actual app eligibility, advertised schemas/session behavior, Slack Connect, and private/public audience containment live. |
| Integrated experience | Existing conversations, run inspectors, review controls, and administrator-only fix artifacts form one experience. The artifact adds no product page. | Final complete-flow browser review across loading, empty, failure, keyboard, light/dark, and narrow-width states. |
| Publication | Work stays on the PR branch; existing local verification checkpoints are recorded below. | Repeated whole-branch reviews, final simplification, Linux/KVM, local CI/image signoff, and canonical PR 619 update. Repeat doctests if their inputs change. |

Streaming, Slack evidence, and proactive work remain part of this full plan even though their rollout boundaries are separate. A narrower autonomous-launch boundary does not establish completion of the plan.

## Latest verified checkpoint

- Native integration: all 49 cases pass in 336.8260 seconds after the missing-manifest fix. A failing-first regression covers selected review, provider interruption, and accepted-result retention: both context loading and execution reject orphaned retained files without inference or transcript changes, then resume after manifest repair. The shared loader adds two lines; the existing SDK/VM fixture covers all three states. Logs are `/tmp/native-missing-manifest-red.log`, `/tmp/native-missing-manifest-three-green.log`, and `/tmp/native-missing-manifest-49-green.log`; these are session evidence, not release attestations.
- Runtime static scan: HLint reports zero hints across all 86 changed `.hs` files under `src/` and `shared/src/` relative to local `master`. The diff scan finds no added warning suppression, `unsafeEff`, `unsafePerformIO`, `unsafeCoerce`, `fromJust`, or `undefined`. This does not establish whole-branch semantic or interface review; tests, CLI modules, and generated sources are outside this scan. The JSON result is `/tmp/unified-agent-runtime-hlint.json`.
- SDK worker: all 19 tests pass on Node 25.2.1.
- Abrupt SDK-worker loss: the six PR recovery cases pass after adding SIGKILL following a committed wiki write. Retry from the earlier paired checkpoint retains one revision and its original writing run, the original evidence/window/notes, and one final publication. Six model requests include one unreported call, whose cost remains unknown. Structured tool events supply the owned process ID. The expanded 49-case suite passes; whole-machine and uncheckpointed-work recovery remain unverified.
- Doctests: `make test-doctests` passes all 2,048 examples, with zero errors or failures. Its first attempt exposed a missing direct `openai` dependency in the compiled integration-test component; `c3a7cd566` fixes that dependency and regenerates the Cabal source-file inventory. The rerun compiles the affected specs and executes the examples successfully.
- Mac CLI packaging: the existing package script passes signing, strict signature verification, dependency linkage, and `--version` on a temporary copy of the cached CLI binary. It replaces the development binary's Homebrew zlib path with `/usr/lib/libz.1.dylib`. This is a packaging-path check, not a fresh release build or Linux attestation.
- Server: the watcher compiles all 167 modules. Its configured production database rejects authentication, so compilation does not establish live application acceptance.
- Browser artifact: all 101 steps in twelve flows pass at widths 320/390/768/1280, including expanded source. All 113 source panels match their cited Git revisions. Checked text/surface pairs have at least 7.09:1 contrast; no horizontal overflow or browser errors were observed. Keyboard selection retains focus. Local Chrome inspected the uploaded HTML; the hosted viewer and screen-reader interaction were not tested.
- Hosted review: the existing Page visualization was replaced and read back while preserving its title and surrounding content. The latest HTML is 184,198 bytes, SHA-256 `59f78729f64034f444e12f951653dca3bd1c61daa0b50ba7c9df6ca6d58fefc8`, at Page sequence 53. It includes the verified 49-case native result, abrupt-worker recovery, and missing-manifest behavior. The full 101-step browser check passes again at all four widths; all 113 excerpts retain their cited source text, with presentation indentation removed. Each code panel retains its own source revision.
- Shared API and incident acceptance: all 76 API examples pass in 20.7662 seconds; all 41 incident-delivery examples pass in 13.7290 seconds. These runs exercise the current combined implementation, including real HTTP catalog dispatch and proactive incident replay, authority withdrawal, and ambiguous-delivery reconciliation. Saved logs are `scripts/local/unified-agent-api-76-green.log` and `scripts/local/unified-agent-incidents-41-green.log`.
- Source and CI checks: the pinned formatter passes all 62 changed Haskell source files. HLint found five expression simplifications; `0ad8a4d8f` applies them, and the affected files pass the follow-up check. The missing-manifest interpreter also passes formatting and HLint. `make ci-selftest` passes, including all twelve release-decision tests with a local Git remote and fake registry. This is not full CI, an image build, or an attestation. Its saved log is `scripts/local/unified-agent-ci-selftest.log`.
- Earlier checkpoints: 450 bot examples and 39 Slack-evidence examples passed in their recorded slices. These passes are not a final whole-branch signoff.
- Live Slack configuration: the [1 November audit](../docs/reviews/2026-11-01-slack-live-configuration.md) compares the authenticated app manifest with the branch template. It identifies missing Messages-tab configuration, two bot scopes, eleven event subscriptions, and the message shortcut. No live configuration changed. MCP eligibility remains unproven; the submission overview offers preparation instructions without a publication status.
- Isolated Slack acceptance: the app portal offers Monoscope workspace `T0AP82L853K` for app creation. A bot-only “Monoscope Local QA” manifest passed Slack validation and reached the final Create and Install screen. Confirmation for this new installation is pending. No QA app exists yet; no personal search grant, shared-app callback change, or production deployment occurred. The proposed manifest is `scripts/local/monoscope-local-qa-manifest.json`.

The new `nativeFix_applicationResumesSdkSessionsWithoutReplacingItsRunOrAttempts` case passes in 20.6481 seconds through the real application worker, SDK, VM, and database. It retains one shared run, three original attempts, three regression receipts, and one result across admission, candidate, and peer interruptions; completed replay makes no new inference call. GitHub exchange and object retention use existing test interpreters. The expanded full suite now passes all 49 cases. The subsequent Effectful adapter reduction passes the same application-job case and all five PR continuation cases, using the existing production interpreter and original callback effect rows. Browser acceptance of the integrated routine flow passes four cases. Whole-branch review remains pending; preserve concurrent active edits and test focus.

The [native continuation audit](../docs/reviews/2026-11-01-native-continuation-audit.md) records the current scoped review. Earlier detailed implementation narratives remain available through `git show d881b701b:plans/2026-10-30-unified-agent.md`. Their historical test counts and unfinished notes do not override this checklist or the contract below.

## Product contract

Monoscope investigates telemetry, uses project tools, remembers team knowledge, and follows up on its work.
Slack, the web app, schedules, and project events provide inputs to the same system.
Every execution has a project, an authorizing member, a conversation, a run, and retained evidence.

Background routines act automatically within their configured authority.
Users do not approve every tool call or routine run.
An enabled routine grants continuing authority until someone pauses it or changes its policy.
The authorizing member's current role limits that authority. Each request also retains its original role ceiling; upgrades apply to new work.
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

Fresh Slack requests now use the routine's configured policy and the shared owner/requester role ceiling.
Legacy Slack checkpoints retain Observe authority during adoption into the common run checkpoint.
Interactive, scheduled, and Slack administrator actions use the common Autonomous policy and durable operation ledger.
Native wiki, GitHub publication, and issue enrichment use that ledger.
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
Retain the actual agent attempt in each native operation and link wiki authorship to its shared writing run. Never cast between attempt and run IDs.
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
Slack's [official search reference](https://github.com/slackapi/slack-skills-plugin/blob/main/skills/slack-search/SKILL.md) confirms `slack_search_public`, `slack_search_public_and_private`, `slack_read_channel`, and `slack_read_thread`. Their full input/output schemas and this app's live eligibility still need verification; these names alone do not establish a working integration.

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

## Native and requester-evidence invariants

### Capability, request, and review boundaries

Capability constructors must enforce intrinsic editor/administrator floors for writes. Service invocations retain ordinary viewer reads and conversation evidence, without administrative reads or project-write authority. Discovery, execution, and evidence classification interpret the same descriptors. Preserve the caller's existing effect interpreters and the CLI/shared package dependency boundary.

Queued ConversationRun work retains a durable member grant. Historical browser InteractiveRun requests cannot silently become background grants. Issue context uses the shared required-project builder, without a simulated browser session or duplicate issue guide. Slash intake commits input, requester, grant, destination, and job together; acknowledgement performs no inference. Delivery uses the original channel/DM and the real root run ID, with no fabricated message timestamp. A slash reply remains an unthreaded channel-visible message; delivery failure cannot redirect its audience or fall back to the response URL.

Legacy Slack adoption preserves its original model request, system context, completed results, accepted steering, and pending intent under Observe/viewer ceilings. Reuse a known mutation response. An unresolved or unrecorded mutation becomes LegacyUncertain and an unknown operation, rather than being replayed. A relinked identity cannot adopt another member's retained request. Busy-thread followups select the stored requester before applying the message limit; another member's receipts cannot steer or starve that request.

Selected review uses a nonempty collection of action classes and one reviewer requirement. Conversation evidence storage and ordinary reads stay automatic. Decisions retain the reviewer's authority at decision time as well as current membership; a later reviewer upgrade cannot strengthen an earlier vote. Review arguments, actor, audience, deadline, and decision cannot change on retry. Expiry before execution prevents mutation even after approval. Decision capabilities stay outside the model's tool catalog.

Pending review releases the worker, thread lock, and execution lease while retaining the original request. Approval/expiry wakes the existing job. Deferral locks the proposal and cannot postpone a decision already recorded; stale leases or duplicate jobs cannot consume a continuation twice. Request-scoped lookup retains the original proposal attempt while finding its latest admitted continuation. Signed stop and routine controls cancel through the same transactional finalizer. Preserve uncertain started effects, proposals, votes, and artifacts; post-commit notice cleanup cannot restore execution authority or call the model. Resume enables future work, never the cancelled request.

Maintenance retires actorless installations and detached attempts through the same completion transaction and artifact snapshots. It must not invent a replacement actor, expand a null grant, or cancel a healthy current lease during reauthorization.

External publication reconciliation binds the stored destination, GitHub App identity, and exact retained body. A wrong app or edited body cannot close an uncertain action. A fresh request must resolve an older pending publication before making another change; without an observed receipt, that action stays stopped.

### Native fixes and retained sessions

A native fix has one dedicated conversation, shared run, immutable administrator grant, and existing fix job. Duplicate admission keeps the original request. Only its current native lease constructs the opaque run context; an ordinary conversation claim or replaced lease cannot borrow that authority. Account or role loss, withdrawn setup/credentials, and revoked grants prevent execution. Observed revocation cancels the original request; later restoration of access cannot revive it. Expired but still-authorized leases remain recoverable.

Main, admission, and peer attempts retain their real typed attempt IDs. Every restored attempt must belong to the exact project and fix. Wiki authors point to the actual shared writing run; never cast an attempt ID to a run ID. Repeated identical wiki actions reuse the saved response and revision. Native artifact bookkeeping and common completion commit in one transaction; failure on either side rolls back both, including the shared result artifact.

The shared run inspector exposes a generic report. Specialized native output remains behind its existing administrator-only handlers. An administrator link may open that view; viewers must neither receive the link nor its private output. No additional fix-history page, worker, scheduler, or permission store is required.

PR, fix, admission, and peer continuation use one retained-session interpreter. Preserve the original prompt, model specification, evidence window, evidence, notes, attempt, tool budgets, completed tool IDs, and pending parent/child tool. Resolve current authority and credentials again. A retry of the same verified comparison retains its original peer decision and reviewer attempt; a new independent review requires a separate root. Admission binds the exact regression artifact.

Publish the SDK transcript/VM/workflow checkpoint manifest last. A missing manifest means a fresh session only when its directory is empty; retained files without a manifest fail before inference and remain available for repair. Validate identity and transcript fingerprints before restore. Restore a private guest copy; keep its live disk separate from the current clean verifier baseline. Fix continuation retains the verifier-session identity and phase receipts rather than restarting reproduction. An unresolved child wait must not consume its admission budget again on every wake. End-of-input cancels the SDK before another model start; cancellation is not a completed tool failure.

Saved candidate descriptors are not verification proofs. Only capture or checked restore can construct CapturedCandidate; it has no Generic or FromJSON bypass. CandidatePatch also requires its existing smart constructor at JSON decode. Restore validates admission metadata, native reports, logs, source fingerprints, image identity, Git history, patch/bundle hashes, and comparison receipts through the shared verifier. Corruption or changed inputs fail before another model call. Completed recovery revalidates the actual result and reconstructs the reviewed proof without model inference. Durable completion releases obsolete suspended disks while retaining its transcript and result.

Cooperative provider-failure and typed review-wait recovery are implemented. A failure after an accepted result returns that checked result only if no later tool invalidated it. Failed calls remain attributable in usage. Abrupt SDK-worker loss after a committed wiki write is verified with an existing paired checkpoint and the shared operation ledger. Whole-machine loss and live work that was never checkpointed remain separate acceptance requirements; a cooperative snapshot is not evidence of them. The installed Gondolin `VM.checkpoint` stops the guest, so saving at inference boundaries would require verified resume semantics and cost measurements, not just another call to the current suspension helper. Cover the actual background-job path and Linux/KVM before claiming native production acceptance.

Migration `0280_native_fix_runs.sql` groups native linkage and original-requester backfill and has already run locally. It must remain unchanged.

### Request origins, OAuth, and evidence audiences

Retain the immutable signed Slack destination and requester before queueing or reading history. Deleting/recreating web conversation metadata cannot change the request's audience, origin, or retry identity. Late authority loss rejects a replay without creating a fresh request. Terminal hooks and maintenance use the same finalizer; completed work cannot become eligible to execute more operations.

One typed audience and evaluator govern initial transcripts, navigation, titles, documents, retained runs, artifacts, model context, and publication. Personal evidence requires its author and current eligible project membership. Channel evidence requires the current linked channel membership. Signed receipt provenance takes precedence over mutable conversation metadata; missing or conflicting provenance stays restricted, including after deletion/recreation. Shared mutations and publication cannot launder private evidence into another audience.

Keep execution permission distinct from result-disclosure permission. Raise the conservative conversation/run requirement before a restricted tool executes, and retain its strongest scope across new attempts, metadata updates, deletion, and recreation. That scope also covers older findings and tool-result views, checkpoints, model history, documents, titles, and future answers; their final disclosure audit remains required. Domain role ordering must not depend on PostgreSQL enum ordering.

Personal consent binds project, initiating account, linked Slack user/workspace, expiring receipt, and S256 verifier. It cannot replace the workspace bot installation or another user's grant. Access/refresh tokens use shared authenticated encryption and have no diagnostic encoders that expose them. Existing legacy credential reads remain compatible without weakening new writes.

Grant creation, refresh claims, disconnect, and signed upstream revocation share the member lock. HTTP stays outside database transactions. Conditional writes bind the exact credential and claim, so disconnect, membership loss, workspace replacement, or later reconnection cannot resurrect an old grant. Rotating grants keep refresh token/expiry together. An uncertain or abandoned refresh claim cannot automatically replay a single-use token; definite rate limits use the existing cooldown.

Signed revocation must match its immutable receipt subject and target workspace/user without deleting the bot installation or another account's grant. Receipt completion and grant/consent deletion commit together. Replays and delayed workers cannot revoke a later reconnection. Acquire member locks in stable order and keep expired-consent cleanup in that member/project scope.

Remote MCP reads validate initialization/response identity and advertised schemas, release sessions, and use only the requesting user's current grant. Current-channel reads bind the signed channel and reject unrecognized locator arguments. Workspace-wide search requires the requester's personal DM or web context. Provider write tools remain excluded even if annotated read-only. Discovery denial, throttling, schema changes, or provider failure leave core project work available; requested reads respect cooldown without repeating provider requests.

### Slack sessions and proactive delivery

Generated titles initialize an untitled conversation/session and cannot overwrite a user's rename. Signed rename intake and its existing worker update thread metadata and the app conversation atomically, with stale-event and current-member checks. Messages-tab opens refresh contextual suggestions through the same signed identity and DM lock, including first visits without an assistant-thread row. Ordinary thread stops retain explicit confirmation and status cleanup; do not substitute a different code-channel lifecycle contract.

An incident installation binds a real workspace/channel pair and excludes DMs and malformed channel IDs. Admission and publication recheck project, workspace, channel, issue conversation, installation, activation boundary, authority, and public evidence scope. Reauthorization/resume excludes pre-activation events. Only an alert, measured recovery, or explicit resolution can start the corresponding report; a data gap cannot create a closing report. Event identities, retained completion, and the existing outbox prevent repeated inference or duplicate publication after an uncertain acknowledgement.

## Remaining execution order and release evidence

1. Add meaningful arbitrary-host-loss coverage through the existing ownership and checkpoint boundaries. Preserve the original run, attempts, model, comparison receipts, and inference-free completed replay established by the SDK application-job and PR continuation cases.
2. Repeat hs-distill, hs-evasion-review, and hs-lob-review over the complete branch; implement material findings and repeat until settled. Review runtime callers and opaque proofs as well as changed declarations. Use existing library interpreters/combinators where they delete code; do not weaken types or effect discipline to reduce line count.
3. Apply emil-design-eng, better-interface, and better-ui to the integrated conversation/run/review/artifact flows. Exercise actual handlers and browser interactions, including loading/failure states, both themes, narrow widths, and keyboard use. Keep fixture transport, authenticated application, physical-device, and live Slack evidence distinct.
4. Complete controlled Monoscope Slack acceptance: app grants/subscriptions/eligibility, the silent private mention, linked/unlinked outcomes, native sessions and renames, prompts, shortcut intake, Slack Connect, actual MCP schemas, and audience containment. The missing connector is not proof that no browser session exists; inspect available tools before asking for information already available. A controlled test destination still needs to be identified before sending test messages.
5. Establish real interruption and ambiguous-start recovery, then implement and verify streaming through the current publication state. Saving a returned timestamp alone cannot close a lost-start acknowledgement window.
6. Run the required doctests, focused/full regression gates, CLI-linkage check, and Linux/KVM acceptance with truthful capabilities. Run simplify as the last code-review pass. Only after implementation, reviews, and focused verification finish, run final local signoff and the required image check using `DOCKER_CONTEXT=orbstack MONOSCOPE_BUILDER=monoscope-local-amd64`. Follow `docs/local-ci.md` and AGENTS.md; do not publish an attestation for an unpassed check.
7. Push/update canonical PR 619 after required local checks pass. Review and merge through GitHub. This plan does not authorize a production deployment or live app configuration change.

Applied migration filenames and contents remain immutable, including the locally applied native-fix migration. Consolidate related future, unapplied schema work into one coherent migration; do not squash previously applied files to reduce the file count.

## External references

- [Slack MCP overview](https://docs.slack.dev/ai/slack-mcp-server/): eligibility and user authorization requirements.
- [User-token exchange](https://docs.slack.dev/reference/methods/oauth.v2.user.access/) and [authorization metadata](https://mcp.slack.com/.well-known/oauth-authorization-server): endpoints, client-secret POST, and S256 PKCE.
- [Token rotation](https://docs.slack.dev/authentication/using-token-rotation/): paired refresh/expiry metadata and single-use refresh tokens.
- [Upstream token revocation](https://docs.slack.dev/reference/events/tokens_revoked/): workspace-bound OAuth user IDs; signed receipt handling and recovery are implemented.
- [Message shortcuts](https://docs.slack.dev/reference/interaction-payloads/shortcuts-interaction-payload/): signed invocation, selected-message context, and callback identity.
- [Suggested prompts](https://docs.slack.dev/reference/methods/assistant.threads.setSuggestedPrompts/): Agent apps omit the legacy thread timestamp and provide at most four prompts.
- [Agent session status](https://docs.slack.dev/reference/methods/agents.sessions.setStatus/): initial titles are ignored once a session exists.
- [Agent sessions](https://docs.slack.dev/ai/agent-sessions/): native status, renaming, and title-change events.
- [Stream start](https://docs.slack.dev/reference/methods/chat.startStream/): stream arguments and recipient requirements.
- [Stream completion](https://docs.slack.dev/reference/methods/chat.stopStream/): final blocks and metadata.

External API assumptions require another review during their implementation slice.
