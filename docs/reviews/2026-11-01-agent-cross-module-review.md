# Agent publication and integration review

Date: 1 November 2026.
This is a scoped review of the current branch against `origin/master`.
It does not establish whole-branch signoff.

## Functions and boundaries inspected

The publication review covers GitPublication, its Native implementation and Types,
AgentArtifacts, ReviewEvaluation, and their public entry points.
It follows request capabilities, authorization callbacks, retained journals,
remote repository identity, source revisions, uploaded media, and receipt validation.
The artifact review follows bounded reads, digests, object keys, private packaging,
and the distinction between application capabilities and backend receipts.

The registry review covers AgentCatalog in full.
It follows all API classifications, local operations, native operations, composite tools,
review classes, minimum mutation roles, and result-disclosure roles.
The raw-SQL caller was followed into executeSecuredQuery and its tenant-scoping boundary.

The integration review covers the changed metric-catalog types and functions,
getMetricCatalogResponse, getMetricCatalogPage, the overview caller,
queryMetrics and queryProjectMetrics, and their common runQueryAST path.
It also covers the new configuration fields, worker-tool wiring, maintenance timer,
command-line evaluation dispatch, the shared Git command builder,
and the changed scoped Markdown renderer.
The CLI command additions were inspected, but the complete workspace implementation
remains a separate review scope.

## Repeated skill passes

The hs-distill pass checked for parallel publication executors, duplicated tool policies,
repeated query execution, and handwritten instances that bypass existing derivation.
Publication stages reuse the existing capability and callback instead of adding authority state.
The metric API and overview reuse one catalog query with an explicit page mode.
The chart wrapper parses its optional project once before the project-bound function.
A second pass found no further material reduction within these functions.

The hs-evasion-review pass checked constructors, decoding, effect boundaries,
remote identity checks, resource cleanup, and the difference between live and fake interpreters.
The request, published-fix, retained-media, and byte capabilities hide their constructors.
Remote publication rechecks permission, repository, branch, source and media identity.
The native backend returns a receipt; only the application interpreter creates PublishedFix.
The evaluation interpreter retains rejected submissions and calls the supplied result validator.
Raw process and storage IO stays inside interpreters or executable entry points.
A second pass found no new material contract evasion in this scope.

The hs-lob-review pass checked the changed Markdown renderer and tool presentation.
Heading IDs and fragment links share the same scope prefix.
No new browser event handler, JavaScript state machine, or stylesheet abstraction is introduced here.
The second pass found no material client-tier or locality issue in these functions.

## Decisions retained

The native publication journal remains separate from the application's operation ledger.
It records remote draft and upload progress, while the application ledger records authority and outcomes.
Combining them would conflate distinct recovery facts.

The test publication interpreter runs the whole publication effect with a supplied receipt.
Its Prepare and Complete callbacks do not simulate every internal native upload stage.
Native command and application lifecycle tests must cover those stages separately.
This review does not treat a fake-interpreter pass as live GitHub upload acceptance.

The optional-project chart wrapper remains for existing callers.
Its project-bound implementation rejects a missing project before query execution.
Removing the wrapper would expand this change across unrelated chart callers.

## Evidence and remaining work

### Membership, secrets, and enrichment follow-up

The eligibility review follows changed permission queries and every activeMembership composition site.
Each supplies the required member, account, and project aliases.
Shared eligibility checks account, project, membership, and free-plan ownership together.
Session-only sudo fallback remains separate from member and Slack grants.
Permission order and current-role checks retain the existing viewer, editor, and administrator boundaries.

The secret review reads ProjectApiKeys completely and follows Git and Slack token callers.
New provider secrets use authenticated encryption with a nonce from the UUID effect.
Version, envelope, key, and authentication failures retain explicit SecretError cases.
The legacy API-key cipher and Git token fallback remain compatibility paths; new tokens use the authenticated format.
Slack credential reads remain bound to the requester, project, installation, identity, and current membership.

The enrichment review reads IssueEnhancement and its Types module completely.
It follows the worker, retained native proposal, operation scope check, issue lock, and atomic completion.
An inactive template makes no inference call. Observe produces retained findings without changing issue metadata.
Mutation, result, artifact, and operation receipt share the existing completion transaction.
The small Types module breaks the operation/enrichment import cycle rather than introducing another executor.

Two distillation, evasion, and locality passes find no additional material issue in this scope.
No runtime source changes are made. Four focused Linux cases pass in 1.1473 seconds.
They cover current account/project/plan eligibility, revoked authority, private demo evidence, and the enrichment lifecycle.
All ten scoped runtime/test inputs match the compiled Linux snapshot.
The exact scope, hashes, and test log are recorded in scripts/local/agent-membership-enrichment-review.json.

### Native execution follow-up

A further review reads CodeReview and AgentOperation completely, then follows role policies
and project-memory callbacks into the common operation ledger.
Two hs-distill, hs-evasion-review, and hs-lob-review passes find no additional material issue in this scope.
The source receipt is scripts/local/native-execution-review-source.json.

All task roles use the existing launch and retained-session interpreter.
Callback unlifting preserves their original effect rows; process IO stays inside that interpreter.
VerifiedFix, ReviewedFix, and ExplainedFix remain opaque and do not derive bypass constructors.
Independent approval binds the exact verified candidate and a distinct attempt.
Admission binds its exact artifact; resumed reproduction and comparisons are verified again.
Tools and phase/check receipts invalidate an accepted draft before another result can be returned.
Completed replay revalidates its retained result before starting another model process.

The paired checkpoint retains transcript, VM state, budgets, workflow, and application context.
A missing manifest beside retained files fails closed rather than starting a replacement session.
Snapshot pruning follows durable persistence and cannot revoke a saved result.
Admission and peer roles intentionally permit project wiki operations through current application authority.
Their legacy knowledge callbacks reject unsupported service-memory operations.
Operation identity excludes transport call IDs and wiki worker attempt IDs, while preserving the actual edit.
The ledger remains scoped to its project and shared run; author attribution is checked separately.
No browser behavior or additional client interpreter enters this path.

No source changes or additional test run are claimed for this follow-up.
The separate [workspace review](2026-11-01-workspace-interpreter.md) records the passing Linux application cases
and distinguishes their controlled provider/publication interpreters from live acceptance.

### Retention and job persistence follow-up

A further complete source read covers Outbox, Explanation, ReviewEvaluation, IssueFixes, AgentUsage, and ImpactReviews.
It follows their native interpreter callbacks, shared event-run context, current authority, media readers, and usage writers.
Two distillation, evasion, and locality passes find no further material issue within this scope.
No runtime source changes are made in this follow-up.

Receipt replay decodes the complete journal before callbacks and removes entries only after database acceptance.
Conflicting receipt identities remain rejected. Phase receipts retain their issue-fix scope.
The explanation cache binds its renderer, source result, regression, media hashes, and measured sizes.
Its reader compares the retained manifest before returning media bytes.
The evaluation interpreter retains rejected submissions and bounds source reads, output, and execution time.
Its test interpreter remains beside the production interpreter.

Issue-fix progress replays admitted steps into private reproduction and comparison proofs.
Leased persistence binds the current requester, source mapping, installation, revision, and independent review.
Completion updates the native result and shared routine run in the same transaction.
PR review events use the shared routine context and operation ledger for publication.
Usage writers bind attempts to their project and native run, preserve billing attribution, and reject conflicting replays.
The two short numeric Semigroup instances remain explicit; another generic wrapper would add machinery for little reduction.

Three focused Linux cases pass with zero failures in 1.3783 seconds.
They cover interrupted phase delivery, call attribution across billing changes, and frozen regression progress.
All six reviewed module hashes match the compiled Linux snapshot.
The receipt is `scripts/local/native-retention-followup-source.json`.
These focused checks do not replace whole-branch or live publication acceptance.

### Incident delivery and API follow-up

The complete Incidents module review follows event identity, thread identity, queued analysis, send leases, and history reconciliation.
Caller reads cover the incident worker, Slack delivery worker, report-authority evaluator, history reconciler, and MCP API adapter.
Two hs-distill, hs-evasion-review, and hs-lob-review passes find no additional material issue within this scope.
No runtime source changes are made in this follow-up.

Recorded incident events bind their project, source, issue, time, and destinations before analysis starts.
Each analysis uses the shared event routine, conversation, run, authority evaluator, and completion path.
Report delivery checks current routine authority again. Ordinary incident notifications remain separate from optional AI analysis.
An expired send lease becomes uncertain instead of permitting another blind send.
History reconciliation checks the publishing app, thread, root, and immutable delivery identity.
Late non-root history results require the current search lease.
Malformed stored timestamps roll back the claim rather than returning unchecked coordinates.

The current watcher passes all 118 incident-delivery and API examples in 31.0630 seconds.
They include paused and revoked report authority, forged context, interrupted delivery, replay, and real API/MCP route calls.
All 279 recorded Haskell and manifest inputs remain unchanged through the completed run.
An earlier five-case partial run was interrupted by a concurrent Integrations module save and is not counted as completed acceptance.
The source receipt is `scripts/local/incident-api-boundary-watch-source.json`.
The inspected function list and hashes are in `scripts/local/incident-delivery-boundary-review.json`.

The CI runner self-test also passes, including all 23 release tests and the capability/fingerprint assertions.
Its small local Docker fixture checks cache seeding; it does not build the production application image.
The command explicitly selects the local OrbStack context and builder.
The receipt is `scripts/local/unified-agent-ci-selftest-source.json`.
Final local signoff, application-image validation, and publication remain open.

### Shared wiki persistence follow-up

The complete ProjectMemory review follows its bounded text, relative paths, optimistic versions, project scope, and author tags.
Caller reads cover the member form, interactive agent operation, native review callback, and native fix callback.
The shared native write helper serves both native call sites and retains run attribution.
The common transaction guard checks current authority, evidence permission, and project audience before a wiki mutation.
Member forms parse bounded values and use the authenticated member as author.
The opaque path and bounded-text types expose no Generic constructor bypass.
Their JSON and database decoders use the same refinements.
Two scoped skill passes find no further material issue, and no runtime source changes are made.
The source receipt is `scripts/local/project-memory-boundary-review.json`; no additional test result is claimed for this source review.

### Operation, checkpoint, and audience follow-up

The complete Investigations module review follows immutable proposals, claim locks, selected review, operation completion, checkpoints, reply reservations, progress streams, and evidence audiences.
Caller reads follow transaction admission in AI, current run authority, and parsed acknowledgement publication in Slack.
Two scoped skill passes find no further material issue. No runtime source changes or additional test results are claimed.

Claims bind the current running attempt, requester, conversation, checkpoint revision, and pending call.
Mutation and receipt completion share one transaction; uncertain external outcomes remain explicit.
Review execution checks the current approver role and caller authority again.
Checkpoint updates reject stale revisions, and accepted follow-ups commit with their journal entry and signed receipt.
Legacy adoption retains Observe authority and stops unresolved mutations instead of replaying them.

Reply reservations precede HTTP publication. Production acknowledgement callers parse timestamps before confirmation.
Progress streams retain their exact previous state and canonical task identities.
Evidence permissions only increase, while private audiences intersect rather than widen.
Legacy source classification preserves unknown evidence as administrator-only.
The source receipt is `scripts/local/investigation-ledger-boundary-review.json`.

The separate migration audit compares all 295 local files with the isolated QA ledger.
Every checksum matches, with no missing or unknown file. No migration is run by this audit.
The branch also adds migration files without changing any migration already on master.
Its receipt is `scripts/local/unified-agent-qa-migration-audit.json`.

### Shared agent loop and discovery follow-up

The complete AI module review follows response parsing, discovery, requester grants, retained model context, tool execution, operation admission, and output preparation.
The first distillation pass localizes the single-caller Slack discovery helper and combines tool-name extraction beside its caller.
It also removes the schema pass-through wrapper. The change removes 14 lines.
The second pass finds no further material reduction within this module.

Discovery preserves grant refresh, cooldown handling, current-token checks, and effect order.
Tool loading preserves the retained request and appends only newly discovered definitions.
Global deduplication is rejected because it would also rewrite existing retained request entries.
The evasion and locality passes retain the opaque query/widget decoders, precise external effects, common operation guard, and local helper scope.
No warning suppression, dependency, or client-side behavior is added.

Both watchers compile the updated source. Pinned formatting, HLint, and whitespace checks pass.
The existing stream, audience, incident, and API selection passes 155 examples in 60.6209 seconds.
Four focused discovery examples then pass in 1.2608 seconds.
All 279 recorded Haskell and manifest inputs remain unchanged through the focused run.
The source receipt is `scripts/local/agent-discovery-locality-source.json`.
Final exact-tree release checks remain separate.

### Routine admission, completion, and retained artifacts follow-up

The complete routine/control block in Issues is read from its refined inputs through history seeding.
The review follows event admission, scheduled claims, authority refresh, atomic completion,
lease recovery, pause/cancel/resume, policy changes, and retained artifacts.
The adjacent enrichment transaction changes are covered by the enrichment review above.

Event recovery retains the original actor, input, instructions, policy, and permission ceiling.
Explicit restarts create a new request root; duplicate event identities reuse the admitted request.
Native incident admission binds the installed channel, event, episode, and current installation.
Scheduled claims retain one run for the scheduled slot and fence completion by the active lease.
Completion rechecks current authority before a project mutation, then commits the mutation,
result, message, artifacts, and terminal state in one transaction.
Uncertain external operations remain explicit; cancellation closes unstarted operations.

Two scoped distillation, evasion, and locality passes find no additional material issue.
The shared SQL fragments have multiple callers; their extraction removes real duplication.
Opaque RunContext and CommandOrigin constructors remain private, without a public Generic bypass.
No browser interaction is introduced by this model block.
No runtime source changes are made during this follow-up.
The existing routine selection passes 67 examples with zero failures in 19.2874 seconds.
All five recorded runtime/test inputs remain unchanged through that run.
The source receipt is `scripts/local/routine-control-boundary-review.json`.

### Route dispatch and job continuation follow-up

The wiring review follows the complete new job-admission module, agent dispatch,
conversation and incident execution, routine completion, retry exhaustion, and review wakeups.
The route review follows the changed endpoint declarations, full server dispatch,
API/MCP adapters, actor attribution, and authenticated conversation/media wiring.
Review decisions and their queue wakeup share one transaction.
The worker rechecks the stored request after taking its conversation lease.
Exhausted queued requests retain a visible terminal run rather than staying Running.

The first distillation pass removes three unused context arguments from the route constructor.
The existing effects still supply handler context; the OTLP closures retain their explicit inputs.
All five call sites use the smaller constructor. No route or wire format changes.
The repeated distillation, evasion, and locality passes find no further material issue here.
The WAI unlift remains at its existing application adapter, without a new domain IO escape.

Both watchers compile. All 164 selected API, repository, queued-conversation, and slash-command
cases pass in 42.9408 seconds; all 279 recorded inputs remain unchanged through that run.
Pinned formatting, runtime HLint, and whitespace checks pass.
The source receipt is `scripts/local/route-job-boundary-review-source.json`.

The source record is `scripts/local/agent-cross-module-review-source.json`.
No runtime source changed during this review, so no additional test run is claimed.
The preceding chart checkpoint passes 489 bot cases and 2,083 doctest examples.
Those results retain their own recorded input hashes and do not prove every boundary above.

A fresh fetch still finds `origin/master` as an ancestor of the local branch.
Draft PR 619 still refers to the earlier published head and reports conflicts there.
Publication waits for the remaining whole-branch reviews and local release checks.
Live chart delivery, native media transport, Linux/KVM acceptance, and abrupt host-loss
requirements remain separate acceptance work.

### Review convergence at the current source snapshot

The repeated source-review loop has no outstanding material finding at `a1748e155`.
This combines the routine, authority, catalog, persistence, native execution,
workspace, publication, adapter, and route reviews recorded above and in their linked reports.
The additional entry-point sweep checks service-reader bot adapters, member activity attribution,
shared response types, dashboard rendering reuse, sidebar audience filtering, and request finalization.
It retains the shared progress request set: cancelled and detached requests need their own identities.
The existing browser regressions cover failed, dropped, delayed, and cancelled requests.
No additional runtime change is justified by this sweep.

All exact local receipt paths checked in seven review reports exist.
The source index is `scripts/local/unified-agent-review-convergence.json`;
the receipt-reference check is `scripts/local/unified-agent-review-evidence-audit.json`.
The index is a review record, not an execution attestation.
Final simplification follows the remaining live acceptance and any resulting fixes.
Final exact-tree CI, the local production image, and PR publication remain required.

### Generated source manifest correction

The final metadata check fails because monoscope.cabal omits migrations 0281 and 0282
from its generated extra-source-files list. Running hpack adds those two existing files.
No migration contents or runtime source changes.
The existing make fmt-check hpack-check guard then passes.
Repeated distillation, evasion, and locality review retains generator ownership rather than a handwritten patch.
All 237 pinned files in the separate live crash probe remain unchanged.
Red and green receipts are `scripts/local/hpack-migration-manifest-red.json`
and `scripts/local/hpack-migration-manifest-green.json`.
This metadata check is not final CI or image signoff.


### Incoming master issue-lifecycle integration

Fresh master `2c15b4bc5` adds PR #760 after the earlier integration inventory. Its typed issue-detail/sample payloads, shared test-clock helpers, seven independent lifecycles, and HTML fixtures are integrated with the shared agent implementation. The upstream migration keeps its exact filename and bytes; hpack registers it beside the branch's existing migrations.

The merge retains the durable issue-chat request identity as a required typed field of IssueView. The found-page constructor, pure renderer, hidden form value, and shared request admission therefore keep one identity. The new issue lifecycle keeps the branch's current shared handler and optional request field for its non-member denial check. Existing durable artifact-write tests retain their operation-ledger setup; a cosmetic upstream helper rename does not replace them with uncheckpointed writes. Three imports explicitly exclude the new generic handler helper where those tests already name an operation call.

Two scoped hs-distill, hs-evasion-review and hs-lob-review passes retain the merged shapes. Typed views replace presentation-only handler output without a second page, executor, policy, or request-ID store. The test DSL reuses the existing clock and pool scope. Threshold comparison remains total over its two direction constructors. Production effects and authorization rows are not weakened; the clock interpreter change is in the existing test boundary. Renderers keep their existing HTML/htmx/hyperscript behavior. There is no warning suppression or applied-migration rewrite.

The first combined run passes 656 of 659 cases. Its three failures pin expected presentation differences: durable form identity and replacement semantics, plus the default API-change title before explicitly activated enrichment. Only those exact expectations are updated in three incoming fixtures; the remaining five fixtures stay byte-identical to master. The focused issue rerun passes all seven lifecycles in 6.1787 seconds. Pinned formatting and scoped runtime HLint pass with zero hints. The final combined rerun passes 659 examples with zero failures in 234.4043 seconds. The supported complete doctest target passes all 2,105 examples with zero errors or failures. All 557 recorded Haskell, package, migration and fixture inputs remain unchanged. Merge b418cea23 includes the fresh master head. The private source/result receipts are master-760-integrated-final-source.json, master-760-integrated-final-result.json, master-760-doctests.log, and master-760-final-checkpoint.json under scripts/local/. This scoped integration review does not establish final whole-branch or release signoff.

### Rebase onto the new master

The branch is rebased onto master 2c15b4bc5 with git rebase --rebase-merges.
All twenty selected incoming heads remain ancestors. Adapted #661 behavior remains in the preserved tree.
The isolated candidate resolves the typed issue view, request identity, artifact tests, and generated manifest together.
A tree audit removes unrelated formatting drift before adoption.

The final rebased commit 59f1d374c has exactly the verified integration tree from b418cea23.
All 557 recorded inputs remain unchanged, so the 659-case pass and 2,105 doctests retain their tested source.
Adoption updates the branch reference atomically without rewriting working files or the index.
Concurrent review-document edits remain intact.
The original integration history is retained at local branch codex/slack-agent-before-master-760-b418cea23.
The private receipt is `scripts/local/master-rebase-760.json`.
The later uncertain-append finding still needs its regression and fix before final release checks.


### Public and private Slack destination confinement

The existing sixteen-scenario evidence-read lifecycle now covers both public C and private G destinations, through command and threaded origins. The same assertions verify that hostile channel/workspace arguments cannot redirect a read, unsupported locators and broader searches fail before disclosure, and membership, consent, and citation failures retain their existing boundaries. Citation fixtures derive from the selected destination. No production behavior or authorization type changes.

All 64 controlled integration cases pass in 22.0914 seconds under seed 13421578. All four recorded source inputs remain unchanged. Two scoped hs-distill, hs-evasion-review and hs-lob-review passes find no further material issue: one lifecycle matrix owns the cases, typed audience retention remains before provider reads, and no client behavior or effect escape is added. Pinned formatting passes. The private receipt and log are `scripts/local/slack-destination-confinement-review.json` and `scripts/local/slack-destination-confinement-64-green.log`.

These tests interpret the provider boundary; they do not establish live public-channel acceptance. The earlier broad stream-test failure and final release gates remain open.


### Final source simplification after the rebase

The prior convergence covers 71 runtime inputs. A current hash comparison identifies six changed inputs: the integrated issue views, model, routes and test-clock helpers, plus Slack stream recovery. Those deltas are reviewed again together with the incoming stable anomaly ordering, giving 72 recorded runtime inputs. Two final scoped passes find no additional material distillation, evasion, or client-locality issue. The stream assertion uses the existing JSON traversal instead of a nested vector conversion. Typed issue views retain their required request identity; provider recovery retains the original stream, exact-state update, current authority checks, and acknowledged frame.

No installed simplify skill or command was found in the searched user and repository locations. A manual final simplification pass was performed; this record does not claim that `/simplify` ran. The current-source receipt is `scripts/local/unified-agent-final-review-convergence.json`. The broader watcher run now passes 692 examples with zero failures in 243.5714 seconds; all 557 recorded inputs remain unchanged. It includes the deterministic stream-closure regression and both destination classes. The source and result receipts are `scripts/local/slack-final-broad-source.json` and `scripts/local/slack-final-broad-result.json`. Committed-tree release gates remain outstanding. Earlier browser and native-runtime reports keep their original scope and limits.


### Dead helpers found by the final Weeder gate

The first final local run passes HLint, frontend, build, unit and CLI checks, then Weeder reports lookupVecBoolByKey and lookupVecIntByKey. The generic bot table renderer removed their only callers from master. These are orphans created by this branch, so both wrappers and exports are removed: ten lines, without changing the remaining generic/text lookups or suppressing the check.

Repeated scoped distillation, evasion and locality review finds no further change. The existing watcher passes all 28 selected queued-bot, row/chart and WhatsApp cases in 9.1129 seconds, with the recorded source unchanged; pinned formatting also passes. The focused Weeder rerun and final release checks remain required. The red guard, cleanup review and focused test proof are `scripts/local/unified-agent-final-weeder-red.log`, `scripts/local/weeder-orphans-review.json`, and `scripts/local/weeder-orphans-focused-bots-green.log`. The earlier 692-case result keeps its original source snapshot.
