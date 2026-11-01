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

The source record is `scripts/local/agent-cross-module-review-source.json`.
No runtime source changed during this review, so no additional test run is claimed.
The preceding chart checkpoint passes 489 bot cases and 2,083 doctest examples.
Those results retain their own recorded input hashes and do not prove every boundary above.

A fresh fetch still finds `origin/master` as an ancestor of the local branch.
Draft PR 619 still refers to the earlier published head and reports conflicts there.
Publication waits for the remaining whole-branch reviews and local release checks.
Live chart delivery, native media transport, Linux/KVM acceptance, and abrupt host-loss
requirements remain separate acceptance work.
