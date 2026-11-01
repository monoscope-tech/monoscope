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

The source record is `scripts/local/agent-cross-module-review-source.json`.
No runtime source changed during this review, so no additional test run is claimed.
The preceding chart checkpoint passes 489 bot cases and 2,083 doctest examples.
Those results retain their own recorded input hashes and do not prove every boundary above.

A fresh fetch still finds `origin/master` as an ancestor of the local branch.
Draft PR 619 still refers to the earlier published head and reports conflicts there.
Publication waits for the remaining whole-branch reviews and local release checks.
Live chart delivery, native media transport, Linux/KVM acceptance, and abrupt host-loss
requirements remain separate acceptance work.
