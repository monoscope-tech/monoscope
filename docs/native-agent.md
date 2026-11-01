# Native review and fix design

The native worker investigates repository changes and reported issues through retained SDK and guest sessions.
Its implementation shares the agent authority evaluator, operation ledger, project wiki and retained artifact interface.
See the [unified design](../plans/2026-10-30-unified-agent.md) for cross-entry-point guarantees.

## Ownership

Haskell owns workflow policy, admission, repository identity, candidate verification and final acceptance.
`Data.Effectful.CodeReview` runs the SDK child and validates its protocol results.
`Data.Effectful.AgentWorkspace` owns guest commands and resource lifetime.
The Node adapter owns the Pi model session, metering and tool forwarding.
The guest executes repository file and shell operations as UID 1000.
Model tools cannot use the private supervisor protocol or start a replacement VM.

The public CLI depends on the DB-free shared package.
Server-side comparison commands belong to `monoscope-server`, because they require server policy and effects.
The [adapter README](../impact-review-worker/README.md) describes setup and the acknowledged wire protocol.
The [operations reference](agent-operations.md) describes caches, checkpoints, images and storage.

## Review

PR review binds the project, repository, base/head revisions, service and telemetry window.
Findings retain source evidence and production evidence separately.
Source-only evaluation cannot claim telemetry support or use project wiki tools.
Bounded-patch and checkout-harness comparisons use the production review interpreter.
Their private scoring rubric stays outside model input.
Provider failures and unknown costs remain explicit in comparison results.

A successful structured provider response passes the caller's validator before acceptance.
A rejected draft cannot become success in either the production or test interpreter.
Knowledge writes use the shared operation ledger and current application authority.
Historical service-memory callbacks cannot bypass unsupported-operation rejection.

## Fix workflow

1. Admit a bounded reproduction against the pinned repository revision.
2. Reproduce the issue before source edits are enabled.
3. Retain the admitted tests and lock their identity during candidate work.
4. Capture the candidate commit and artifacts through native host operations.
5. Verify the exact candidate and regression artifacts.
6. Run independent review with a distinct attempt bound to that candidate.
7. Retain the explanation, media and package before preparing publication.

Admission carries a nonblank bounded rationale.
`VerifiedFix`, `ReviewedFix` and `ExplainedFix` remain opaque proofs.
A peer decision cannot approve a different candidate or replace its evidence manifest.
Host validation checks paths, sizes, digests and exact artifact identity.
The production workflow does not trust a model's claim that a command or verification succeeded.

Issue jobs preserve their original requester and administrator ceiling through their shared run link.
Current project eligibility and membership still apply to listing, playback and publication.
An account or project restriction cannot be bypassed through an older retained result.
PR publication uses the existing GitPublication capability and retained reconciliation journal.
Slack activation alone grants no code-publication or deployment permission.

## Checkpoint and process lifetime

A checkpoint pairs the SDK transcript, VM snapshot, budgets and application phase.
Persistence precedes snapshot pruning.
A retained manifest is required to resume existing files; missing or conflicting state fails closed.
Recovery keeps the original shared run and attempt identities.
Completed replay validates the retained result without another inference call.

The SDK shuts down before guest release.
The outer exchange also closes the child when a callback suspends before checkout.
Graceful Close, input EOF and process exit stay inside the existing cleanup deadlines.
The process-group finalizer remains responsible for forced cleanup.
No cleanup exception is converted into a successful receipt.

External actions require operation-specific reconciliation after an uncertain acknowledgement.
A claimed operation row does not prove universal exactly-once execution.
Uncheckpointed host loss can fail closed rather than resume.

## Results and verification

Run inspection, explanations, generated media and protected downloads appear inside the conversation.
Project wiki revisions retain the actual writing attempt and shared run.
Usage records distinguish model calls, missing receipts, verifier checks, workspace allocations and retained media.
Cost estimates remain separate from customer charges.

The [consolidated review](reviews/2026-11-01-agent-cross-module-review.md) records current tests, KVM acceptance and source limits.
Earlier prototypes, model comparisons and implementation diaries remain available in Git history.
They are not required runtime inputs or current acceptance claims.
