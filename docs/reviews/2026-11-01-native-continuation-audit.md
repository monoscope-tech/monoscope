# Native continuation verification

Date: 1 November 2026.
Source commit: `53f48b667a392cd01cd5662a40f900b026d7b2db`.
This is a scoped review record, not release signoff.

## Integration boundary

`git fetch origin master` reports `3c34f3c83` as the current remote master.
That commit is an ancestor of this branch.
The published PR history is also part of the combined branch, as recorded in the integration plan.
Publication still requires final reviews and local signoff.

## Checkpoint review

The `hs-distill` and `hs-evasion-review` passes inspected the native continuation changes and their application callers.
The review included checkpoint identity, child attempts, restored artifact proofs, current authority, and cancellation.

| Contract | Source evidence | Remaining verification |
| --- | --- | --- |
| One request retains its original inputs | `FixContext` retains the original attempt, issue prompt, evidence window, model, evidence, and notes. | Application-job acceptance across the complete SDK continuation. |
| Child work remains attributable | Admission and peer checkpoints retain typed attempt IDs. `Usage.ownsAttempt` checks each restored attempt. | Broader native integration results for the current source. |
| Restored data cannot construct artifact proof directly | `CandidateSnapshot` decodes descriptors. `restoreCandidate` verifies retained files before constructing `CapturedCandidate`. | Corrupt metadata, patch, and bundle regressions in the native suite. |
| Access remains current | The producer resolves current repository credentials and checks the job lease before calls and completion. | Revocation regressions in the application-job suite. |
| Completed work avoids duplicate inference | The finished checkpoint revalidates the accepted result through the existing validator. | Finished-result replay in the native suite. |

The changed runtime uses raw IO inside its existing effect interpreter.
Its application callers retain the `CodeReview` effect and the existing fake interpreter.
The diff adds no warning suppression, unsafe effect escape, separate worker, or separate run ledger.

The `hs-lob-review` pass found no client behavior in this commit.
This result does not cover the complete branch interface.
The existing visual review and interface reports remain separate evidence.

HLint identified two redundant mappings in PR publication authorization.
Direct composition preserves the same authorization calls and typed failures.
The repeated scoped pass reports no hints.
The repository-pinned Fourmolu 0.19.0.0 accepts all four inspected runtime files.

## Verification session

`npm test` in `impact-review-worker` passes all 19 cases under Node 25.2.1.
The test watcher compiles 241 modules from the current worktree.
The worktree also contains concurrent edits to the candidate parser and the visual review.
Those edits are preserved.

The first native-suite attempt failed during setup because the local database was unavailable.
Its 47 setup failures do not establish native behavior.
OrbStack and the existing `monoscope` PostgreSQL container are now running.
The local MinIO service is also running through `make tmux-minio-local`.
The same test watcher reruns `--match "Pages.ImpactReviews"` through `.test-args`.
Its completed result must replace this pending status before publication.
An interrupted run passed ten cases before the authorization simplification triggered a reload.
Those ten cases do not establish completion of the full suite.

The production-configured server compiles but cannot authenticate to its configured database.
Compilation does not establish live application acceptance.

## Release boundary

Whole-branch Haskell and interface reviews, doctests, Linux/KVM acceptance, local signoff, and the canonical PR update remain open.
Controlled acceptance uses only the Monoscope Slack workspace.
Streaming requires real interruption and ambiguous-start recovery evidence before implementation can pass its release boundary.
The current source does not establish that evidence.

The local image builder uses the `orbstack` endpoint and a Unix socket on this Mac.
Final image generation must explicitly select `DOCKER_CONTEXT=orbstack` and `MONOSCOPE_BUILDER=monoscope-local-amd64`.
No image build or final signoff started during this review.
