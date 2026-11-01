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

| Contract | Source evidence | Verification evidence |
| --- | --- | --- |
| One request retains its original inputs | `FixContext` retains the original attempt, issue prompt, evidence window, model, evidence, and notes. | The new application-job case passes after all three interruptions, including a changed model configuration. |
| Child work remains attributable | Admission and peer checkpoints retain typed attempt IDs. `Usage.ownsAttempt` checks each restored attempt. | The application job retains exactly three attempts across retries. All 47 preceding native cases pass. |
| Restored data cannot construct artifact proof directly | `CandidateSnapshot` decodes descriptors. `restoreCandidate` verifies retained files before constructing `CapturedCandidate`. | The native suite passes corrupt metadata, patch, and bundle regressions. |
| Access remains current | The producer resolves current repository credentials and checks the job lease before calls and completion. | The preceding application-job case passes its account and role revocation regressions. |
| Completed work avoids duplicate inference | The finished checkpoint revalidates the accepted result through the existing validator. | The new case repeats the completed background job without another inference call. |

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
The same test watcher completed `--match "Pages.ImpactReviews"` through `.test-args`.
The preceding 47 examples passed in 307.1925 seconds, with no pending cases.
The source includes the candidate parser change committed as `d881b701b`.
An interrupted run passed ten cases before the authorization simplification triggered a reload.
Those ten cases do not establish completion of the full suite.

A new application-job acceptance case passes in 20.6481 seconds.
It interrupts admission, candidate capture, and independent review through the real SDK and local VM.
Its assertions require one routine run, three original model attempts, three comparison receipts, and a retained result artifact.
It also changes the configured model after interruption and requires the original request to retain its model.
The completed result returns without another inference call when the background job repeats.
GitHub token exchange and object retention use existing test interpreters.
The fixture does not establish live GitHub or S3 acceptance.
The fixture maps GitHub repository references to its temporary local repository.
Peer checkpoint lookup applies the same mapping as execution, preserving the real artifact identity contract.
The final lint cleanup uses the existing `whenLeft_` combinator.
Both application-job cases pass against that final source in 23.2736 seconds.
Scoped HLint reports no hints, and the repository-pinned formatter accepts the test module.

Repeated `hs-distill`, `hs-evasion-review`, and `hs-lob-review` passes found no further material issue in the new case.
It reuses the existing SDK fixture, background worker, effect interpreters, SQL helpers, and artifact reader.
Only test observation uses local IORefs. No runtime seam, warning suppression, alternate executor, or client behavior was added.
The expanded suite also passes: 48 examples, zero failures, no pending cases, in 338.0501 seconds.
That run includes the new application-job acceptance case from `1bf5f2573`.
The saved log is `scripts/local/native-shared-continuation-48-green.log` (ignored local evidence, not a release attestation).

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


## Atomic project-write authority

A deterministic regression changed authority between the cached check and the atomic claim.
Before the fix, wiki revisions still committed in all five cases: role loss, Observe policy,
a new review requirement, private audience, and a higher evidence requirement.

The transaction now reads current run authority after the ledger locks the run.
Project writes also require the current review policy and a compatible evidence scope.
A rejected check returns a typed `OperationAuthorityWithdrawn` result.
The transaction rolls back the claim and mutation together.
The caller uses the existing `AgentStopped` path so the worker can cancel the request.
Ordinary conversation evidence remains automatic under its existing capability.

The regression requires that stop result, no wiki revision, and no retained operation claim.
All 19 operation examples pass in 6.2257 seconds, including both atomic rollback cases and uncertain external outcomes.
The failing-first log is `scripts/local/atomic-wiki-authority-five-red.log`.
The final focused log is `scripts/local/atomic-wiki-authority-operations-19-green.log`.
These runs use the actual shared executor and database with the existing model and clock interpreters.

The guard reuses the existing authority query inside the transaction.
The original reader and transaction reader share one typed result decoder.
The ledger carries guard failure explicitly instead of treating it as a saved tool response.
No warning suppression, raw IO seam, client handler, or migration was added.
Pinned Fourmolu passes the changed authority source and regression module.
HLint reports no hints in `Issues.hs` and `AI.hs`.
All 61 selected-review examples pass in 17.0456 seconds.
The native application-job recovery case passes in 21.5561 seconds.
The PR checkout harness passes in 6.5214 seconds, including atomic wiki receipts and publication recovery.
Their logs are `scripts/local/atomic-wiki-authority-reviews-61-green.log`,
`scripts/local/atomic-wiki-authority-native-fix-green.log`, and
`scripts/local/atomic-wiki-authority-native-review-green.log`.
The scoped follow-up review finds no suppression, weaker contract, or duplicate executor.
No client behavior changed. Whole-branch reviews and final release checks remain open.
This scoped result does not establish whole-branch signoff or live Slack acceptance.

## Slack identity at the atomic write boundary

A further failing-first regression unlinks the Slack requester after the durable tool checkpoint and before the atomic claim.
Before the fix, the wiki revision committed despite that unlink.
The test uses real event admission, the shared agent loop, the database, and controlled model and clock effects.
It does not use a live Slack callback.

The operation path now retains its `AgentAccess` through retries and reconciliation.
Atomic model actions check that access inside the mutation transaction.
Slack access requires the same linked member, project, installed workspace, and minimum role.
The effect and transaction resolvers share one SQL query.
Native callers retain their existing member access; their admitted sources remain constrained by the native scope check.

The regression failed in 0.3008 seconds and passes in 0.3374 seconds after the fix.
All 20 operation cases pass in 6.3377 seconds, including the earlier five authority changes.
The existing assertion that rejected claims leave no operation receipt remains in place.
Logs are `scripts/local/atomic-slack-unlink-{red,green}.log` and
`scripts/local/atomic-slack-identity-operations-20-green.log`.
HLint reports no hints in the two changed runtime modules.
All 56 cases selected by `agentReview_` pass in 16.4971 seconds.
The real native application recovery case passes in 22.5406 seconds.
Their logs are `scripts/local/atomic-slack-identity-reviews-56-green.log` and
`scripts/local/atomic-slack-identity-native-fix-green.log`.
The PR checkout/wiki recovery case passes in 5.0798 seconds.
Its log is `scripts/local/atomic-slack-identity-native-review-green.log`.
The runtime fix and regression are committed as `cf2a56c6c`.
The broader bot suite passes all 460 examples in 173.5638 seconds.
The saved log is `scripts/local/atomic-slack-identity-bots-460-green.log`.
The input hashes in `scripts/local/atomic-slack-identity-bots-run-source.json` still match after the run.
This run includes the current streaming prototype and the scoped artifact-heading assertion.

Scoped `hs-distill` and `hs-evasion-review` passes retain the shared query and explicit access constructors.
There is no duplicate authority SQL, widened effect row, warning suppression, or new executor.
`hs-lob-review` finds no client behavior in this change.
Whole-branch review, live streaming acceptance, and final release checks remain open.

## Retained fix project eligibility

The native publication lifecycle now covers inactive projects and free-plan
members who are not the project's first member. Before the fix, both states
returned retained media, exposed the completed-fix listing, authorized native
publication, and returned HTTP 200 from the authenticated fix-page handler.
The expected result is no media or listing, `AccessRevoked` for publication,
and HTTP 404 from the page handler. The failing result records both states:
`[(False,False,Nothing,200),(False,False,Nothing,200)]`.

Migration `0282_issue_fix_project_eligibility.sql` updates the existing shared
access view. It adds the active-project and first-member free-plan predicates
used by routine membership eligibility. All existing administrator, account,
issue, repository, credential, and installation checks remain in the view.
Applied migrations `0237` and `0238` are unchanged. The checked remote master
ends at migration `0257`; this branch previously ended at `0281`.

The extended native lifecycle passes after applying the migration through the
normal test-template setup. It also verifies that the first member still reads
media on a free project and that restoring the original project plan restores
publication eligibility. Existing native upload, interruption, corrupt-media,
and revoked-account checks remain in the same lifecycle.

The failing log is `scripts/local/native-project-plan-eligibility-red.log`.
The first passing native case is retained in
`scripts/local/native-project-plan-eligibility-green-combined.log`; that combined
run contains two separate failing Slack citation regressions and is not a green
suite. Input hashes and the exact scope are recorded in
`scripts/local/native-project-plan-eligibility-source.json`.

Repeated scoped distillation, evasion, and locality checks retain one access view
and one existing lifecycle regression. No alternate permission store, raw IO
runtime seam, warning suppression, client behavior, or weaker capability was
introduced. Pinned Fourmolu and `git diff --check` pass. The full native suite,
whole-branch reviews, and release gates remain open.

The subsequent full native group passes together with all 32 Slack destination
and citation cases: 82 examples, zero failures in 370.1305 seconds. This includes
all 50 native cases and the permanent project-eligibility regression after the
temporary browser adapter was removed. The recorded native, model, test, and
migration inputs match their pre-run hashes. The saved log is
`scripts/local/native-project-eligibility-full-82-green.log`; hashes are in
`scripts/local/native-project-eligibility-full-source.json`. This closes the
native regression run at that source checkpoint. Whole-branch review, Linux/KVM,
and final release signoff remain separate requirements.

## Whole-application loss before a candidate checkpoint

The separate persistent database `monoscope_qa_native_hostloss_20261101` now exercises the real application worker, SDK, VM, and database with controlled model responses, GitHub token exchange, and object retention. At the interruption boundary, run `3cbfbead-9981-4e21-ae4c-19615fe01a2a` has committed one wiki revision under its actual shared run, retained one regression check and its admission, and successfully edited `/workspace/runtime.sh`. The next model request is held open. The candidate transcript exists but its checkpoint manifest does not.

After saving the database, artifact hashes, process ownership, and receipts, the test sends SIGKILL to the entire GHC application and its SDK/VM descendants. The original application PID is confirmed absent. The watcher restarts the application, and the original ten-minute lease expires without changing its deadline. The worker then refuses the missing native context before inference. The same four attempts, regression check, operation receipt, wiki revision, and fourteen retained files remain unchanged. Two of those attempts belong to earlier fixture-setup failures and are retained rather than erased. The job releases its lease and retains no fabricated result.

The automatic-completion oracle fails; the independent observer verifies the intended fail-closed contract. This is evidence for uncheckpointed whole-application loss, **not successful recovery**. The separate paired-checkpoint check below covers the recoverable case. Physical-host loss and the Linux application path remain open. The earlier child-process and cooperative-provider checks retain their narrower scope.

Private evidence is `scripts/local/native-hostloss-before-kill.json`, `native-hostloss-uncheckpointed-acceptance.json`, `native-hostloss-uncheckpointed-worker.log`, `native-hostloss-source.json`, and the full `native-hostloss-before-kill.dump`. The isolated fixture and its evidence remain available; the Slack fixture database is unchanged.

## Seal the native job lease construction boundary

The evasion review found that the hidden `Lease` constructor still had public `Generic` and `DecodeRow` instances. Generic JSON parsing or an arbitrary row decoder could therefore construct the claimed proof outside `claimJob`. The new rejection doctest first failed because the generic parser was available. The type now derives only `Show`; `claimJob` decodes the existing three-column SQL result as a tuple and constructs the private lease there. The SQL, owner checks, effect constraints, and bracketed lifetime remain unchanged. Runtime line count is unchanged, with no new module, dependency, migration, or client behavior.

Both construction rejection examples pass, and the complete supported doctest invocation passes **2,098 examples, zero errors or failures**. The two canonical native application lifecycles pass **2 examples, zero failures**, in 26.4531 seconds, including claim/authority/completion and retained SDK continuation. The initial decoder example used `:t`, which merely reported its constraint; the final example evaluates the expression so the compiler must supply an instance. That intermediate failed run remains recorded rather than presented as green.

Repeated scoped hs-distill, hs-evasion-review, and hs-lob-review passes found no further material issue. The tuple decoder reuses the established library, the private constructor is used only at the claim boundary, and no client behavior changes. Pinned Fourmolu, HLint, and whitespace checks pass. Source hashes and exact outcomes are `scripts/local/native-lease-constructor-source.json`, `native-lease-application-source.json`, and `native-lease-application-result.json`; logs use the same prefixes. These are scoped checks, not final whole-branch signoff. Concurrent workspace-effect changes require their own verification and the final full regression run.

## Whole-application recovery from a paired checkpoint

The separate `monoscope_qa_native_paired_20261101` fixture first saves a real paired SDK transcript, VM disk, and `FixWaiting` workflow checkpoint. It retains the committed wiki write, admitted regression, original main and admission attempts, and guest source edit. The worker then resumes active work; the next provider request is held open before another tool runs. The entire GHC application and its live SDK/VM descendants are killed with SIGKILL.

After the watcher restarts and the original lease expires naturally, run `f7a62f8b-1a11-4a30-b168-a4753c36c0c8` succeeds. Both original attempts and the regression receipt survive unchanged. One new independent peer attempt brings the total to three; the final check count is three. There is still exactly one wiki revision and one operation receipt, with the wiki author pointing to the actual shared run. Completed-job replay performs no inference. The original application PID is absent, and all recorded runtime, image, and private harness hashes match. The application case passes **1 example, zero failures**, in 563.7112 seconds, including the lease wait; the independent receipt observer also passes.

The source receipt is `scripts/local/native-hostloss-paired-source.json`. The before-kill database dump, process tree, paired checkpoint, and retained hashes use `native-hostloss-paired-before-kill.*`; the final evidence is `native-hostloss-paired-acceptance.json`, `native-hostloss-paired-result.json`, and `native-hostloss-paired-worker-green.log`. These are real application/SDK/VM interruption checks with controlled provider/GitHub exchange and object retention. They do not prove physical power-loss durability or the Linux application path. The [workspace review](2026-11-01-workspace-interpreter.md) records separate Linux/KVM backend acceptance.


## Native bridge drain before resource cleanup

The post-PR-761 combined run completes 860 examples with one failure and all 557 recorded inputs unchanged. The malformed-VM-identity case reports `waitForProcess: No child processes`; the exact-seed focused retry passes. This is an observed process-lifetime failure, not evidence that invalid identity was admitted.

The installed typed-process cleanup cancels its exit waiter before terminating a child. Its [upstream lifetime guidance](https://github.com/fpco/typed-process/issues/25) distinguishes waiting for normal exit from terminating a process that may still be finishing. The existing cleanup table now includes a bridge that acknowledges Close, drains for 250 ms, and records completion. The old runtime kills it before that marker; the six-case run fails that assertion. A first wait-only change also fails because the input pipe remains open. Both failed attempts are retained.

The final change closes input after the terminal Close acknowledgement and waits for the library's exit result inside the existing two-second deadline. It adds two runtime lines and uses existing imports. Rejection, timeout and pending-call cleanup retain their previous forceful path; the original action exception and process-group cleanup remain intact. No error is ignored, timeout widened, state added or dependency changed.

The rebuilt shared package is loaded by the consuming watchers through the documented restart procedure. All six allocation lifecycle cases then pass in 6.2274 seconds with unchanged recorded inputs. The freshly rebuilt CLI also passes the existing temporary packaging, strict signing, linkage and version checks; its source binary remains unchanged and the temporary package is removed. Two imports orphaned by PR #761 are removed. Repeated scoped distillation, evasion and locality reviews retain the small lifetime sequence and shared test table; runtime HLint has no hints.

The subsequent combined post-fix run passes all 861 examples in 616.8195 seconds with all 557 recorded inputs and the test focus unchanged. The supported doctest target passes all 2,105 examples with zero errors or failures and the same unchanged inputs. This broader pass includes the original malformed-identity case; it is stronger evidence than the isolated retry, without claiming elimination of every possible process-library race.

Fresh Linux binaries built locally in OrbStack then pass 15 selected application cases in 119.4542 seconds on the owned KVM VM. All 26 observed application guests use KVM with two CPUs and 1,024 MiB. The fresh CLI also verifies checkpoint reuse, a failed-base/passed-fix comparison and weakened-test rejection across five observed KVM guests; its linkage check passes. Linux doctests pass all 2,105 examples after correcting the private snapshot’s disabled package-environment setting to match the repository. All 557 recorded runtime inputs remain unchanged. This refresh does not widen the historical same-binary host-loss proof. Exact-tree image/signoff and PR publication remain open.

Private receipts are `scripts/local/master-761-integrated-result.json`, `native-allocation-echild-repro-result.json`, `native-cleanup-drain-red.log`, `native-cleanup-drain-first-fix-red.log`, `native-cleanup-drain-green-result.json`, `native-cleanup-cli-package.json`, `native-cleanup-hlint.log`, `native-cleanup-final-combined-result.json`, `native-cleanup-independent-doctests-result.json` and `linux-native-cleanup-acceptance.json`.

## Worker shutdown before checkout

Final Linux integration verification completes 1,695 examples with one failure and 25 pending. The retained-operation suspension case receives a generic execution failure instead of its original review exception. Its exact-seed Mac retry passes, which does not resolve the complete-run failure. No integration attestation is published, and both owned image runs are stopped before further implementation.

The existing case now also checks that its worker finishes a 250 ms EOF drain. That assertion fails on the old interpreter: graceful shutdown runs only after checkout, while this callback suspends before checkout. The initial worker exchange now uses the same existing close, bounded wait and forced-stop routine. Checkout-local shutdown still runs before releasing its guest. The fix adds three net runtime lines; it adds no state, dependency, timeout, exception suppression or alternate executor.

The original exception identity and deadline assertions remain. The extended case passes, followed by all 28 selected native review, fix, submission, session and allocation cases in 141.4886 seconds. All 590 recorded inputs remain unchanged. A fresh Linux binary also passes the original case with its recorded seed. Repeated scoped distillation, evasion and locality reviews find no further material change; pinned formatting and runtime HLint pass.

All 16 fresh Linux/KVM application cases then pass in 122.5319 seconds, with 26 actual KVM guests observed and all 590 recorded inputs unchanged. A separate v4 host-loss refresh now uses the current runtime and an isolated database; its captured checkpoint, original attempts, regression and wiki receipt precede an abrupt virtual-host kill. The same-binary restart and natural lease recovery remain in progress. Complete integration verification, affected release gates and the final image remain required.

Private evidence is `scripts/local/native-review-suspension-signoff-red.json`, `native-review-suspension-drain-red.log`, `native-review-suspension-drain-green.json`, `native-review-suspension-native-result.json`, `native-review-suspension-final-review.json`, `linux-native-suspension-binary.json`, `linux-native-suspension-acceptance.json` and `linux-hostloss-v4-kill.json`.
