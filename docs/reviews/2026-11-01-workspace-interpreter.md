# Native workspace interpreter review

Date: 1 November 2026. This review covers AgentWorkspace in full, its Guest adapter,
the shared identifier and Git modules, and their CLI/package boundaries.
Whole-branch review and release signoff remain open.

## Repeated reviews

The hs-distill pass replaces two copies of the guest privilege and environment prefix
with the existing agentCommand helper. Preparation and regression verification retain identical arguments.
The second pass finds no further material reduction within this scope.

The hs-evasion-review pass follows request decoding, workspace identity, local locks,
leases, allocation receipts, frozen tests, source fingerprints, comparison budgets,
checkpoint capture and restore, remote archive verification, pruning stages, and owner cleanup.
Raw filesystem, process, clock, UUID, and storage IO lives inside the Workspace interpreter.
The module supplies its production and test interpreters together.
The four changed shared modules retain the DB-free package boundary.

Revision, fingerprint, allocation, and frozen-file parsing retain their existing bounds.
Regression output distinguishes setup failure, reproduced failure, changed source,
timeout, output exhaustion, and cleanup failure.
Remote pruning requires verified archive identity and content before local deletion.
Unknown or invalid cache entries remain visible in maintenance reports.
Resumed sessions use private copies; verifier baselines remain separate from live edits.
Resource lifetime uses bracket, withFileLock, process scopes, and recorded stage recovery.
Cleanup failure does not produce a successful release receipt.

TestArtifact is a transport record, not an admission capability.
The hidden InspectAdmission setup is constructed through admissionSetup, which checks its bounds.
Retained evidence separately checks file identity, hashes, report rows, source and test fingerprints.
The exported transport constructor therefore does not grant admission or reviewed publication authority.
The second pass finds no further material evasion within these reviewed functions.

The hs-lob-review pass finds no browser behavior in this interpreter change.
The shared command helper keeps privileged process setup next to its existing implementation.
No new client state, event handler, interpreter, or resource manager is introduced.
A repeated pass finds no locality issue within this scope.

## Verification

The supported make cli-build succeeds after the shared-package change.
Each root watcher is reloaded once for that change; ordinary test focus uses .test-args.
The fresh Mac CLI passes packaging, signing, portable linkage, version, and help checks.
Its package record is scripts/local/workspace-review-cli-package.json.
Pinned formatting, HLint, and whitespace checks pass for AgentWorkspace.

The current supported doctest run passes **2,098 examples, 0 errors, 0 failures**.
It includes the concurrent native-lease constructor hardening.
Its log is scripts/local/native-lease-constructor-final-green.log.
The complete native review/fix group passes **50 examples, 0 failures, 348.4111 seconds**.
This covers real SDK/guest execution, cancellation, frozen comparisons, workspace preparation,
dependency reuse, cache maintenance, retained sessions, shared authority, wiki receipts, and publication recovery.
The log is scripts/local/workspace-review-native-50-green.log.
All 205 final recorded source hashes still match after the run.

## Integration and environment

A fresh Git fetch, remote branch query, and GitHub branch API agree on master at 3c34f3c83.
That commit is already an ancestor of this branch.
All twenty selected PR heads remain included, along with the adapted #661 head.
The current inventory contains 28 open pull requests.
Draft PR 619 still points to its older published head; no publication occurs during this review.

Three detached GHC processes from earlier watcher sessions are removed from this checkout.
Two were consuming about one CPU core each.
Before termination, each has parent PID 1, the exact checkout as its working directory,
no listening TCP port, and no live child process.
The active watchers and tests keep running.
The private cleanup receipt is scripts/local/workspace-orphan-ghc-cleanup.json.

OrbStack is local, but its Linux host has no /dev/kvm device.
A separate local Lima VM provides real ARM64 Linux/KVM for the remaining acceptance work.
It has four CPUs, eight GiB of RAM, and a 64 GiB sparse disk.
Its tools and image are checksum-verified. Project and home directories are not mounted;
the only shared runtime mount is Rosetta.
All twenty worker tests pass on Node 25.2.1 in Linux.
The pinned Gondolin 0.14.0 guest boots and restores a retained edit under UID 1000.
Both observed QEMU processes use KVM, CPU host, two CPUs and 1024 MiB, with an open /dev/kvm descriptor.
The inner guest has no host mounts or network.
The receipt is scripts/local/linux-kvm-checkpoint.json.
The current Linux ARM64 CLI also builds through make cli-build in the local OrbStack container.
It reuses the shared Cabal and build-cache volumes; it does not generate an application image.
All 28 staged Haskell and Cabal inputs match this checkout, and the transferred binary hash matches its receipt.
Through that binary, preparation checks UID 1000 and retains a build artifact in its verified baseline.
Loading returns the same checkpoint hash.
The frozen comparison fails on base and passes on the repaired head.
A weakened candidate test is blocked.
All five observed CLI guest processes use KVM and an open /dev/kvm descriptor, with two CPUs and 1024 MiB.
The result and build logs are scripts/local/linux-kvm-cli-acceptance.json and linux-kvm-cli-build.log.
This verifies the Linux SDK and shared CLI interpreter.
Application image generation remains on the explicitly selected local OrbStack builder.

## Linux application checkpoint

The isolated Linux application snapshot compiles locally in OrbStack through the supported doctest target.
All **2,098 doctest examples pass**, with zero errors or failures.
The first harness disabled GHC environment files, preventing doctest package discovery.
Restoring the project default fixes that harness failure.

The resulting ARM64 integration binary runs inside the local Linux VM against local test services.
Seven PR-review recovery cases and two application fix-job cases pass:
**9 examples, 0 failures, 110.2487 seconds**.
These exercise application jobs, retained sessions, authority, receipts, and completion through the real SDK and guest.
The model provider and publication interpreter are controlled test implementations; this is not live GitHub publication acceptance.
The observer records 26 QEMU processes using KVM, CPU host, two CPUs, 1024 MiB, and an open KVM descriptor.
The transferred binary hash matches the local build receipt.

The first application run passes all seven review cases but fails both fix cases.
The VM lacks FFmpeg, Graphviz, and HOST_URL, which CI already supplies.
Installing those tools and setting the absolute origin produces the passing rerun above.
Both failed logs remain available; no runtime or test assertion changes make the rerun pass.
The acceptance receipt is scripts/local/linux-kvm-application-acceptance.json.
Its source comparison finds only concurrent Slack Utils and WorkflowsSpec changes after the snapshot.
Native application and interpreter inputs still match; this result does not validate those later Slack edits.

## Remaining work

Finish remaining host-loss acceptance and live scenarios,
the complete branch review loop, final simplification, local release gates, and the PR update.
