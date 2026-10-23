# Self-hosted Pi/Gondolin adapter

Monoscope runs Pi on the host and repository tools inside a local Gondolin microVM. Model APIs may be hosted; checkouts, sandboxes, evidence and orchestration run on our infrastructure.

The pinned libraries are `@earendil-works/pi-coding-agent` 1.4.0 and `@earendil-works/gondolin` 0.14.0. This directory still contains workflow policy being moved to Haskell. The [ownership plan](../docs/plans/2026-09-30-pr-impact-review.md#haskell-ownership-revised-implementation-direction) records the target and remaining work.

## Setup

Use Node 22.19 or newer, Git, the current Monoscope CLI and a supported local hypervisor. Linux needs hardware virtualization. Image export also needs Docker or Podman, `cpio`, `lz4` and `e2fsprogs`.

```sh
npm ci --ignore-scripts
npm run build:guest
```

Build the native CLI with `make cli-build`. Set `IMPACT_REVIEW_NATIVE_CLI` if `monoscope` is not on PATH. The optional Monoscope compiler image and prepared-cache/S3 operations are documented in the [operations reference](../docs/plans/2026-10-22-agent-worker-operations.md#reuse-a-prepared-project-workspace).

## Runtime boundary

`cli.mjs` is a child of the Haskell CodeReview interpreter. Set `IMPACT_REVIEW_AGENT` to its absolute path and select an immutable guest image with `IMPACT_REVIEW_GUEST_IMAGE`. Haskell supplies role, prompts, tool schemas, pinned revisions, checkout metadata, private receipt paths and callbacks. Telemetry/wiki tools pass through the existing protocol and are authorized by Haskell. Candidate Git commits, bundles, admission records and fix-phase receipts are created natively. Haskell journals and acknowledges each phase before the SDK can advance. The child cannot clone or fetch application source as a fallback.

Haskell owns the fix state machine: admission, reproduction, source unlocking, candidate capture, verification and submission. `fix.mjs` adapts SDK file tools, bounded VM commands, file copies and cancellation. Only the private host protocol can request supervisor commands; model file and shell tools run as UID 1000.

The VM has no network, private COW writes and read-only host mounts. Agent commands run as UID 1000 with a reset environment. Model credentials stay on the host. `sandbox.mjs` is the SDK command bridge used by native workspace operations.

Haskell prepares admission patches through the private VM command bridge before model work. Artifact limits, patch application, file-count bounds and workspace locking remain native policy. Admission and independent review are read-only. Fixing requires host-admitted reproduction before source edits. Haskell supplies the peer’s candidate identity and exact evidence-file manifest from verified artifacts. The common SDK runner copies those bytes into the read-only VM and submits its decision through host validation. Peer review does not reopen artifacts or clone a second mirror.

The provider wrapper waits for Haskell's durable Call acknowledgement before invoking `ModelRuntime.streamSimple`. Completed usage, including compaction responses, is correlated with that start. Keep this fail-closed boundary during migration: exceptions from Pi's `before_provider_request` hook are caught by Pi and do not by themselves prevent a request.

Stderr is diagnostic only. Stdout is the acknowledged JSON-lines protocol. The child must preserve cancellation, VM cleanup, bounded output, receipt delivery and final host acceptance.

## Verification

Use the repository's long-lived Haskell test watcher:

```sh
printf '%s\n' '--match "Production impact reviews" --seed 478291220' > ../.test-args
```

The suite uses real local VMs and a deterministic loopback provider. Guest/profile/MinIO prerequisites are described in the operations reference; pending cases are not successful validation.

`evaluate.mjs` and `reviewbench.mjs` remain research entry points until their Haskell replacements are verified. Their recorded identities include native artifact-verifier source. Historical operational and evaluation details are in [the plan](../docs/plans/2026-09-30-pr-impact-review.md) and [workflow record](../docs/plans/2026-10-20-issue-fix-workflow.md).
