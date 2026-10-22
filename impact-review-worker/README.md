# Self-hosted Pi/Gondolin adapter

Monoscope runs Pi on the host and repository tools inside a local Gondolin microVM. Model APIs may be hosted; checkouts, sandboxes, evidence and orchestration run on our infrastructure.

The pinned libraries are `@earendil-works/pi-coding-agent` 1.4.0 and `@earendil-works/gondolin` 0.13.0. Haskell owns workflow policy and every production agent VM; this directory keeps the Pi session, the Gondolin SDK bridge and research entry points. The [ownership plan](../docs/plans/2026-09-30-pr-impact-review.md#haskell-ownership-revised-implementation-direction) records the target and remaining work.

## Setup

Use Node 22.19 or newer, Git, the current Monoscope CLI and a supported local hypervisor. Linux needs hardware virtualization. Image export also needs Docker or Podman, `cpio`, `lz4` and `e2fsprogs`.

```sh
npm ci --ignore-scripts
npm run build:guest
```

Build the native CLI with `make cli-build`. Set `IMPACT_REVIEW_NATIVE_CLI` if `monoscope` is not on PATH. The optional Monoscope compiler image and prepared-cache/S3 operations are documented in the [operations reference](../docs/plans/2026-10-22-agent-worker-operations.md#reuse-a-prepared-project-workspace).

## Runtime boundary

`cli.mjs` is a child of the Haskell CodeReview interpreter; set `IMPACT_REVIEW_AGENT` to its absolute path. The server owns every agent VM. It resolves `IMPACT_REVIEW_GUEST_IMAGE`, leases a prepared checkpoint when `IMPACT_REVIEW_PREPARE_SCRIPT` (and optionally `IMPACT_REVIEW_DEPENDENCY_ARCHIVE`) is configured, and creates or resumes the VM through `sandbox.mjs`. The server process therefore needs the hypervisor and any `GONDOLIN_*` settings, such as `GONDOLIN_VMM=krun` on macOS.

The child runs only Pi: the model session, metering and skill discovery. Pi's read/bash/edit/write calls reach Haskell as `Guest` operations and run as UID 1000 with a reset environment. Role tools (regressions, reproduction, candidate capture, verification, telemetry, wiki) are Haskell handlers. Haskell also supplies the role's prompt, tool schemas, built-in tools and submission envelope, and records phases, checks and VM allocation. The child cannot clone, fetch or start a VM.

The VM has no network, private COW writes and root-only host mounts; the agent works on guest-owned copies of the checkout and skills. Model credentials stay on the host. Admission and independent review are read-only. Fixing requires host-admitted reproduction before source edits.

The provider wrapper waits for Haskell's durable Call acknowledgement before invoking `ModelRuntime.streamSimple`. Completed usage, including compaction responses, is correlated with that start. Keep this fail-closed boundary during migration: exceptions from Pi's `before_provider_request` hook are caught by Pi and do not by themselves prevent a request.

Stderr is diagnostic only. Stdout is the acknowledged JSON-lines protocol. The child must preserve cancellation, VM cleanup, bounded output, receipt delivery and final host acceptance.

## Verification

Use the repository's long-lived Haskell test watcher:

```sh
printf '%s\n' '--match "Production impact reviews" --seed 478291220' > ../.test-args
```

The suite uses real local VMs and a deterministic loopback provider. Guest/profile/MinIO prerequisites are described in the operations reference; pending cases are not successful validation.

`evaluate.mjs` and `reviewbench.mjs` remain JS research entry points that start their own VM through `review.mjs` and `workspace.mjs`. They take review criteria and tool contracts from the host policy in their Haskell-prepared checkout metadata (the `Calibration` role for ReviewBench). Their recorded identities include native artifact-verifier and policy source. Historical operational and evaluation details are in [the plan](../docs/plans/2026-09-30-pr-impact-review.md) and [workflow record](../docs/plans/2026-10-20-issue-fix-workflow.md).
