# Self-hosted Pi/Gondolin adapter

Monoscope runs Pi on the host and repository tools inside a local Gondolin microVM. Model APIs may be hosted; checkouts, sandboxes, evidence and orchestration run on our infrastructure.

The pinned libraries are `@earendil-works/pi-coding-agent` 1.4.0 and `@earendil-works/gondolin` 0.14.0. This directory still contains workflow policy being moved to Haskell. The [ownership plan](../docs/plans/2026-09-30-pr-impact-review.md#haskell-ownership-revised-implementation-direction) records the target and remaining work.

## Setup

Use Node 22.19 or newer, Git, the current Monoscope CLI and a supported local hypervisor. Linux needs hardware virtualization. Image export also needs Docker or Podman, `cpio`, `lz4` and `e2fsprogs`.

```sh
npm ci --ignore-scripts
npm run build:guest
```

Built guests initialize loopback names in `/etc/hosts`, so offline tools can resolve `localhost`. Rebuild guest assets to apply this to existing images; checkpoint identity includes the guest build ID.

Build the native CLI with `make cli-build`. Set `IMPACT_REVIEW_NATIVE_CLI` if `monoscope` is not on PATH. The optional Monoscope compiler image and prepared-cache/S3 operations are documented in the [operations reference](../docs/plans/2026-10-22-agent-worker-operations.md#reuse-a-prepared-project-workspace).

The optional Linux `impact-worker` image packages the native CLI and SDK runtime. See [the operator setup](../docs/plans/2026-10-22-agent-worker-operations.md#optional-linux-agent-image) for KVM, guest images and persistent storage.

## Runtime boundary

`cli.mjs` is a child of the Haskell CodeReview interpreter. Set `IMPACT_REVIEW_AGENT` to its absolute path and select an immutable guest image with `IMPACT_REVIEW_GUEST_IMAGE`. Haskell supplies role, prompts, tool schemas, pinned revisions, checkout metadata, private receipt paths and callbacks. Telemetry/wiki tools pass through the existing protocol and are authorized by Haskell. Candidate Git commits, bundles, admission records and fix-phase receipts are created natively. Haskell journals and acknowledges each phase before the SDK can advance. The child cannot clone or fetch application source as a fallback.

Haskell owns the fix state machine: admission, reproduction, source unlocking, candidate capture, verification and submission. The shared runner routes SDK tools into the VM. `fix.mjs` adapts restart and completion callbacks. Shell and mutation tools run as UID 1000. Reads use Gondolin's filesystem access, including the read-only skill mounts. Only the private host protocol can request arbitrary supervisor commands.

`run_regression` and `reproduce_issue` accept `testFiles: [{path, content}]` with complete new or changed regular files. Haskell generates their Git diff against the pinned base, preserving existing executable modes. Supply 1–100 unique repository-relative paths and at most 256 KiB of combined UTF-8 content; the generated patch has the same byte limit. Symlinks, directories, traversal and unchanged entries are rejected. Legacy patch inputs remain supported, but cannot accompany `testFiles`. Admission, frozen-test fingerprints and verification still apply.

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

The Haskell comparison runner is `monoscope-server evaluate-impact [MODE] CASE.json RESULT.json`. Select `bounded-patch` or `checkout-harness` to run one mode; omit the selector to run both. Run it from the matching Monoscope source checkout, with `IMPACT_REVIEW_AGENT`, `IMPACT_REVIEW_USAGE_DIR`, `IMPACT_REVIEW_CACHE_DIR` and the guest/model configuration above. It uses the production CodeReview interpreter for the selected modes. Results use `native-impact-evaluation/v1`, record source/skill hashes and durable call/usage receipts, and reject an existing output file. The private rubric stays outside model input. Provider failures remain failed comparisons with unknown costs where usage is missing.

The command belongs to the server binary because it reuses server validation and effects; the public `monoscope` CLI stays DB-free. The native source-review comparison is `monoscope-server evaluate-source CASE.json RESULT.json`; insert `bounded-patch` or `checkout-harness` before the paths to run one mode. Its case has `projectId`, local `repository`, `base`, `head`, `provider`, `model`, and `pullRequest: {repo, pr_number, title, body}`. It returns a private `native-source-evaluation/v1` report with the original source-finding fields and measured receipts. This replaces the unpublished `reviewbench.mjs`/`RB_*` launcher; it is not the official ReviewBench submission envelope. Source mode has no telemetry or wiki tools and protects its checkout before inference. Production impact findings retain their existing evidence requirements. Historical evaluation records are in [the plan](../docs/plans/2026-09-30-pr-impact-review.md) and [workflow record](../docs/plans/2026-10-20-issue-fix-workflow.md).
