# Native agent operations

Use the [adapter README](../impact-review-worker/README.md) for SDK/guest setup and the acknowledged process protocol.
Build the public CLI with `make cli-build`.
Current workspace commands use the shared Haskell implementation; prototype JavaScript launchers are removed.

## Workflow and recovery

Haskell owns admission, repository identity, candidate verification, independent review and publication.
The Node adapter owns the Pi model session, metering and tool forwarding.
Repository tools run as UID 1000 inside the Haskell-owned guest.
Model tools cannot use the private supervisor protocol or start another VM.
The public CLI depends only on the DB-free shared package.

PR reviews bind the project, repository, base/head revisions, service and telemetry window.
Findings distinguish source evidence from production evidence.
Source-only evaluations have no telemetry or wiki tools.
The comparison runner keeps its scoring rubric outside model input.
Every interpreter applies the caller's response validator before accepting a result.
Failed calls and unknown costs remain explicit.

A fix follows these steps:

1. Admit a bounded reproduction against the pinned revision.
2. Reproduce the issue before source edits become available.
3. Freeze the admitted tests and retain their identities.
4. Capture the candidate commit and artifacts through native host operations.
5. Verify the exact candidate and regression artifacts.
6. Run independent review in a distinct attempt bound to that candidate.
7. Retain the explanation, media and package before publication.

`VerifiedFix`, `ReviewedFix` and `ExplainedFix` are opaque proofs.
Admission requires a nonblank bounded rationale.
Host verification checks paths, sizes, digests and artifact identities.
A model's claim that a command succeeded does not establish success.
Wiki writes and publication use shared operation records and current application authority.
Issue jobs retain their original requester, administrator ceiling and shared run.
Slack activation grants no code-publication or deployment authority.

A session checkpoint pairs the SDK transcript, VM snapshot, budgets and workflow phase.
The worker saves its manifest before pruning old snapshots.
Missing or conflicting retained state stops execution before another inference call.
Recovery preserves the original run and attempt identities.
Completed replay verifies the retained result without another inference call.
Uncheckpointed host loss can fail closed instead of resuming transient edits.
Uncertain external actions require operation-specific reconciliation.
An operation claim does not establish universal exactly-once effects.

The SDK closes before guest release, including suspension before checkout.
Close acknowledgement, input EOF and process exit share the existing cleanup deadline.
The process-group finalizer terminates remaining children and retains cleanup failures.
Run evidence, media and protected downloads stay inside the conversation inspector.
Wiki revisions retain the actual writing attempt and shared run.
Usage distinguishes model calls, missing receipts, verifier checks, workspace allocations and retained media.
Cost estimates remain separate from customer charges.

## Prepare a project workspace

Use a private request containing `projectId`, a credential-free `repository` and its exact `base` revision.
Keep the trusted preparation script outside customer checkouts.
Dependencies come from the pinned guest image or an explicit host-owned archive; preparation has no network credentials.

```sh
monoscope workspace prepare \
  --request /private/baseline.json --checkout /private/checkout \
  --script /private/prepare-project.sh --image /private/guest \
  --cache /private/agent-cache --bridge /path/to/impact-review-worker/sandbox.mjs
```

Set `IMPACT_REVIEW_PREPARE_SCRIPT` and `IMPACT_REVIEW_CACHE_DIR` to those same inputs on the worker.
For explicit dependencies, add `--dependencies /private/dependencies.tar` and set `IMPACT_REVIEW_DEPENDENCY_ARCHIVE`.
A configured archive requires a matching prepared workspace; a miss stops before inference.
Checkpoint identity binds the project, repository, base, image, script and dependency bytes.
The loader checks the disk digest and uses a private copy-on-write overlay.
Running processes and watchers do not survive disk-only checkpoints.

## Native checkpoint archive and restore

`monoscope workspace archive` and `monoscope workspace restore` use the existing Haskell MinIO SDK. Supply the same `--request`, `--cache`, `--image`, `--bridge`, `--script` and optional `--dependencies` arguments as workspace preparation, plus `--remote /etc/monoscope/workspace-store.json`. This is an operator interface; the agent receives no storage credentials or remote-storage tool. Existing jobs do not automatically archive or restore a cache miss yet.

The private JSON configuration requires `endpoint`, `bucket`, `region`, `accessKey`, `secretKey`, and `maxBytes`. Use an explicit endpoint on your servers and an existing private bucket. `maxBytes` limits one checkpoint's logical size; it is not a total disk quota. For example:

```json
{
  "endpoint": "https://minio.internal:9000",
  "bucket": "monoscope-private",
  "region": "us-east-1",
  "accessKey": "REPLACE_WITH_HOST_CREDENTIAL",
  "secretKey": "REPLACE_WITH_HOST_SECRET",
  "maxBytes": 536870912
}
```

Keep this file outside repositories with mode `0600`. Endpoint user info, query strings and fragments are rejected; TLS verification remains enabled. The CLI reads bounded regular files and never sends this configuration to Gondolin. It does not create a bucket or select a managed endpoint.

Objects use `agent-workspaces/v1/PROJECT/IDENTITY/`. The identity includes repository, pinned base and image build, preparation-script digest, and optional dependency archive digest/size. Archive verifies the local entry, streams the disk to `DISK_SHA256.qcow2`, checks the stored size, then publishes the digest-associated `DISK_SHA256.json` manifest followed by `manifest.json`. A request with `expectedCheckpoint` restores through its digest manifest, independently of the latest pointer. The disk hash is not an S3 ETag.

Restore verifies bounded metadata and the exact identity before downloading. It enforces both the configured size limit and declared byte count, checks SHA-256, inspects the checkpoint's image ID, and normalizes the manifest's image directory to the current host. It publishes a synced local directory only after these checks pass. Failure removes temporary restore files. A valid local hit avoids remote transfer; a corrupt local entry fails closed. The pinned base image must already be available on the destination host. No guest RAM, process or watcher state is restored.

Transfers retain private project-scoped records in `CACHE/PROJECT/transfers/`. Each records identity, direction, start time, and started/completed/failed outcome. Completion includes checkpoint bytes/digest and elapsed time; failed or interrupted transfers have unknown transferred bytes. All are explicitly unpriced. These records are not yet ingested into customer usage summaries and do not establish storage byte-time, total network traffic or charges. SIGKILL can leave a started record and temporary files; orphan reconciliation and automatic cleanup remain pending. Remote blob/multipart retention is also not configured by this command.


## Native checkpoint eviction

Run `monoscope workspace prune --request /private/prune.json --cache /srv/monoscope/cache --remote /etc/monoscope/workspace-store.json`. The request is `{"projectId":"PROJECT_UUID","maxBytes":536870912}`; zero is allowed when all idle archived entries should be considered. This is an explicit operator command, not an automatic job scheduler or an agent tool.

The budget covers that project's `workspaces/` and `leases/` trees. Counts come from native `du -sk`, using allocated KiB converted to bytes and deduplicating hard links across those trees. Filesystem compression/cloning can affect reported allocation. This does not measure total server free space, Git mirrors, shared toolchain images, journals or billable storage byte-time. The remote configuration's `maxBytes` remains a separate per-checkpoint transfer limit.

The pruner visits entries by `last-used.json`; successful native lookups refresh this separate marker. Legacy entries without a marker use manifest creation time. Manifest and disk fingerprints stay unchanged. Before eviction, Haskell checks the owned directory's names, manifest identity, directory key and disk digest. The matching per-digest S3 manifest must have the same identity, hash and size, and a bounded streamed read must hash to that digest. A same-size corrupted remote object cannot authorize local deletion. Invalid, unarchived or unavailable entries remain local.

Live immutable lease references pin their canonical entry. Lookup, lease acquisition, lease cleanup, archive, restore and prune share a project lock, so eviction cannot race a new reference. The lock is released while the borrower uses its lease; pruning holds it during remote verification. That verification can delay cache operations for this project. Other projects have independent locks. Safe entries retire through a private temporary directory before removal; ordinary exceptions clean it up.

The JSON report includes `beforeBytes`, `afterBytes`, `leaseBytes`, `targetBytes`, typed entry outcomes and unrecognized names. If retained entries or lease allocation leave the cache above target, the command prints the report and exits unsuccessfully. It does not claim the budget was met or delete active/unverified state to force it. Unknown names, directory symlinks, unexpected entry contents, other projects, Git mirrors and toolchain images are preserved.

Private journals in `CACHE/PROJECT/prunes/` retain started, running and finished states, the partial/final report and elapsed time. Elapsed time includes the initial inventory; a journal starts before scanning entries. A stopped run can leave an incomplete journal. Resource costs remain unpriced, and these maintenance records are not yet customer usage or charges. SIGKILL can also leave `.prepare-*`, `.restore-*`, `.evict-*` or lease directories. They remain visible in allocated usage; safe orphan reconciliation, remote retention and automatic admission control remain unfinished. Host maintenance below automates verified archive and eviction.


### Automatic host cache maintenance

Set `IMPACT_REVIEW_WORKSPACE_MAINTENANCE` to a local JSON configuration file:

```json
{"cacheDir":"/srv/monoscope/workspaces","remote":"/etc/monoscope/workspace-store.json","maxBytes":10737418240}
```

Use the existing private S3 configuration for `remote`. The server starts a local maintenance fiber only when this setting exists. Each configured application instance, including consumer-only instances, checks its own configured cache on startup and every five minutes after a pass ends. A cache-root file lock serializes instances sharing that directory. This uses the existing Haskell workspace effect; no JavaScript service or managed scheduler is involved. Run the same pass manually with `monoscope workspace maintain --config /etc/monoscope/workspace-maintenance.json`.

The host budget measures allocated bytes across the entire cache root with native `du -sk`. When the root exceeds its budget, maintenance visits the largest project allocations first, then uses the existing per-project LRU order. Active immutable lease links remain pinned. A missing or invalid remote copy causes a valid idle checkpoint to be uploaded through the existing transfer journal. The pruner streams and verifies the stored SHA-256 before removing the local checkpoint. Existing valid remote copies avoid another upload. Restore keeps the existing disk-checkpoint contract; running processes and RAM are not restored.

Unknown entries, invalid checkpoints, unavailable archives and active leases remain local. Their bytes still count toward the host budget. Unknown project directories are reported and preserved. Maintenance returns the final allocation, per-project reports and failures; the CLI exits unsuccessfully and the server logs attention when the budget is unmet or a project pass fails. Passes under budget avoid creating per-project prune journals or transferring checkpoints.

This is a periodic reclamation budget, not an allocation quota: active workloads can grow between passes. SIGKILL lease/VM orphan reconciliation and remote retention remain separate work. Transfer and prune records retain explicit unpriced status; customer billing ingestion and storage byte-time pricing remain unfinished.


### Recover interrupted checkpoint staging

New restore and eviction operations write a private project-scoped record under `stages/` before creating scratch data. The record binds a UUID directory to its operation, identity, phase and timestamps. Recovery runs before budget eviction, including when the cache is under budget, and skips projects whose native lock is busy.

Only registered restore and eviction directories are eligible. Recovery preserves unknown names, symlinks, hard links, invalid metadata and mismatched project identities. Partial downloads can be discarded after the native writer releases the project lock. Completed staged checkpoints and interrupted evictions require a fresh streamed SHA-256 check of the archived bytes before deletion. A durable reclaiming phase supports retry after a crash during deletion. Records retain unpriced status.

Unregistered legacy scratch directories, preparation directories and leases remain protected. This does not recover orphaned VMs or establish allocation admission. Gondolin 0.13.0's installed `vm/core.js` stops the VM before materializing a checkpoint; the restore bridge only reads checkpoint metadata. Preparation and lease recovery still need their own lifetime proof.


## Optional Linux agent image

`Dockerfile` has an `impact-worker` target for an agent-enabled server. The default `production` target retains the ordinary server runtime. The worker adds the native CLI, Node 24, locked Pi/Gondolin modules, bundled skills, QEMU, Git, FFmpeg, Graphviz and a checksum-verified GitHub CLI 2.103.0 with attachment support. Research runners, build commands, host `node_modules`, guest images and local research artifacts are excluded from that runtime.

Generate production images on this Mac with OrbStack and Rosetta AMD64 emulation.
Use the local `orbstack` Docker context and `monoscope-local-amd64` builder.
Inspect their endpoints before building; the socket must be local to this Mac.
Do not use an SSH builder, production server or GitHub runner for image generation.
The [local CI reference](local-ci.md) defines builder setup and final signoff.

The optional worker target can be built locally after implementation and focused verification:

```sh
DOCKER_CONTEXT=orbstack docker buildx build --builder monoscope-local-amd64 \
  --platform linux/amd64 --target impact-worker --load \
  -t monoscope-impact-worker:local .
```

This command builds a local image; it does not authorize deployment.
Production deployment requires a merged, verified commit and separate authorization.

The CLI wrapper clears the server's `GHCRTS` before executing the separate binary. Image builds run the CLI as `monoscope` with the server profiling environment still set, import the actual adapter modules, check bundled skill paths, and check GitHub attachment support. These are packaging checks; upload authorization still depends on a suitable credential and the native publication gates.

On a Linux host, grant only the KVM device and its numeric group, mount an explicit immutable guest directory read-only, and retain the private data volume:

```sh
docker run --rm --device /dev/kvm --group-add "$(stat -c %g /dev/kvm)" \
  --env-file /secure/worker.env -e CONSUMER_ONLY=False \
  -e IMPACT_REVIEW_GUEST_IMAGE=/guest \
  --mount type=bind,src=/srv/monoscope/guest,dst=/guest,readonly \
  --mount type=volume,src=monoscope-agent-data,dst=/var/lib/monoscope \
  monoscope-impact-worker:local
```

The environment file supplies the existing application database, storage and model configuration. Credentials remain runtime inputs. The image's private cache and receipt defaults are `/var/lib/monoscope/cache` and `/var/lib/monoscope/usage`; Gondolin's cache shares that data volume. Bind mounts must permit the image's `monoscope` user to write these directories. Configure host maintenance separately with the existing maintenance JSON; unregistered SDK/image cache data remains protected and can leave the soft budget unmet.

Run regular server mode because `CONSUMER_ONLY=True` skips odd-jobs. Dedicated job routing remains a follow-up. The runtime receives no Docker socket. Guest creation is a trusted operator build step; repository agents use the offline VM and approved tools. A bare minimal guest supports shell investigation; repositories needing GHC, Node or the Monoscope CLI need a prepared dependency profile.

Current verification and limits are recorded in the [consolidated review](reviews/2026-11-01-agent-cross-module-review.md). Building this target does not deploy it.
