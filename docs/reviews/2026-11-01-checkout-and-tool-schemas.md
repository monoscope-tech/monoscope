# Checkout recovery and agent tool schemas

Date: 1 November 2026. This review covers the checkout interpreter, MCP schema generation, and their two integration regressions.
Whole-branch review and release signoff remain open.

## Interrupted checkout cache

An abrupt stop of the local Linux VM preserved the native agent checkpoint but truncated the disposable Git mirror HEAD file.
The trusted source repository remained readable. The first application continuation failed with `Repository checkout could not be prepared`.
The source cache therefore prevented recovery before the saved agent session could resume.

The checkout interpreter now rebuilds the disposable mirror after a Git process failure and retries once.
The existing exclusive lock covers the rebuild. Each retry checks current source access and fetches the same pinned revisions.
Private checkouts remain independent clones, and their existing lifetime covers child-process cleanup.
The retry catches only `ExitCodeException`, preserving cancellation and the outer checkout timeout.

The existing checkout lifecycle now corrupts HEAD and config in turn.
It requires a successful pinned checkout, a cache miss, private-directory cleanup, and a subsequent cache hit for each corruption.
Its concurrent checkout, source-revocation, retained-history, and child-lifetime assertions remain in place.

## Self-contained tool schemas

MCP tool inputs previously referenced OpenAPI components without including their definitions.
Lazy agent tool discovery therefore exposed body references without the fields needed to construct monitor and search requests.

Each generated input now includes its reachable definitions under `$defs` and uses local references.
The existing containers graph traversal handles recursive schemas without recursive expansion.
Tool names, dispatch, authority, and operation policy remain unchanged.

The JSON-RPC integration regression checks monitor and search body fields through the actual `tools/list` response.
It also requires every reference in every advertised input schema to resolve locally.

## Reviews and focused checks

The first focused run reproduced both bugs: 76 examples, two failures, 32.2089 seconds.
The final focused run passes 76 examples with zero failures in 38.0861 seconds.
The subsequent broader run passes 585 examples with zero failures in 192.1411 seconds.
It covers bot workflows, OTLP ingestion, and both new regressions.
All 279 recorded Haskell and manifest inputs remain unchanged during that run.
Its source receipt is `scripts/local/checkpoint-fix-broad-watch-source.json`.
The local Linux supported doctest target passes all 2,098 examples with zero errors or failures.
Pinned formatting, HLint, and whitespace checks pass.

Repeated hs-distill, hs-evasion-review, and hs-lob-review passes narrow the retry to Git failures and localize the single-caller registry builder.
The final pass finds no further material issue within these four changed files and their inspected callers.
Process IO remains inside the existing checkout interpreter. No browser behavior or package dependency is added.
The source hashes and exact log paths are recorded in `scripts/local/checkout-and-mcp-schema-final-source.json`.

## Recovery of the original interrupted run

The local Lima VZ driver was killed during resumed work, removing the Linux application host, SDK, and inner KVM guests.
The external local test database survived. The original lease expired naturally before takeover.
The initial continuation exposed the truncated cache described above.

The corrected binary then resumes the same fixture, database, and native checkpoint without resetting them.
That continuation passes one application case in 19.2457 seconds.
The independent database observer checks the original shared run, both original attempts, and the original regression receipt.
The completed run contains three attempts, three checks, one wiki revision, and one operation receipt.
The wiki revision still names the shared run as its author. Completed replay adds no inference call.
All four observed QEMU guests use real KVM, two CPUs, and 1024 MiB.

The recovery binary differs from the binary present at the initial crash because it contains the cache fix.
This result proves recovery of the retained checkpoint after that failure, rather than an unchanged-binary crash cycle.
The model provider, GitHub token exchange, and media retention use controlled test interpreters.
An earlier harness attempt lost its newly created source fixture and did not establish application recovery.
The second attempt seals trusted fixture inputs before execution without flushing native checkpoints.
Neither attempt proves physical Mac power-loss or storage-hardware recovery.

The retained receipts are `scripts/local/linux-hostloss-v2-acceptance.json`, `linux-hostloss-v2-fixed-kvm-observations.json`, and `linux-hostloss-recovery-fix-source.json`.
The failing and successful application logs remain separate.
All compilation and execution occur on this Mac. No production host or GitHub runner builds an image.

## Fresh crash with the corrected binary

A separate experiment repeats the complete crash and recovery cycle with the corrected binary present before shutdown.
The original experiment and database remain intact.
Before the crash, the fresh run retains two attempts, one regression check, one wiki revision, and one completed operation.
The application then resumes active work before the local VZ driver is killed.

After restart, a different boot ID and the same executable hash establish a fresh host with an unchanged application binary.
All durable checkpoint files retain their hashes.
One unreferenced staging transcript changes beside an unchanged Finished manifest.
The trusted Git origin remains readable, while the mirror HEAD is again empty and Git rejects that cache.
The harness does not repair the cache or flush native checkpoints.

The original lease expires at 17:31:25.659533 UTC.
The independent observer records takeover at 17:31:26.759830 UTC, with no earlier replacement attempt.
The application repairs the cache and completes the original shared run.
Both original attempts and the original regression remain; the final totals are three attempts and three checks.
The single wiki revision and operation remain unchanged, and completed replay adds no inference.
All four observed recovery guests use KVM, two CPUs, and 1024 MiB.
The application case passes in 565.6467 seconds, including the natural lease wait.

All 22 recorded native runtime inputs and 28 worker files match their source snapshots.
The guest worker files also match those 28 hashes.
The same executable hash is recorded before shutdown, after restart, and after completion.
The local supported build passes all 2,098 doctest examples.
No application, Node worker, or QEMU process remains after completion; the test VM is then stopped.

The receipts are `scripts/local/linux-hostloss-v3-acceptance.json` and `linux-hostloss-v3-harness.json`.
Separate cache probes record the empty HEAD after restart and the repaired 23-byte HEAD after application recovery.
This proves abrupt Linux virtual-host recovery with the unchanged binary and a surviving external local test database.
The controlled-provider and physical-Mac limits above still apply.
