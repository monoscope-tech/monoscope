# Impact reviews: follow-ups after #812

PR #812 shipped triggers, merge-risk grading, widget evidence and terminal proof. Its description and `docs/agent-operations.md` describe that work. What remains:

- Browser proof: Playwright `recordVideo` in a second guest image.

## D. Research and investigation reach (from review feedback)
- The VM stays offline: it runs untrusted PR code, and open egress would let a PR exfiltrate source or abuse our network.
- Web docs: a host-side `fetch_url` tool (with an SSRF guard, size cap, text extraction). The host fetches; the guest never touches the network.
- Dependencies: registry-only egress (npm, PyPI, Hackage, crates, Go proxy, Maven) through a host proxy, if Gondolin supports a proxied NIC. Otherwise, use the prepare-script dependency archive.
- Monoscope investigation: mount monoscope-skills (`investigate`, `kql-reference`, `triage`) via `IMPACT_REVIEW_SKILLS`, and widen the host tools with the CLI's read verbs (issues, monitors, endpoint stats, trace lookup). These stay scoped to the mapped services and the review window. No API key enters the VM.

## E. Warm sandboxes per tenant and repository (follow-up PR)

The goal: a review or proof should not rebuild a repository's dependencies, wherever the job runs.

**What exists today.** A prepared workspace is a qcow2 disk checkpoint, keyed by project, repository, base revision, guest image, preparation-script digest and dependency-archive digest. `monoscope workspace archive` and `restore` move it to and from S3 with size and SHA-256 checks, under `agent-workspaces/v1/PROJECT/IDENTITY/`. Host maintenance uploads idle checkpoints, verifies them, and evicts them under a disk budget.

**The gaps:**
1. Jobs never prepare. Preparation is an operator command, so only hand-prepared repositories are warm.
2. A lookup miss never restores from S3, so a job on a fresh host starts cold even when the checkpoint is archived.
3. The key includes the exact base commit. Every merge to the main branch is a new key, so busy repositories miss on nearly every PR.

**Design:**
- **Restore on miss.** Add an optional `remote` to `Guest.Workspace` / `Options`. In `leaseWorkspace`, when the local lookup misses, run the existing verified `Restore`, then look up again. Restore takes the project lock itself, so call it outside the lease's lock. A failed restore falls back to a cold start and is logged.
- **Prepare in the job.** When nothing is restorable, the job runs the trusted preparation script itself: inside the VM, as today's CLI does, with no credentials. It then publishes the checkpoint locally and uploads it in the background. Only one job per project and identity prepares at a time (the project lock); others wait briefly or start cold.
- **Warm from the nearest ancestor.** Add a second, coarser pointer: `PROJECT/REPO/IMAGE/SCRIPT/latest` names the most recent checkpoint for that repository, whatever its base. On an exact miss, restore the latest one, check out the new base inside the VM, and rerun the preparation script. The script must be incremental (cabal, npm, cargo and go caches all reuse prior work). Publish the result under the exact key. The warm start never changes the exact key's contract: what was verified is what is stored.
- **Tenant isolation.** Keys and S3 prefixes stay per project, and nothing is shared across tenants. A shared, read-only toolchain layer (compilers, package indexes) belongs in the guest image, not in tenant checkpoints. That keeps the cross-tenant surface at zero.
- **Cost controls.** Per-project byte budgets already exist locally. Add an S3 lifecycle rule (expire checkpoints unused for N days), and record restore and prepare durations as metrics (`agent.workspace.restore`, `agent.workspace.prepare`, `cache_hit` / `ancestor_hit` / `cold`). The win then shows up in our own telemetry.

**Order:** restore on miss (small, in `leaseWorkspace`), then in-job preparation and upload, then ancestor warm starts. Each step stands alone and is measurable.

## F. Fair, isolated compute for sandboxes

- **Admission control:** a per-host VM semaphore (cores ÷ 2 for 2-vCPU guests) and a per-tenant concurrency cap. Excess work waits in the job queue instead of oversubscribing the host.
- **Placement:** `impact-worker` runs on dedicated hosts, never beside ingestion or the web server. On shared boxes, put each QEMU process under a cgroup CPU quota.
- **Budget:** count every proof and regression attempt, not only successes, so failing commands cannot keep the VMs busy.
- **Signals:** queue wait, VM-seconds per tenant, and cache hit / ancestor hit / cold-start counts as metrics.
