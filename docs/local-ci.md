# Running CI on your own machine

Run checks locally before pushing. Use `make ci-signoff` to run the CI checks and publish passing results for GitHub to reuse.
The final status output shows which checks still need a remote run.

GitHub runs checks without matching attestations on standard GitHub-hosted runners.
For work from this Mac, generate production images locally with OrbStack before a push.
Do not use production servers or GitHub runners for image generation.

A signoff records passing checks for specific file contents. It does not bypass failed checks or approve different contents.
Repository push access identifies who can publish these records. A Git `Signed-off-by` trailer does not replace CI results.

## The short version

Before the first run, [create the local builder](#local-amd64-images-on-this-mac).

```bash
export DOCKER_CONTEXT=orbstack
export MONOSCOPE_BUILDER=monoscope-local-amd64
make ci-signoff     # local checks, published results, then remaining remote checks
make ci             # run everything CI runs, in CI's own containers
make ci-status      # what CI would run right now, without running any of it
make ci CHECKS="doctests unit-tests"   # just these
make ci-down        # stop the containers (build caches kept)
```

### Host runtime for direct frontend checks

CI and `Dockerfile.deps` use Node 22. Use Node 22 for host-level commands in
`web-components/`, such as `bun run test` and `bun run typecheck`. The current
Vitest/Vite dependency set requires Node 20.19 or newer; older Node 20 releases
can fail before tests start because `node:util.styleText` is absent. `make ci`
remains the preferred parity path because it runs checks in the pinned
dependency image.

The repository includes `.nvmrc`; with nvm installed, run `nvm use` from the
repository root before running a host-level frontend command. This selects the
same Node 22 major version as CI.

`make ci` publishes an attestation for every check that passes. Push, and the
gate job finds them.

## Before pushing

1. Run `make ci-signoff`.
2. Read the final status output for checks that still need GitHub.
3. Commit the checked changes and push them.
4. In the PR description, record the local commands, results, and any checks left for GitHub.

If you edit check inputs after signoff, run signoff again. GitHub only reuses results with matching content fingerprints.
If local checks fail, fix the failure before pushing.
If services or tools are unavailable, record the missing checks and let GitHub run them.

`CHECKS="..."` limits the local run. The final output still covers every check.
`CI_NO_ATTEST=true` prevents publication. Without published results, GitHub must repeat the checks.
If publication fails, the final output shows that GitHub still needs those results.

## How it works

`ci/checks.tsv` is the one definition of what CI is; both the workflows and
`make ci` read it. Each check declares:

- **inputs** — the paths whose content the result depends on. Every check also
  implicitly depends on `ci/checks.tsv`, `scripts/ci/`, and `.github/workflows/`,
  so changing what a check *does* invalidates it everywhere.
- **requires** — the capabilities the environment must have for the result to
  mean anything (`ghc`, `node`, `pg`, `minio`, `tf-real`, …).

A check's **fingerprint** is a SHA-256 over the git blob hashes of its inputs. It
is computed from the *working* tree, not from `HEAD`, so you can run `make ci`
before committing and the attestation still matches the commit you make from it.

A passing check is published as an empty commit at

```
refs/ci-attest/v1/<check>/<fingerprint>/<platform>/<capabilities>/<date>
```

Everything the gate needs is in the ref name, so deciding costs one `ls-remote`
and no object fetches. The gate reuses an attestation only when the fingerprints
match **and** the recorded capabilities are a superset of what the check requires
— an environment that fell back to a stub service can never satisfy a check that
needs the real one.

Attestations are ordinary pushed refs, so producing one requires push access to
the repo. Fork PRs can't create them, and can't be affected by them.

## Fidelity, and where your laptop falls short

`make ci` runs in `ghcr.io/monoscope-tech/monoscope-deps` — the same image the
workflow uses, and it is multi-arch, so it runs natively on Apple Silicon. The
service containers, their env, and the tuning in `ci/compose.yml` are kept
identical to the workflows for the same reason.

Two deliberate differences from your normal `make test`:

- **Your `.env` is not visible.** `ci/compose.yml` mounts an empty file over it.
  A local secret — or a `DATABASE_URL` pointing at production — changing the
  result is exactly what makes "passes locally, fails in CI" happen.
- **Editing `ci.sh` mid-run is safe.** The container runs from a copy, because
  bash reads a script incrementally by file offset — editing the original while
  it runs would otherwise make the running shell resume at a stale offset and die
  with `syntax error near unexpected token`. A run lasts tens of minutes, so
  editing it meanwhile is normal, not a mistake.
- **Build directories are container-private.** `dist-newstyle`, all three
  `node_modules` directories, npm's download cache, and Playwright's browser download are named
  volumes. Your host's macOS/arm64 artifacts would corrupt the Linux build.
  The cabal store, `dist-newstyle`, and the npm and Playwright caches are shared
  by every checkout (only `node_modules` stays per worktree) and persist, so only
  the first `make ci` on a machine is a cold build; a new worktree or sign-off
  clone builds incrementally from whatever branch built last. `make ci-down` keeps
  them; `make ci-clean` deletes them for every checkout.
  Each `integration-tests` invocation recreates only the ephemeral Postgres,
  MinIO, and TimeFusion service containers, so deterministic fixture IDs and
  telemetry rows never leak into the next run. The build-cache volumes remain.
  Checks that do not need services start only the runner; `e2e` starts Postgres,
  and `integration-tests` starts all three services.
  When every selected check is already attested, `make ci` skips Docker entirely.
  Local CI runs serialize automatically to avoid overloading the host.
  Worktree runs mount their shared Git metadata so attestation fingerprints work in containers.
  If HLint is installed on the host, `make ci` runs that check there, as the GitHub workflow does.
  The host passes its attestation-ref list to the runner, which has no SSH keys.

**TimeFusion publishes no arm64 image**, and the amd64 one segfaults under
emulation on Apple Silicon. Build one for this machine, once:

```bash
make tf-image            # docker build ../timefusion for this architecture
```

`make ci` and `make ship` then find it on their own — no exported variables — and
`integration-tests` runs and attests here like every other check. Without it, on
a Mac:

- everything except `integration-tests` runs and attests normally;
- `integration-tests` is refused, and stays CI's job.

`CI_ALLOW_DEGRADED=true make ci` runs it anyway against the
Postgres-as-TimeFusion fallback. That is genuinely useful feedback and it is
**not** attested — the dual-write TF leg is exactly what that check exists to
exercise.

For one focused integration run against the local TimeFusion source checkout,
run:

```bash
make test-integration-tf
```

This command starts local MinIO and TimeFusion. It stops TimeFusion after the
test run. Use it to investigate the real PGWire write and read path without a
full containerized CI run.

TF's image is distroless, so it has no shell for a compose healthcheck; `ci.sh`
waits for its pgwire port from the runner container instead. A TF container that
shows no health status is therefore normal, not broken.

Dependency images rebuild when their declared inputs change or through a manual workflow run.
There is no weekly rebuild. Use the manual run for base image or system package updates.

### Native Linux and KVM checks

The normal CI container has no KVM device and deliberately hides host guest images.
A passing container check does not establish native VM acceptance.
On this Mac, the OrbStack host also has no `/dev/kvm` device.
Use a separate local Linux VM for those checks; keep image builds on OrbStack.

The tested setup uses Lima 2.2.1 with `vmType: vz`, `arch: aarch64`,
and `nestedVirtualization: true`, as described by the
[Lima configuration](https://github.com/lima-vm/lima/blob/v2.2.1/templates/default.yaml).
The outer VM has four CPUs, eight GiB of RAM, and a 64 GiB sparse disk.
Project and home folders are not mounted. Rosetta shares only its runtime.
The SDK's inner guest uses two CPUs and 1024 MiB.
Check the KVM API, actual QEMU accelerator, and open device descriptor;
an existing device path alone does not prove that a guest uses KVM.
The [workspace review](reviews/2026-11-01-agent-cross-module-review.md) records the current acceptance scope.
Keep test binaries, assets, and receipts in the guest user's home; a VM reboot clears `/tmp`.

Application fix-job checks also require FFmpeg, Graphviz, and an absolute `HOST_URL`,
as supplied by `scripts/ci/ci.sh`. Without them, media preparation or publication packages fail.
For an isolated VM, install `ffmpeg` and `graphviz` and set `HOST_URL=http://localhost:8080/`.
Use `USE_EXTERNAL_DB=true` with the local test database and an empty `.env`;
do not copy production credentials into the VM.

If a required watcher restart leaves an orphaned GHC process, inspect its parent,
working directory, listeners, and children before cleanup. Stop only a confirmed
detached process from that checkout. Preserve active watchers and other worktrees.

## Production images and deployment

Same-repository PRs run `Production image ready` and publish a production image.
Configure branch protection to require `Production image ready / Production image`
and `Release regression tests` before merging. Fork and Dependabot PRs run checks but cannot publish production
images; master builds their image after merge.

Images are indexed by the complete committed source tree, `linux/amd64`, and the
registry digests of both the dependency image and Debian runtime image. These
base digests are passed into the Docker build, so the fingerprint describes the
actual toolchain used. The context comes from `git archive`; ignored local build
settings and generated files cannot alter a published artifact. Full-tree hashing
is deliberately conservative: even a file outside the Docker context can
trigger a new image.

Master reuses the existing immutable digest when these inputs match, including
across squash merges. Changed sources or base-image digests require a new build,
including a dependency-image update between the PR build and merge. Commit-SHA tags are
aliases; the image retains OCI labels for the original build revision, source
tree, input fingerprint, and builder. Its embedded `GIT_HASH` and
`GIT_COMMIT_DATE` describe the original build, which can be the PR revision.

Image builds never update `latest`. After the test gate passes, the deployment
job serializes with other GitHub deployments and checks the live master ref
before updating `latest` and again before submitting the digest to CapRover.
Every master push starts this workflow, including docs-only pushes, so a newer
ignored commit cannot strand an in-flight application deployment.
A failed remote lookup blocks deployment. Local deployments make the same
master check; deliberate rollbacks use a separate command. Local deployments are
outside GitHub's concurrency group. The ref check and tag/deployment updates are
separate operations; a merge between them can make an update stale.

```bash
DOCKER_CONTEXT=orbstack MONOSCOPE_BUILDER=monoscope-local-amd64 make deploy-image
DOCKER_CONTEXT=orbstack MONOSCOPE_BUILDER=monoscope-local-amd64 make ship
make deploy-app SHA=<sha>  # deploy current master only
make rollback-app SHA=<sha> # explicitly deploy an older image already on master
make deploy-status
```

`make ship` never pushes to master. Merge the pull request first, then use a
clean checkout of that merged revision. Every deploy-path check must be proven
for this tree. GitHub still verifies the gate and skips checks with matching
attestations; its image step resolves and aliases the locally built registry artifact.

`make ci-signoff CHECKS="release-tests"` runs the release regression suite on the
host using Node’s built-in test runner, with no npm dependencies, and records its
result. Docker is required for the small cold-cache regression build. The Cabal
cache mounts are seeded from the dependency image, so a fresh builder retains its
prebuilt packages instead of hiding them beneath an empty cache.
`make ci-selftest` also runs it. The suite checks
squash reuse, source and toolchain invalidation, pinned build inputs, rejection
of dirty or mismatched checkouts, and stale local/remote deployment prevention.

On this Mac, the real cold-cache fixture validates and selects the local builder.
Each run uses a new cache ID and asserts that only the prebuilt dependency exists
before adding a file. Later build steps must retain both files. It avoids global
`--no-cache`, which can discard cache mounts, as discussed in
[BuildKit's cache-mount issue](https://github.com/moby/buildkit/issues/5305).

### Local AMD64 images on this Mac

Production uses `linux/amd64`. This Mac uses Apple Silicon.
[OrbStack uses Rosetta](https://docs.orbstack.dev/docker/#intel-x86-emulation)
to run AMD64 containers locally. The image build must execute on this Mac.
Do not use a production host, an SSH builder, or a GitHub runner for this build.

Create the local builder once:

```bash
docker --context orbstack buildx create --name monoscope-local-amd64 --driver docker-container orbstack
docker --context orbstack buildx inspect --bootstrap monoscope-local-amd64
```

Inspect the Docker context and builder endpoint before a build:

```bash
docker context inspect orbstack
docker --context orbstack buildx inspect monoscope-local-amd64
docker --context orbstack run --rm --platform linux/amd64 debian:12-slim uname -m
```

The context must use a local Unix socket. The builder endpoint must be
`orbstack`. The container command must return `x86_64`.

Select this context and builder explicitly:

```bash
DOCKER_CONTEXT=orbstack MONOSCOPE_BUILDER=monoscope-local-amd64 make ci-signoff
DOCKER_CONTEXT=orbstack MONOSCOPE_BUILDER=monoscope-local-amd64 make deploy-image
```

`release.sh` checks the Mac host, explicit context, Unix socket, builder name,
and sole `orbstack` node before image generation. Missing builders fail instead
of falling back. CI can reuse and alias a completed registry image; a cache miss
fails with instructions to build locally. CI does not compile a production image.

`make builder-setup` now creates only the local OrbStack builder. It rejects
`BUILD_HOST` before any SSH or Docker operation. The default builder name is
`monoscope-local-amd64`; select the context and builder explicitly as above.
Registry pulls and pushes transfer artifacts. They do not move compilation to another host.

`ci-signoff` runs checks and an image build. Report their progress separately.
The image fingerprint includes immutable dependency and runtime image digests.
A changed base digest can require another image, even with unchanged application code.
Use `make ci-status` for a status check instead of another signoff run.

The production Dockerfile locks its application compiler cache during each build
step. A separate cache namespace excludes artifacts from older unlocked builds.
Concurrent builds using this Dockerfile wait before writing compiler outputs.
The first build in the new namespace recompiles application code; later builds
reuse it. Dependency caches remain available.

### Credentials

Deploying needs three values, read from the environment or `.env` (gitignored):

| Variable | What |
|---|---|
| `CAPROVER_URL` | e.g. `https://captain.example.com` |
| `CAPROVER_APP` | the app name to deploy |
| `CAPROVER_APP_TOKEN` | that app's deploy token (CapRover → app → Deployment) |
| `CAPROVER_PASSWORD` | optional, admin password — only `make deploy-status` needs it |

Use the per-app deploy token, not the admin password: it deploys that one app and
nothing else, and it can be rotated on its own. The password is optional on
purpose — the credential every developer keeps is the one that cannot enumerate
the server.

## Building the deploy image yourself

`make deploy-image` uses the same `scripts/ci/release.sh` recipe as the PR and
master image jobs. It requires a clean tree and registry access. It publishes
an immutable digest, a content-input tag, and a commit-SHA alias without moving
`latest`. Master reuses that artifact when its source tree and pinned bases
match. Image labels preserve who built it and from which revision.

## Knobs

| Variable | Effect |
|---|---|
| `CHECKS="a b"` | restrict `make ci` / `make ci-status` to these checks |
| `CI_FORCE=true` | re-run even when an attestation already exists |
| `CI_KEEP_GOING=true` | don't stop the sweep at the first failure |
| `CI_ALLOW_DEGRADED=true` | run checks whose capabilities are missing, unattested |
| `CI_NO_ATTEST=true` | run, publish nothing |
| `CI_SHARDS=n` | integration-test shard count (local defaults to 12; GitHub uses 4) |
| `CI_ATTEST_DISABLED=true` | ignore all attestations — set as a repo variable to force full CI runs |
| `MONOSCOPE_CI_TF_IMAGE` / `MONOSCOPE_CI_TF_PLATFORM` | point at a locally built TimeFusion image (auto-detected after `make tf-image`) |
| `TF_TARGET_CPU` | CPU baseline used by `make tf-image`; defaults to `neoverse-n1` on arm64 and `x86-64-v3` on amd64. Override only for a known compatible target. |
| `BUILD_HOST` | rejected by `builder-setup`; SSH image builders are prohibited |
| `DOCKER_CONTEXT` / `MONOSCOPE_BUILDER` | for this Mac, set `orbstack` and `monoscope-local-amd64` explicitly. Inspect their local endpoint before a build. |
| `SHIP_ANY_BRANCH=1` | let `make ship` deploy something other than master |

`CI_UNIT_BIN`, `CI_CLI_BIN`, `CI_INTEGRATION_BIN`, `E2E_SERVER_BIN`, and
`CI_LOCAL_ASYNC_SERVICES` are internal handoffs between local checks; leave them unset.

## When you need to invalidate everything

The fingerprint covers repository content, not the toolchain. If the deps image
is rebuilt with materially different system packages, existing attestations stay
valid even though the environment moved. Two escape hatches:

- bump `EPOCH` in `scripts/ci/ci.sh` — invalidates every attestation at once;
- set the repo variable `CI_ATTEST_DISABLED=true` — the gate ignores all
  attestations until you unset it, with no code change.

Old refs are deleted after 30 days by `.github/workflows/ci-attest-gc.yml`.

## Reading the gate

Every run writes a table to the job summary: each check, whether it ran or was
skipped, and the exact attestation ref that let it skip. The deploy job records
in its own summary whether it was gated on a test run or on pre-existing
attestations.

## Changing a check

Edit `ci/checks.tsv` and the matching case in `run_body` (`scripts/ci/ci.sh`).
`make ci-selftest` checks the two stay in sync, along with the fingerprint and
capability logic. Narrow a check's `inputs` only where it is provably sound: a
too-wide set costs a rerun, a too-narrow one ships an untested change.


The integration suite exercises signed PNG requests through the real renderer.
It requires Node and Bun as well as the database services. Local CI installs the
frontend dependencies and builds `chart-cli` before running integration shards.
The native `make live-test-dev` watcher also builds the renderer first; install
`web-components` dependencies before starting it.
