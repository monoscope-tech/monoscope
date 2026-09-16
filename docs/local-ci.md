# Running CI on your own machine

Run checks locally before pushing. Use `make ci-signoff` to run the CI checks and publish passing results for GitHub to reuse.
The final status output shows which checks still need a remote run.

GitHub runs checks without matching attestations on standard GitHub-hosted runners.
All workflows use GitHub runners, including releases and image builds.

A signoff records passing checks for specific file contents. It does not bypass failed checks or approve different contents.
Repository push access identifies who can publish these records. A Git `Signed-off-by` trailer does not replace CI results.

## The short version

```bash
make ci-signoff     # local checks, published results, then remaining remote checks
make ci             # run everything CI runs, in CI's own containers
make ci-status      # what CI would run right now, without running any of it
make ci CHECKS="doctests unit-tests"   # just these
make ci-down        # stop the containers (build caches kept)
```

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
- **Build directories are container-private.** `dist-newstyle` and both
  `node_modules` are named volumes; your host's are macOS/arm64 artifacts and
  sharing them corrupts both. The volumes persist, so the second `make ci` is
  fast — but the **first one is a cold build** and takes as long as a cold CI
  run. Start it and go do something else; every run after that is incremental.
  `make ci-down` keeps them; `make ci-clean` deletes them and buys you the cold
  build back.

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

TF's image is distroless, so it has no shell for a compose healthcheck; `ci.sh`
waits for its pgwire port from the runner container instead. A TF container that
shows no health status is therefore normal, not broken.

Dependency images rebuild when their declared inputs change or through a manual workflow run.
There is no weekly rebuild. Use the manual run for base image or system package updates.

## Shipping without waiting for CI

`make ship` is the whole deploy from this machine: checks, image, push, deploy.

```bash
make ship
```

It runs, in this order, and stops at the first thing that fails:

1. **Checks**, exactly as `make ci` runs them, attesting each pass.
2. **Image** — build and push `ghcr.io/…/monoscope:<sha>` (and move `:latest`).
3. **Push** the commit to `origin/master`.
4. **Deploy** — tell CapRover to run that image, and record that it was deployed.

Why this order: checks first because everything after is expensive; image before
push so CI's probe finds it and skips its own build; push before deploy because
production must run a commit that exists on origin — `ship` refuses to deploy a
commit `origin/master` does not contain, so a rollback always has something to
roll back to.

CI still runs on the push, but there is nothing left for it to do: every check is
attested, the image is in the registry, and the deploy job asks
`ci.sh deployed <sha>` first and skips when the answer is yes. It becomes a
second opinion instead of the critical path.

**`ship` will not deploy something it cannot vouch for.** If a deploy-path check
(`build`, `doctests`, `unit-tests`, `cli-tests`, `integration-tests`, `e2e`) has
no attestation for this exact tree, it stops. A `weeder` or `hlint` failure does
not stop it — those gate pull requests, not the deploy — but it says so.

Related commands:

```bash
make deploy-status         # what CapRover is running right now
make deploy-app SHA=<sha>  # deploy an already-built image, e.g. a rollback
```

### A native amd64 builder (do this once)

Prod is `linux/amd64`. On Apple Silicon that build is emulated, and an emulated
GHC build is slow enough that people go back to letting CI do it — which is the
habit this file exists to remove. Point buildx at any amd64 machine you can SSH
to and the build is native:

```bash
BUILD_HOST=ubuntu@your-amd64-box make builder-setup
make builder-status
```

The buildkit container is capped (`BUILD_CPUS`, default 12; `BUILD_MEMORY`,
default 48g) because that host is often also serving something. Nothing requires
a builder: with none configured the build falls back to the emulated local one,
and a stale `MONOSCOPE_BUILDER` name falls back too rather than failing a deploy.

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

The image build is the rest of the deploy: with tests cached it is ~4 of the ~4.5
minutes. It is not fingerprinted like the other checks, because it produces an
*artifact* rather than a verdict — skipping it would leave nothing to deploy. Its
cache is the registry, and its fingerprint is the commit SHA:

```bash
make deploy-image          # build + push ghcr.io/…/monoscope:<HEAD sha>, linux/amd64
```

Push that commit and CI's `build-image` job finds the tag already there and skips
the build. The same check makes a re-run, a `workflow_dispatch` of an existing
commit, or a revert to an already-built SHA into a ~30-second deploy for free.

Two things to know before using it:

- **It refuses a dirty tree.** The image is tagged with a commit SHA and must
  actually be that commit, or the tag lies about what is running in production.
- **It records who built it.** An image cannot be re-derived from source to check
  (a Haskell build is not bit-reproducible), so unlike an attestation there is no
  way to verify after the fact that a pushed image matches its tag. The deploy job
  therefore prints `built by: …` in its summary, every deploy, sourced from a
  `refs/ci-attest/v1/image/<sha>/…` record written at build time. If that line
  ever names someone unexpected, that is the signal.

Prod is `linux/amd64`. On Apple Silicon that is emulated — but by **Rosetta**, not
QEMU, so it runs at a useful fraction of native rather than 10× slower. Budget
~10 GB of free disk for the amd64 deps image the first time.

## Knobs

| Variable | Effect |
|---|---|
| `CHECKS="a b"` | restrict `make ci` / `make ci-status` to these checks |
| `CI_FORCE=true` | re-run even when an attestation already exists |
| `CI_KEEP_GOING=true` | don't stop the sweep at the first failure |
| `CI_ALLOW_DEGRADED=true` | run checks whose capabilities are missing, unattested |
| `CI_NO_ATTEST=true` | run, publish nothing |
| `CI_SHARDS=n` | integration-test shard count (CI uses 4; more needs more `max_connections`) |
| `CI_ATTEST_DISABLED=true` | ignore all attestations — set as a repo variable to force full CI runs |
| `MONOSCOPE_CI_TF_IMAGE` / `MONOSCOPE_CI_TF_PLATFORM` | point at a locally built TimeFusion image (auto-detected after `make tf-image`) |
| `BUILD_HOST` / `BUILD_CPUS` / `BUILD_MEMORY` | the native amd64 build host and its caps (`make builder-setup`) |
| `MONOSCOPE_BUILDER` | buildx builder to build the image with; falls back to the default builder if absent |
| `SHIP_ANY_BRANCH=1` | let `make ship` deploy something other than master |

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
