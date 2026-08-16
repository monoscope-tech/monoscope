# Running CI on your own machine

Our GitHub runners are 4 vCPU. A full `Deploy` run is ~20 minutes and a cold one
is closer to 40. Most laptops here are considerably faster than that, and most of
what CI does on any given push it has already done — the PR that produced the
merge ran the same tests over the same bytes.

So CI now asks, before every check: **has anyone already proven this?** If yes it
skips. It does not matter who proved it — a previous run, a re-run, or you.

Nothing here is mandatory. Push and wait for CI as before, or run `make ci` and
watch CI have almost nothing left to do.

## The short version

```bash
make ci             # run everything CI runs, in CI's own containers
make ci-status      # what CI would run right now, without running any of it
make ci CHECKS="doctests unit-tests"   # just these
make ci-down        # stop the containers (build caches kept)
```

`make ci` publishes an attestation for every check that passes. Push, and the
gate job finds them.

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

**TimeFusion has no arm64 image**, and the amd64 one segfaults under emulation on
Apple Silicon. So on a Mac:

- everything except `integration-tests` runs and attests normally;
- `integration-tests` is refused, and stays CI's job.

`CI_ALLOW_DEGRADED=true make ci` runs it anyway against the
Postgres-as-TimeFusion fallback. That is genuinely useful feedback and it is
**not** attested — the dual-write TF leg is exactly what that check exists to
exercise. On a Linux/amd64 box the real service starts and the whole suite
attests.

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
| `MONOSCOPE_CI_TF_IMAGE` / `MONOSCOPE_CI_TF_PLATFORM` | point at a locally built TimeFusion image |

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
