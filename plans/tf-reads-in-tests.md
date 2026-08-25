# Why the test harness reads Postgres, not TimeFusion

Decided 2026-08-25, after `bac2a9a5d` pinned `enableTimefusionReads = tfEnabled` in
`Pkg.TestUtils` and CI went from green to 23 failing examples across seven specs.

## What the pin did

`tfEnabled` is `isJust TIMEFUSION_PG_TEST_URL`. Locally that is unset, so reads stayed on
Postgres and every suite passed. **CI sets it** — `.github/workflows/haskell.yml` runs a real
`timefusion` container — so in CI every telemetry read moved from *per-example, synchronous
Postgres* to *shared, asynchronous DataFusion*. The pin's own goal was unrelated and still
holds: it stops a developer `.env` carrying `ENABLE_TIMEFUSION_READS=True` from emitting
TimeFusion-only SQL (`variant_get`) against the test Postgres. `False` satisfies that goal too.

## The two failure shapes, and why only one is fixable

**Inflated counts** — `expected 2, got 6`; `expected 0, got 2`; `expected 2, got 16`.
Postgres is cloned per example from a template; a real TimeFusion is **one instance for the
whole run**, keyed only by `project_id`, with no reset between examples. Every spec bills
`testPid = UUID.nil`, so an absolute `count(*) WHERE project_id = <nil>` measures whatever ran
first. This *is* fixable — mint a project per example, which
`Pkg.TestUtils.createTestProject` now does for `ReportUsageSpec` (under `nextRandom`, not the
deterministic test UUID stream, which restarts identically every example and would collide).

**Deflated counts** — `expected 100, got 1`; `expected 202, got 7`; `expected 500, got 65`.
This is TimeFusion's write→read visibility: a test that writes rows and reads them back in the
same breath does not see them. **No project isolation and no SQL fix can make this synchronous.**
It is a property of the store, not of the test.

The second shape is why reads are pinned off rather than the harness being taught to cope. A
test asserting "I wrote 500 rows, I can read 500 rows" is asserting read-your-writes, and
TimeFusion does not offer it.

## What is still exercised

`enableTimefusionWrites` remains `tfEnabled`, so the dual-write path
(`bulkInsertOtelLogsAndSpansTF`) still runs against a real TimeFusion in CI and still catches
serialisation and schema faults. Only the *read* leg is pinned.

## The thing this cost us, worth keeping

The pin was wrong for CI but it was not useless: routing reads through TimeFusion made CI
briefly prod-like and immediately surfaced a **real production bug** — `fetchLogPatterns`'
on-the-fly fallback used a SQL `mode() WITHIN GROUP`, which DataFusion has no aggregate for, so
the query could not be planned at all against the store production actually reads. Dormant since
February, found in one CI run, fixed in `13982e099` by tallying the modal value in Haskell.

That is the argument for eventually running a TF-read suite deliberately — a separate,
opt-in target that tolerates asynchrony (poll-until-visible rather than assert-immediately) and
isolates by project, rather than flipping the default suite's read path. `make test-integration-tf`
is the natural home. Today's pin-to-`False` is the unblock, not the end state.

## Verifying a change to this

Local green proves nothing here, because locally `tfEnabled` is `False` and the read path under
test never runs. Reproduce with a real TimeFusion:

```
make timefusion-start
TIMEFUSION_PG_TEST_URL=postgresql://postgres:postgres@localhost:12345/postgres \
  LOG_LEVEL=attention USE_EXTERNAL_DB=true \
  cabal test integration-tests --ghc-options="-O0" --test-show-details=direct \
  --test-options='--match /Opentelemetry.GrpcIngestion/'
```

With the pin restored, `Test 4.1` fails `expected [4], but got [17]`. With reads off, 41/41 pass.
A local TimeFusion accumulates between runs exactly as CI's does, so wipe it or a genuinely fixed
run will fail on the previous run's rows.
