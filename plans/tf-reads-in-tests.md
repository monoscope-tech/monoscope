# TimeFusion reads in the integration suite

History of a decision that was made twice, 2026-08-25. **Outcome: reads stay ON in CI**
(`enableTimefusionReads = tfEnabled` in `Pkg.TestUtils`). This document exists because the
first answer was the opposite one, and the reasoning that overturned it is worth keeping.

## What happened

`bac2a9a5d` pinned `enableTimefusionReads = tfEnabled`. `tfEnabled` is
`isJust TIMEFUSION_PG_TEST_URL` — unset locally, **set in CI**, where a real `timefusion`
container runs. So every telemetry read in CI moved from *per-example, synchronous Postgres*
to *one shared, asynchronous DataFusion*, and 23 examples across seven specs began depending
on what had already run. Locally everything stayed green, which is why it shipped.

Two distinct failure shapes came out of it:

- **Inflated counts** (`expected 2, got 16`). TimeFusion is one instance for the whole run,
  keyed only by `project_id`, with no per-example reset, while Postgres is cloned per example.
  Every spec used `testPid = UUID.nil`, so an absolute `count(*) WHERE project_id = <nil>`
  measured whatever ran first.
- **Deflated counts** (`expected 500, got 65`; `expected 100, got 1`). Write→read visibility:
  a spec that writes rows and reads them back in the same breath is asserting read-your-writes,
  which TimeFusion does not offer.

## The rejected answer, and why it was wrong

Pinning reads to `False` fixes both shapes in one line, and was committed on that basis — the
deflated shape looked unfixable, so the store seemed unusable for synchronous assertions.

That reasoning was too pessimistic about the *assertions* and too casual about what the pin
bought. Reads-on had already paid for itself within a single CI run by surfacing a **real
production bug**: `fetchLogPatterns`' fallback used a SQL `mode() WITHIN GROUP`, which
DataFusion cannot plan, against the store production actually reads — dormant since February,
fixed in `13982e099`. Turning reads off would have discarded exactly the signal that found it,
and every future one like it.

The pin-to-`False` commit was reverted once the alternative was shown to work.

## The answer that held

Fix the tests, not the routing — `226d42b05`, `7dbd35b71`, `13b93a2be`, `58963baec`:

- route telemetry reads per query rather than globally,
- isolate count assertions from a shared store (a delta, or a project of the test's own —
  `Pkg.TestUtils.createTestProject` mints one under `nextRandom`; the deterministic test UUID
  stream restarts identically every example and would collide),
- read attributes through `variant_get` on TimeFusion instead of `#>>`,
- give shared-fixture examples their own project and distinct messages.

That took CI from 23 failures to 1.

## What is still open

One example: `Pages.LogExplorer.Log / Time Range Selection / should respect exact time
boundaries`, failing `predicate failed on: 0`.

It looks like the deflated read-your-writes shape, and it is **not** — that was tested and
disproved on PR #502. Raising the poll budget from 10s to 30s changed nothing: the run spent
the extra time (shard 0 went 100s → 128s) and still read back 0 rows.

What is ruled out, by elimination:

- *not* write visibility — 30s of polling makes no difference;
- *not* fresh-project writes failing — every other `createTestProject` example in the spec passes;
- *not* explicit `from`/`to` bounds — many passing examples pass them.

What is left: this is the only example whose window is **narrow and offset into the past**. Its
rows take `timestamp` and `date` from the message (`ProcessMessage.hs:420,448`), so they land in
the `date=2024-12-31` partition while `frozenTime` is `2025-01-01`. TimeFusion partitions on
`date`. So the first thing to check is whether anything in the query path derives a `date`
predicate from "now" rather than from the requested window — that would prune away the very
partition the rows are in, on TimeFusion only, and would be invisible on Postgres.

If that is it, it is a **product** bug and not a test bug: any user querying a narrow window in
the past would lose rows the same way.

## Verifying any change here

Local green proves nothing: locally `tfEnabled` is `False`, so the read path under test never
runs. Use a real TimeFusion:

```
make timefusion-start
TIMEFUSION_PG_TEST_URL=postgresql://postgres:postgres@localhost:12345/postgres \
  LOG_LEVEL=attention USE_EXTERNAL_DB=true \
  cabal test integration-tests --ghc-options="-O0" --test-show-details=direct \
  --test-options='--match /Opentelemetry.GrpcIngestion/'
```

**Wipe MinIO first** — a local TimeFusion accumulates between runs exactly as CI's does:

```
aws --endpoint-url http://127.0.0.1:9000 s3 rm \
  s3://timefusion-test/timefusion-minio-test --recursive
```

Skipping that wipe produces "failures" that are only the previous run's rows; it cost a full
suite run to learn.
