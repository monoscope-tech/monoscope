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

## The last one — RESOLVED, and it was a real store bug

**Outcome: a TimeFusion bug, fixed in timefusion `bf3abc8`; the example is restored
(`a21485f30`), not pending.** Read the elimination trail below with that ending in mind — the
reasoning is kept because every step of it was sound and still describes how to attack this
class, but the conclusion it was heading toward ("test artifact, prod unaffected") was wrong.

A mem bucket advertised its **routing** timestamp instead of the range of rows it actually
held. A batch spanning 21:00–23:00 reported `max=21:00`, so any `timestamp >= 21:30` pruned the
whole bucket. Only *unflushed* rows were ever hidden, which is why the prod spot-checks below
(2–3h and 25–26h back, both reading flushed parquet) came back healthy and looked like
counter-evidence when they were consistent with the bug.

The example is kept rather than deleted because it is the only one that catches this class: it
ingests several rows in one batch and then reads a window above the earliest. Single-row
inserts make `min == max` per batch and hide it completely.

### How it looked while open

One example: `Pages.LogExplorer.Log / Time Range Selection / should respect exact time
boundaries`, failing `predicate failed on: 0`.

It looks like the deflated read-your-writes shape, and it is **not** — that was tested and
disproved on PR #502. Raising the poll budget from 10s to 30s changed nothing: the run spent
the extra time (shard 0 went 100s → 128s) and still read back 0 rows.

What is ruled out, by elimination:

- *not* write visibility — 30s of polling makes no difference;
- *not* fresh-project writes failing — every other `createTestProject` example in the spec passes;
- *not* explicit `from`/`to` bounds — many passing examples pass them.

What is left, as a *description* rather than a diagnosis: this is the only example whose window
is **narrow and offset into the past**. Its rows take `timestamp` and `date` from the message
(`ProcessMessage.hs:420,448`), so they land in the `date=2024-12-31` partition while `frozenTime`
is `2025-01-01`, and TimeFusion partitions on `date`.

That suggested a `date` predicate derived from "now" rather than from the requested window. I
looked: **there is no such predicate in the log-explorer query path**, so that explanation is out
too. Do not spend time on it.

The description was the right scent and the diagnosis was one layer lower than anyone looked:
"narrow window offset into the past" mattered not because of `date` partitioning but because
the window's lower bound sat *above the earliest row of a multi-row batch* — precisely the
shape that the mem bucket's mis-advertised max pruned away.

### A warning about reproducing this locally

Three separate attempts to reproduce it locally produced confident-looking results that were all
environment artifacts, not the bug:

1. a shared TimeFusion whose delta log referenced parquet I had deleted — every query 404s, and
   the log-explorer path renders that as 0 rows, which looks exactly like the failure;
2. the same store again after a wipe that could not be undone (see
   [[local_tf_wipe_corrupts_delta_log]] in the memory directory);
3. a private instance started from `target/release/timefusion` with its own bucket prefix, which
   **accepted 1934 `INSERT INTO otel_logs_and_spans` and then served 0 rows** — almost certainly
   because a standalone run does not flush its batch queue / WAL the way CI's container does.

The lesson is that a hand-rolled local TimeFusion is not a valid test bed for this, and a "0 rows"
result from one proves nothing. Reproduce it in an environment configured the way CI's is, or
iterate through CI itself — a draft PR runs the real-TimeFusion suite without triggering a deploy,
which is how the poll-budget theory above was disproved.

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
