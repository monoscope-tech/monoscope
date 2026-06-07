# 2026-06-01: Dual-write segfault, container crash loop, 2h event loss

**Status:** Resolved
**Severity:** SEV-1 (data loss for a subset of customers)
**Impact window:** 2026-06-01, 09:50 – 12:10 UTC (~2h20m) of dropped events for customers in **transition mode**, i.e. dual-write enabled to both Postgres and TimeFusion. Single-store customers (PG-only or TF-only) were unaffected.
**Detected via:** customer report + OOM-kill alerts on the transition-mode ingest node.

## What happened (2026-06-01, UTC)

The Kafka ingestion pipeline is designed as at-least-once with row-UUID idempotency keys: any downstream outage should show up as **lag**, never **loss**.

Two latent bugs combined to break that contract during this window: one in the dual-write fan-out, one in Kafka commit semantics. Blast radius stayed contained to a single container, because `dualExecPgTf` is the only code path that runs two writers under one `Ki` scope, only transition-mode tenants hit it, and those tenants ran on a dedicated ingest node.

- **09:50.** TimeFusion latency on the transition-mode ingest node spikes (slow writes against a recently-compressed chunk set; orthogonal cause). PG now finishes before TF on most batches, which triggers the dual-write bug: `dualExecPgTf` only awaited the PG fiber, so `Ki.scoped` closed and async-cancelled the in-flight TF Hasql session. Hasql's bracket then ran libpq cleanup on a socket that was still mid-call → NULL deref → container exit 139 (SIGSEGV).

  Customer-visible loss begins here because the Kafka consumer commits the offset as soon as the PG write returns, and on a segfault the process dies after PG returns but before TF has landed. The offset moves; the TF write never happens; nothing redelivers.
- **~10:15.** Crash loop established. Heap reload on each restart plus the extraction-worker backlog push RSS over the swarm limit; OOM kills now interleave with the segfault restarts. Each restart re-arms the same scope-close race on the next batch, and each successful PG write before the next crash commits another offset.
- **12:10.** TF latency recovers, the segfault stops firing, the container stabilises, ingestion resumes. By this point the Kafka offsets have advanced past 2h20m of events that never landed in TF (and, for batches that crashed mid-PG-flush, never landed in PG either). Kafka does not redeliver committed offsets, so those messages are unrecoverable.

## Root causes and fixes

### 1. Scope-close race segfaulting libpq ([Fix: `e484dad9`](https://github.com/monoscope-tech/monoscope/commit/e484dad9))

```haskell
-- before
Ki.atomically $ Ki.await postgresInsert
-- after
Ki.atomically (Ki.awaitAll scope *> Ki.await postgresInsert) -- scope includes the timefusionInsert
```

`postgresInsert` is the PG writer. Awaiting only it let `Ki.scoped` exit while the TF fiber was still mid-Hasql-session; Ki then async-cancelled the TF fiber, Hasql's bracket ran libpq cleanup on a socket mid-call, and libpq dereferenced a freed pointer. Because the crash is at the libpq/C layer, no Haskell handler can catch it, so the fix has to prevent the cancellation rather than recover from it. Awaiting the whole scope keeps it open until the TF fiber finishes; `tryAny` inside that branch guarantees the TF result never throws past the scope.

### 2. Commit-on-either-store breaking at-least-once ([`7a4b94cf`](https://github.com/monoscope-tech/monoscope/commit/7a4b94cf), [PR #402](https://github.com/monoscope-tech/monoscope/pull/402))

Top-to-bottom rewrite of commit semantics:

- **`kafkaService`** commits per `(topic, partition)` via `commitPartitionsOffsets`. Failed groups contribute no offset; lag growth becomes the outage signal. The DLQ topic is no longer subscribed by the live consumer.
- **`pubsubService`** mirrors the same split: poison → DLQ-then-ack, transient → no ack.
- **`publishToDeadLetterQueue`** returns `Either Text ()` so callers can gate commit on DLQ success.
- **`bulkInsertOtelLogsAndSpansTF`** now _requires_ TF success for a normal return; PG-only failure is logged as `PG_WRITE_FAILED_TF_OK` and tolerated, because PG is the legacy store being phased out (per the storage-migration plan in `CLAUDE.md`) and TF is the source of truth going forward. This inverts the previous "PG mandatory, TF best-effort" asymmetry that caused the silent loss.
- **`bulkInsertOtelLogsAndSpans`** splits a failing batch in half to isolate the poison row on non-transient Hasql failures (depth-capped, `BISECT_DEPTH_EXHAUSTED` logged), so a single bad row no longer wedges its partition.
- The brittle string-match `isUnrecoverableError` is replaced with `Hasql.isTransientException`; unknown errors now default to **poison** (DLQ-actionable) rather than **transient** (silent stall).

### 3. Throughput and headroom hardening

So a recurrence degrades to lag instead of crashing:

- [`ecc4eb2b`](https://github.com/monoscope-tech/monoscope/commit/ecc4eb2b) ([PR #412](https://github.com/monoscope-tech/monoscope/pull/412)): per-`(topic, partition)` group concurrency via `pooledForConcurrentlyN` (`kafkaGroupConcurrency`, default 4); a failed group only stalls its own partition. Adds OTel histograms `monoscope.ingest.decode.duration` and `monoscope.ingest.write.duration` for dual-write tail latency.
- [`4ee5350a`](https://github.com/monoscope-tech/monoscope/commit/4ee5350a): extra Kafka processor fiber per node so a slow batch doesn't block the only consumer.
- [`fe77a26e`](https://github.com/monoscope-tech/monoscope/commit/fe77a26e): more aggressive RTS memory release to give catch-up restarts OOM headroom.

## Post-fix expected behaviour

The same TF latency spike now produces growing consumer lag on the transition node and zero data loss, paged via the metric alert `ingest.batches.retried > 0 for >5min` and a DLQ topic age > 1h.
