# Plan: Kafka poll-loop parallelism — revised after Codex review

## TL;DR

**The original plan (per-partition worker fibers + bounded TBQueues + pause/resume backpressure + rebalance reaping) was wrong.** Codex's pushback is correct on all five substantive points and is preserved here so we don't re-discover them. The right sequence is **measure → tune what's already there → if needed, drop in `pooledForConcurrentlyN` on the existing poll thread**. No registry, no per-partition queues, no decoupling of poll from process.

---

## 1. Why the original plan was wrong

### 1.1 The parallelism it adds already exists one layer down

CPU-heavy schema extraction is already fanned out across **4 `ExtractionWorker` shards** with their own `TBQueue`s (`extractionWorkerShards = 4`, `extractionQueueCapacity = 64`, `System/Config.hs:216`). `Telemetry.insertAndHandOff` (`Models/Telemetry/Telemetry.hs:952`) is:

```haskell
bulkInsertOtelLogsAndSpansTF enableTf records   -- one bulk insert
liftIO $ handOffBatches worker caches records    -- async → shards
```

The poll thread's synchronous cost per batch is essentially **one bulk insert**. The original plan rebuilds — at the wrong layer — machinery the codebase already has.

### 1.2 It fragments the bulk insert (anti-efficiency)

Today: one poll → one `fn` per topic → one big `bulkInsert` over all records from all partitions of that topic.
Original plan: N partitions → N smaller inserts competing for the pool. More round trips, worse batching, worse compression on the wire. Directly contradicts the "more efficient" goal.

### 1.3 Real parallelism is capped by the connection pool, not partition count

- `hasqlPool` = **20** (shared with the web server)
- `hasqlTimefusionPool` = **30**
- `hasqlJobsPool` = **10**

(`System/Config.hs:327-329`.) 32 partition workers cannot deliver 32× — they block on `Pool.acquire`. Worse: a 30s acquisition timeout surfaces as `AcquisitionTimeoutUsageError`, which `Hasql.isTransientException` classifies as transient → **no commit → redelivery on next poll → more workers waiting on the same pool → cascade**. The original plan turns pool exhaustion into a redelivery storm.

### 1.4 It contradicts the conciseness and memory goals

A `IORef (Map (TopicName, PartitionId) PartitionWorker)`, a per-partition `TBQueue`, a side stash for paused partitions, periodic assignment diffing, async lifecycle management, and rebalance wiring add ~150 lines of stateful glue to a 389-line file — and stack a third buffer on top of librdkafka's fetch queue and the ExtractionWorker queues. Net resident memory grows; net LOC grows.

### 1.5 It re-introduces what PR #402 deliberately removed

The comment at `Pkg/Queue.hs:298-300` is explicit: lag = the backpressure/outage signal; do not decouple poll from process. The original plan decouples them with a queue, which is exactly the regression #402 removed.

---

## 2. What to do instead

### Step 1 — measure (no code change)

Establish where the wall time actually is **before changing anything**:

1. Read `kafka.process_batch` span durations from the existing tracing (already emitted in `Pkg/Queue.hs:262`). Break down: what fraction is `bulkInsertOtelLogsAndSpansTF` vs. `handOffBatches`'s STM submit vs. parse/cache vs. waiting on `Pool.acquire`?
2. Sample `Pool.acquire` wait time on `hasqlPool` and `hasqlTimefusionPool` over a busy minute. If wait is > a few ms, the pool is the ceiling — adding consumer-side parallelism makes it worse, not better.
3. Check ExtractionWorker shard saturation: are the 4 shard `TBQueue`s near `extractionQueueCapacity = 64`? If yes, the cheap unlock is just bumping `extractionWorkerShards` or `extractionQueueCapacity`. **Zero `Queue.hs` change required.**
4. Are commits the bottleneck (rare)? `commitPartitionsOffsets … OffsetCommitAsync` is fire-and-forget today; if not, we'd see it in the span.

These four data points decide what (if anything) to change.

### Step 2 — if measurement points at the insert, tune the existing levers first

In rough order of effort:

- **Bigger Kafka poll batches** (`messagesPerKafkaPullBatch` / consumer `fetch.max.bytes`) → larger single inserts, better amortization.
- **Larger TF pool** if its `Pool.acquire` wait dominates.
- **More extraction shards** if those queues saturate.

All of these are config knobs. None touch `Queue.hs`.

### Step 3 — if measurement *still* points at the poll loop's serial `processGroup`, use Codex's 5-line fix

Group by `(topic, partition)` instead of topic, and run `processGroup` with bounded concurrency from the **same** poll thread:

```haskell
-- replace:
let byTopic = foldr (\r -> HM.insertWith (<>) r.crTopic.unTopicName (r :| [])) HM.empty rightRecords
tps <- concatMapM processGroup (HM.toList byTopic)

-- with:
let byTP = foldr (\r -> HM.insertWith (<>) (r.crTopic.unTopicName, r.crPartition) (r :| [])) HM.empty rightRecords
tps <- concat <$> pooledForConcurrentlyN kafkaProcessConcurrency (HM.toList byTP) processGroup
```

`pooledForConcurrentlyN` is from `unliftio-pool` (`UnliftIO.Async`), already in the dep tree. Pick a small bound — start with **4** (≈ TF pool stripes; well below 20) — and make it env-configurable.

**Why this is safe and aligned with PR #402:**

- One poll → process (concurrent) → commit → poll, still a single thread of control. `max.poll.interval.ms` behavior unchanged.
- One committer (the poll thread). No concurrent `commitPartitionsOffsets` calls; no librdkafka thread-safety probe required.
- `tpsFor`, `ceTypeFor`, `processGroup`, `publishToDeadLetterQueue`, DLQ-on-poison, transient-skip, per-partition commit semantics: all used **verbatim**.
- No worker registry, no `TBQueue`s, no stash, no pause/resume, no rebalance wiring.
- Bound is **pool headroom**, not partition count — eliminates the redelivery-storm risk in §1.3.
- Compared to today: strictly faster on the same contract. Compared to original plan: smaller code, smaller memory, no new failure modes.

**Trade vs. original plan:** the poll thread still waits for *this batch's groups* to finish before polling again. Today's code already waits — so this is pure improvement, not a regression.

---

## 3. Decisions on the original §7 open questions

- **`setRebalanceCallback`** — moot. Drop.
- **Pause/stash machinery** — moot. Drop.
- **Per-partition metrics** — moot for this change. Separately worth adding `Pool.acquire` wait + ExtractionWorker queue depth as metrics (existing `TODO(otel-metrics)` markers), but that's a different PR.

---

## 4. Action items

1. **(now)** Pull the four measurement signals in §Step 1 from staging. Decide whether anything in `Queue.hs` needs to change at all.
2. **(if config wins)** Bump `extractionWorkerShards` / `extractionQueueCapacity` / pool sizes and re-measure.
3. **(if poll-loop wins)** Land the ~5-line `pooledForConcurrentlyN` change with `KAFKA_PROCESS_CONCURRENCY` env var (default 4).
4. **Do not** land the original per-partition-fiber plan.
