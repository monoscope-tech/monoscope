# Plan: Revert `byTP` insert-fragmentation in `Pkg/Queue.hs`

Follow-up to [`kafka-per-partition-fanout.md`](./kafka-per-partition-fanout.md). That plan ended at: "if measurement points at the poll loop, group by `(topic, partition)` and fan out with `pooledForConcurrentlyN`." That change landed. It made things worse on multi-partition-per-consumer assignments, exactly along the §1.2 anti-efficiency axis the original plan warned about.

## TL;DR

Switch the per-poll grouping back to **per-topic** for the `fn` (insert) call, while **keeping per-partition offsets** in the commit. One big bulk insert per topic per poll (good for TF batching), independent per-partition commit semantics (good for partial-failure isolation). **Keep `pooledForConcurrentlyN`** but key it on `byTopic` instead of `byTP` — parallelism *across* topics (different TF tables, no fragmentation cost) is safe and useful; parallelism *within* a topic was the harm. Rename the config knob from `kafkaGroupConcurrency` → `kafkaTopicConcurrency` (default 3 — one per signal type: logs/spans/metrics).

## Evidence (2026-06-05)

Consumer-group snapshot showed lag concentrated on consumers owning **>1 partition of the same topic** or **mixed-cost partitions**:

| Consumer | Assignments | Lag |
|---|---|---|
| 50c8ec4d | spans p0 + spans p3 | 6.5k + 55k |
| f46ae515 | metrics p3 + spans p2 | 2k + 19k |
| 98987d8c / c1409525 / e81bf80e | metrics + spans (one of each) | 0 / 0 |

`rpk topic consume otlp_spans -p N -n 2000 -o -2000 -f '%V\n'`:

| P | total / 2k msgs | mean | p99 | lag |
|---|---|---|---|---|
| **p3** | **10.4 MB** | **5223** | **51089** | **55k** |
| p2 | 6.4 MB | 3221 | 26450 | 19k |
| p4–p6, p0–p1 | 6.3–8.2 MB | 3129–4116 | 21–29k | 0 / 0 / 7k / 0 / 0 |

- p3 is genuinely fat-tailed (~60% more bytes, ~2× p99) — real shape difference.
- p2 is mid-pack on every metric yet has 19k lag — *not* explainable by data shape.
- Trace-id distribution on p3: 1695 distinct keys in 5000 msgs, top-1 = 1.78%, top-10 = 7.76% → **not** wide-trace skew. Producer keying (`partition_traces_by_id: true`) is fine.

Both lagging consumers own ≥2 partitions of the same topic. All non-lagging consumers own exactly one heavy partition. This is the fragmentation amplifier: byTP splits the consumer's poll batch into N parallel `fn` calls, each its own `bulkInsertOtelLogsAndSpansTF`, all competing for the shared TF pool. The heavier partition loses the race instead of being amortized into a single coalesced insert.

## The change

In `src/Pkg/Queue.hs` (currently lines 246-296):

```haskell
-- replace byTP with byTopic; keep pooledForConcurrentlyN:
let byTopic = foldr (\r -> Map.insertWith (<>) r.crTopic.unTopicName (r :| [])) Map.empty rightRecords
    processGroup (topic, neRecords@(recc :| _)) = do
      let headers    = consumerRecordHeadersToHashMap recc
          ceType     = ceTypeFor appCtx.config.kafkaDeadLetterTopic topic headers
          attributes = HM.insert "ce-type" ceType headers
          allRecords = consumerRecordToTuple <$> toList neRecords
          successTps = tpsFor topic neRecords          -- already multi-partition
      …                                                 -- body unchanged
tps <- concat <$> pooledForConcurrentlyN appCtx.config.kafkaTopicConcurrency (Map.toList byTopic) processGroup
```

That's it. Mechanical revert of the grouping key; the `processGroup` body, span, DLQ handling, and `tpsFor` logic stay identical. `tpsFor` already produces one `TopicPartition` per distinct partition in the topic group, so per-partition commit semantics are unchanged.

Topic count per poll is 1–3 (logs / spans / metrics, plus occasionally DLQ), so the parallelism opportunity is small but real: spans, logs, and metrics inserts hit different TF tables, so running them in parallel doesn't fragment any single insert. Default `kafkaTopicConcurrency = 3` — one slot per signal type, well under the TF pool (30). Scaling horizontally (more monoscope replicas) is expensive because each replica also runs the full web/jobs stack, so squeezing useful in-process concurrency out of each consumer matters.

## Failure semantics — preserved

- **Transient (Hasql) error on a topic's `fn`** → `pure []` (no commit) for **all** partitions of that topic in that poll. librdkafka redelivers. Same as today.
- **Poison error** → DLQ publish, commit `successTps` only if DLQ succeeded. Same as today.
- **Per-partition isolation** — the *concern* that motivated byTP — was *commit* isolation, not *insert* isolation. `tpsFor` already gives independent offsets per partition; a topic-level failure still stalls all the topic's partitions, but that's identical to today's behavior since a topic-level `fn` failure is a TF problem, not a per-partition data problem.

## Action items

1. Land the revert in `src/Pkg/Queue.hs` (≈5 LOC: change the fold key from `(topic, partition)` → `topic`, swap the `pooledForConcurrentlyN` config knob to `kafkaTopicConcurrency`).
2. Rename `kafkaGroupConcurrency` → `kafkaTopicConcurrency` in `System/Config.hs` (default 3); update `KAFKA_GROUP_CONCURRENCY` → `KAFKA_TOPIC_CONCURRENCY` in CapRover envVars if set.
3. Deploy and verify: lag on `otlp_spans` p2 drops to ~0 within minutes (pure fragmentation case); p3 lag shrinks substantially but may not zero out — residual p3 lag is honest fat-tail cost.
4. If residual p3 lag persists after revert, the next lever is **more spans partitions** (producer-side topic config; consumers will rebalance), not more consumer-side concurrency.

## Out of scope

- Producer-keying changes — confirmed `partition_traces_by_id: true` distributes uniformly; no skew to fix there.
- Splitting consumer groups per topic — separate scaling decision, tracked elsewhere.
- Separating ingest from web process — separate scaling decision, tracked elsewhere.
