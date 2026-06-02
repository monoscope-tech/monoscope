# Ingestion Durability Plan

Goal: no committed Kafka / acked PubSub message is ever lost when a downstream store is unavailable. During a TimescaleDB or TimeFusion outage, consumer-group lag grows; data is never silently discarded.

## Current failure modes (recap)

Three bugs compound to lose data when TimescaleDB is down:

1. **`Pkg/Queue.hs` commits offsets unconditionally.** `kafkaService` runs `K.commitAllOffsets` after the result branch, regardless of `Left e`. `pubsubService` mirrors this — on failure it still returns `map fst validMsgs` for ack.
2. **`publishToDeadLetterQueue` is fire-and-forget.** Returns `()`, only `logAttention`s on publish failure, so a failed DLQ write still leads to a committed offset.
3. **Dual-write is PG-mandatory.** `bulkInsertOtelLogsAndSpansTF` wraps TimeFusion in `tryAny` (swallow) but lets Postgres exceptions propagate, so a successful TF write is invisible upstream when PG fails.

The DLQ topic is also re-consumed by the same `kafkaService` loop through the same `fn`, so during a multi-hour outage it acts as a write-amplifier with no separate backoff or quarantine.

## Design principles

- **Commit = at least one durable store accepted the batch.** "Tried" is never "done."
- **Row UUIDs are the idempotency key.** Already minted upfront in `bulkInsertOtelLogsAndSpans` callers; reuse on retry so re-delivery after lag is safe.
- **Each writer reports its own outcome.** No store's failure cancels another store's success.
- **Backpressure via Kafka lag, not DLQ.** During infra outages the right behavior is to *stop committing* and let lag grow. DLQ is for *poison messages*, not infra failures.
- **Distinguish poison from transient.** `isUnrecoverableError` already exists for this — extend its meaning so only poison messages go to DLQ; transient failures cause non-commit + backoff.

## Step-by-step

### 1. `Telemetry.bulkInsertOtelLogsAndSpansTF` returns a structured outcome

Change signature:

```haskell
data WriteOutcome = WriteOutcome
  { pgWrote :: !WriteResult
  , tfWrote :: !WriteResult
  }

data WriteResult = Wrote | TransientFail !SomeException | PermanentFail !SomeException

bulkInsertOtelLogsAndSpansTF
  :: (...)
  => Bool -> V.Vector OtelLogsAndSpans -> Eff es WriteOutcome
```

Both forks wrap their write in `tryAny`; neither re-raises. Caller decides what to do. `retryTimefusion` widens to exponential backoff (e.g. 0.5s × 2^n, capped, ~5 min total) and only counts `HasqlException` as transient — programmer-bug exceptions stay permanent and surface immediately.

Add a `wroteAtLeastOne :: WriteOutcome -> Bool` helper and `isPermanentFailure :: WriteOutcome -> Bool` (both stores rejected with non-transient errors → poison).

### 2. `insertAndHandOff` propagates the outcome upward

Today it returns `()`. Make it return `WriteOutcome`. Hand-off to the extraction worker still happens, but is gated on `wroteAtLeastOne` — if neither store has the data, don't queue extraction.

### 3. `processMessages` returns per-message status, not flat `[Text]`

Today: `Eff es [Text]` (ackIds to ack). Change to:

```haskell
data MsgOutcome = Acked | Retry | Poison
processMessages :: (...) => [(Text, ByteString)] -> ... -> Eff es [(Text, MsgOutcome)]
```

Mapping:
- JSON parse failure / unknown wire type → `Poison`
- `WriteOutcome` with `wroteAtLeastOne = True` → `Acked`
- Both stores `TransientFail` → `Retry`
- Both stores `PermanentFail` → `Poison`

Partial-batch granularity: today the entire batch shares one fate because writes are bulk. That's fine for v1 — one transient error reschedules the whole batch. v2 can split on failure if needed.

### 4. Kafka consumer commits only `Acked` offsets, DLQs only `Poison`

In `Pkg/Queue.hs` `kafkaService`:

- Build per-partition offset maps from `Acked` outcomes only. Call `K.commitOffsets` (not `commitAllOffsets`) with that map.
- Publish `Poison` to DLQ. **DLQ publish must succeed before its offset commits** — propagate `publishToDeadLetterQueue`'s result.
- `Retry` outcomes: do nothing. The offset stays put; on next poll librdkafka redelivers from the last committed point. Add a sleep proportional to consecutive-failure count (`backoff = min 30s (2^n * 100ms)`) before the next `pollMessageBatch` to avoid hot-spinning during a long outage.

Implementation detail: per-partition offset bookkeeping. `K.ConsumerRecord` carries `crPartition` and `crOffset`; build `Map TopicPartition Offset` of max-acked-offset-per-partition, commit that. For correctness when a mid-batch message is `Retry`, commit only up to the last contiguous `Acked` per partition — gaps stay uncommitted.

### 5. PubSub consumer mirrors the same split

`pubsubService`: build `ackIds` from `Acked` outcomes only. `Poison` → DLQ (with success check) → ack. `Retry` → do nothing; PubSub redelivers after the ack deadline. The current `Left e → pure (map fst validMsgs)` swallow goes away.

### 6. `publishToDeadLetterQueue` returns `Either Text ()`

Caller checks. If DLQ publish fails on a `Poison` message we have a real dilemma — the message is broken and we can't park it. Options:
- v1: log + retry-with-backoff + do not commit (so it redelivers). Yes, this means a broken poison message held up by a broken DLQ blocks its partition. That's correct: surfaces as lag, pages on-call.
- v2: secondary local on-disk queue.

Start with v1.

### 7. DLQ is no longer subscribed by the live consumer

Remove `appCtx.config.kafkaDeadLetterTopic` from `allTopics` in `kafkaService`. DLQ becomes write-only from the live loop.

Add a separate `kafkaDeadLetterReplayService` (new function in `Pkg/Queue.hs`) that:
- Consumes only the DLQ topic, with its own consumer group.
- Throttles aggressively (e.g. one batch per 30s).
- Routes back through the same `fn` (processMessages) — but with a header `replay-attempt: N` and a max-attempts cutoff (e.g. 5) before it stays in DLQ forever (alerting on age).
- Runs as a separate `Ki.fork` in `Start.hs` alongside the main service.

Gate it behind `ENABLE_DLQ_REPLAY` env var for safe rollout.

### 8. Metrics & observability

Add (`TODO(otel-metrics)` markers, OK to start with `logAttention` until the metrics API lands):
- `ingest.batches.acked` / `.retried` / `.poisoned` (counters, by topic).
- `ingest.write.pg.duration` / `.tf.duration` (histograms).
- `ingest.write.pg.outcome{result=wrote|transient|permanent}` (counter).
- `ingest.commit.lag.seconds` (gauge — gap between newest record in batch and now).
- Alert on `ingest.batches.retried` rate > 0 for >5min, and on DLQ topic age > 1h.

### 9. Tests (mandatory — failing-test-first)

Integration tests in `test/integration/`:
- **`ingestion_db_down_does_not_commit`**: simulate Hasql failure (point `DATABASE_URL` at a closed port mid-test), publish a Kafka message via test harness, assert offset did not advance and message is redelivered on next poll. **Write this first, watch it fail on master, then fix.**
- **`ingestion_tf_down_pg_up_acks`**: stub the timefusion connection to fail, assert offset commits and PG has the row.
- **`ingestion_poison_routes_to_dlq_and_commits`**: send malformed JSON, assert DLQ received it and original offset committed.
- **`ingestion_dlq_publish_fail_blocks_commit`**: stub DLQ producer to fail, assert offset does NOT advance.
- **`dlq_replay_loops_then_parks_at_max_attempts`**: send a permanently-poison message, run replay service N+1 times, assert it stops re-entering the live pipeline.

Doctests for pure helpers: `wroteAtLeastOne`, contiguous-offset computation, `isPermanentFailure`.

### 10. Rollout

The whole change can ship behind a single env flag `INGEST_STRICT_COMMIT=true`. Default false for one deploy → confirm metrics look sane in staging and prod (no commit-stall, no DLQ flood) → flip to true → remove flag a week later. Keep the flag wiring trivial so revert is one env change.

## Out of scope (call out, don't do now)

- Splitting failed batches into per-message retries. Whole-batch retry is fine while batch sizes are ≤ a few thousand.
- Cross-store transactional consistency. We're at-least-once with idempotency keys; that's the right tier for telemetry.
- Replacing DLQ with an on-disk WAL. Useful long-term but a separate project.

## File touchpoints

- `src/Pkg/Queue.hs` — biggest change. `kafkaService`, `pubsubService`, `publishToDeadLetterQueue`, new `kafkaDeadLetterReplayService`.
- `src/Models/Telemetry/Telemetry.hs` — `bulkInsertOtelLogsAndSpansTF`, `insertAndHandOff` signatures; new `WriteOutcome` type.
- `src/ProcessMessage.hs` — `processMessages` signature + per-message outcome mapping.
- `src/Opentelemetry/OtlpServer.hs` — OTLP gRPC path also calls `insertAndHandOff`; adapt to the new return type (gRPC has its own semantics — propagate transient as `UNAVAILABLE`, poison as `INVALID_ARGUMENT`).
- `src/System/Config.hs` — add `enableDlqReplay`, `enableStrictCommit`.
- `src/Start.hs` — fork the DLQ replay service.
- `test/integration/` — new specs above.

## Estimated effort

~2-3 days for the core (Queue.hs + Telemetry.hs + ProcessMessage.hs + OtlpServer.hs adaptation) including the failing-test-first work. +1 day for DLQ replay service. +0.5 day for metrics/alerts. Rollout window 1 week with the env flag.
