# TimeFusion `TuplesOk` false-positive `POISON_ROW_DROPPED` flood

## Symptom

Production background-job logs are flooded with `POISON_ROW_DROPPED` entries
whenever TimeFusion writes are enabled. Every entry shows the same Hasql
error shape:

```
HasqlException (SessionUsageError
  (StatementSessionError 1 0
    "INSERT INTO otel_logs_and_spans (...) VALUES ($1,...,$89)"
    [...row data...]
    True
    (UnexpectedResultStatementError
      "Unexpected result status: TuplesOk. Expecting one of the following: [CommandOk]")))
```

Notably:

- The row payloads look fine (well-formed timefusion container logs).
- One log line is emitted **per row** in the batch, not per batch.
- Volume scales with ingest throughput when `enableTimefusionWrites=True`.
- Despite the "dropped" label, rows are in fact landing in TimeFusion.

## Root cause

`src/Models/Telemetry/Telemetry.hs` has two layers that both try to handle the
TF wire-protocol quirk where TimeFusion's PGWire returns `TuplesOk` (a tuple
result) instead of `CommandOk` for some multi-row `INSERT ... VALUES` shapes
under the extended query protocol. The data lands; only the wire tag is wrong.

### Layer 1 — `retryTimefusion` (outer wrapper, line 910–929)

```haskell
retryTimefusion n recs =
  tryAny (labeled @"timefusion" @Hasql $ bulkInsertOtelLogsAndSpans recs) >>= \case
    Right count -> Log.logTrace "TimeFusion write succeeded" ...
    Left e
      | "TuplesOk" `T.isInfixOf` show e ->
          Log.logTrace "TimeFusion write succeeded (TuplesOk wire-mismatch ignored)" ...
      | n > 0, Hasql.isTransientException e -> ... retry ...
    Left e -> throwIO e
```

This swallow exists, but it sits **outside** `bulkInsertOtelLogsAndSpans`.

### Layer 2 — `bulkInsertOtelLogsAndSpans` bisector (line 1020–1043)

```haskell
go d pairs =
  tryAny (Hasql.session $ HSession.statement () (stmt pairs)) >>= \case
    Right n -> pure n
    Left e
      | Hasql.isTransientException e -> throwIO e
      | V.length pairs == 1 ->
          Log.logAttention "POISON_ROW_DROPPED" ... $> 0       -- ← false positive emitted here
      | d <= 0 -> ... throwIO e
      | otherwise -> bisect
```

When the TF slice insert returns `TuplesOk`:

1. `tryAny` catches `HasqlException` at the per-slice call.
2. `isTransientException e` is **false** for `UnexpectedResultStatementError`.
3. `V.length pairs > 1`, so the bisect branch runs.
4. Each half also returns `TuplesOk` → recursion continues all the way down.
5. At single-row slices, the `V.length pairs == 1` arm fires and logs
   `POISON_ROW_DROPPED` — once per row in the original batch.
6. The bisector returns `0`, summed up the tree. The outer call sees a
   `Right` (a small/zero count), never reaches the `TuplesOk` swallow in
   `retryTimefusion`, and emits the "TimeFusion write succeeded" trace.

So the rows **do** land (TF stored them on the first slice attempt), but
monoscope simultaneously logs hundreds of attention-level "dropped" events.

Volume per batch: a 512-row slice bisects fully to 512 leaf failures →
512 `POISON_ROW_DROPPED` lines per slice, capped by `bisectCap = 2^9 = 512`.

### Why the PG path looks identical in the logs

The dual-write path runs PG and TF in parallel threads (line 866 PG,
line 867 TF) and both call `bulkInsertOtelLogsAndSpans`. PG returns proper
`CommandOk`, so PG never trips the bisector this way. Every observed
`POISON_ROW_DROPPED` originates from the TF (`labeled @"timefusion"`) thread.

(Confirm in production by checking the `error` field in the log payload —
the SQL text is the same on both, but the `tryAny` site is the labeled
session. We can add the `pool_label` to the `POISON_ROW_DROPPED` payload as
part of the fix to make this unambiguous in future incidents.)

## Why the current outer swallow is insufficient

`retryTimefusion` was written assuming `bulkInsertOtelLogsAndSpans` either
returns success or rethrows. That contract is broken by the bisector's
"degrade to per-row poison drop on terminal failure" branch — for the
`TuplesOk` case, every slice is "terminally failing" from the bisector's
point of view, and every leaf becomes a logged drop returning `0`. The
exception never escapes, so the outer swallow never fires.

## Blast radius assessment

- **Data**: none. Rows land in TF on the first slice attempt. The
  bisector's per-leaf retries do re-attempt inserts, but TF dedups on
  `(id, timestamp)`, so we're not duplicating either.
- **Logs / alerting**: high. `logAttention` is the level paging humans;
  this floods the alert pipeline and trains operators to ignore the
  message.
- **Cost**: every false-positive triggers a full bisection (≈ 1023 extra
  TF round trips per 512-row slice — one per internal node + one per
  leaf), which is a real load amplifier on both monoscope ingest workers
  and TimeFusion.
- **Observability of real poison rows**: poisoned by noise. A genuine
  `POISON_ROW_DROPPED` is now indistinguishable from the TF wire quirk
  without grepping the `error` field.

## Fix

Move the `TuplesOk` wire-mismatch handling **into** `bulkInsertOtelLogsAndSpans`
at the per-slice `tryAny` site, treating it as success with the slice's row
count. Then the outer swallow in `retryTimefusion` becomes redundant.

### Patch sketch

```haskell
go d pairs =
  tryAny (Hasql.session $ HSession.statement () (stmt pairs)) >>= \case
    Right n -> pure n
    Left e
      -- TF PGWire returns TuplesOk instead of CommandOk for some multi-row
      -- INSERT shapes under the extended query protocol. The rows DID land;
      -- only the wire-protocol result tag is wrong. Treat as success with the
      -- slice's row count so bisection doesn't fan out into per-row "poison"
      -- false positives.
      | "TuplesOk" `T.isInfixOf` show e ->
          pure (fromIntegral (V.length pairs))
      | Hasql.isTransientException e -> throwIO e
      | V.length pairs == 1 ->
          Log.logAttention "POISON_ROW_DROPPED" ... $> 0
      | d <= 0 -> ... throwIO e
      | otherwise -> bisect
```

Then remove the now-dead `TuplesOk` arm in `retryTimefusion`.

### Why "as success" is safe

- TF's `INSERT INTO ... VALUES` is atomic per statement: either the whole
  slice committed or it didn't. A `TuplesOk` tag implies the server
  executed the statement to completion (it would have returned an error
  status otherwise) and is just reporting the result row in tuple form
  rather than command-tag form.
- We do not consume the returned tuples for anything meaningful — we ask
  for `D.rowsAffected`, which Hasql can't decode from `TuplesOk` (hence the
  exception), but the `INSERT` succeeded server-side.
- We're returning `V.length pairs` rather than a real `rowsAffected`; this
  matches caller expectations (`bulkInsertOtelLogsAndSpansTF` only logs the
  count for tracing — no business logic depends on the exact value).

### Why scoping inside the bisector is better than scoping outside

- Symmetry with the failure model the bisector implements: each `Left e`
  arm is asked "is this poison, transient, or benign?" The TF wire tag is
  in the "benign" bucket and belongs there next to `isTransientException`.
- Prevents the unnecessary bisection round trips entirely (no log2(slice)
  retry storm on every healthy TF batch).
- Single source of truth — no need to keep two layers in sync.

### Defensive add: include pool label in the poison log

While we're in here, add a `pool_label` field to `POISON_ROW_DROPPED` so
"is this PG or TF?" doesn't require re-reading the SQL. Cheap and avoids
a repeat of this investigation.

```haskell
Log.logAttention "POISON_ROW_DROPPED"
  (AE.object
    [ "id" AE..= (fst (V.head pairs)).id
    , "error" AE..= show @Text e
    -- Note: bulkInsertOtelLogsAndSpans is called via both `Hasql` (PG) and
    -- `labeled @"timefusion" @Hasql` (TF). We can't distinguish here without
    -- threading a label argument — pick the simpler option and thread it.
    ])
```

Actually, threading the label adds a parameter touching every caller.
Cheaper alternative: have `bulkInsertOtelLogsAndSpansTF` log its own
`POISON_ROW_DROPPED` *summary* per slice failure (count + first id) when
the inner call returns `0` from a non-empty input. Defer; keep the fix
narrow.

## Verification plan

### 1. Reproduce with a failing test (per bug-fix workflow)

Add an integration test in `test/integration/` that:

- Stands up the TF Hasql pool (or uses a mock `Hasql` effect that returns
  `UnexpectedResultStatementError "Unexpected result status: TuplesOk..."`).
- Calls `bulkInsertOtelLogsAndSpans` with a >1-row vector.
- Asserts:
  - The returned count equals the input length.
  - No `POISON_ROW_DROPPED` log line was emitted.
  - The session was invoked exactly **once** (no bisection).

Without the fix, the test fails on the bisection round-trip count and the
log assertion. With the fix, it passes.

If a mock at the Hasql layer is too invasive, fall back to a unit-level
test of `go` extracted into a helper that takes the statement runner as
an argument — but prefer integration so we exercise the labeled effect
wiring too.

### 2. Manual prod-shape replay

Pull one of the offending SQL+params payloads from the log entries quoted
above, replay against a local TF instance with `psql` to confirm the
server-side outcome is in fact a successful insert returning a tuple
result. (Sanity check on the "rows actually land" claim.)

### 3. Post-deploy observation

After deploy, watch:

- `POISON_ROW_DROPPED` rate in the alert pipeline should drop ~to zero on
  TF-enabled projects (residual hits are real poison rows worth
  investigating).
- TF write latency p50/p99 should drop slightly (no more 1023× round-trip
  amplification per batch).
- TF row counts vs PG row counts should remain at parity (no regression
  in actual writes).

## Alternatives considered

### A. Make `isTransientException` return True for the TuplesOk shape

Bad. `isTransientException` controls retry semantics elsewhere
(`retryTimefusion` and the Hasql wrapper); marking a non-transient
condition transient would cause genuine retries on a non-error.

### B. Fix it upstream — patch Hasql to accept TuplesOk for `D.rowsAffected`

Correct long-term, but out of scope and the wire-tag mismatch is a
TimeFusion server-side quirk we'd want to fix there anyway. File a
TimeFusion-side issue regardless — the proper fix is for TF's PGWire to
return `CommandOk` for INSERTs.

### C. Detect via exception type instead of string match

`show e` matching is fragile. The Hasql `SessionError` constructor is
exposed, so we could pattern-match on
`UnexpectedResultStatementError "Unexpected result status: TuplesOk..."`.
However the message text itself is the discriminator (it's parameterized
by the actual status), so we still end up string-matching on
"TuplesOk" within the message. Worth doing as a follow-up cleanup —
extract a `isTfTuplesOkQuirk :: SomeException -> Bool` helper next to
`isTransientException` and keep both checks structural.

For this fix, mirror the existing string match in `retryTimefusion` to
minimize behavioral delta. Refactor to a structural matcher in a small
follow-up.

## Follow-ups (separate PRs)

1. File a TimeFusion issue: `INSERT ... VALUES` over extended query
   protocol should return `CommandOk`, not `TuplesOk`. Once fixed
   server-side, remove both the swallow in `bulkInsertOtelLogsAndSpans`
   *and* the redundant arm in `retryTimefusion`.
2. Extract `isTfTuplesOkQuirk` helper; use it in place of string match.
3. Add `pool_label` (or equivalent) to `POISON_ROW_DROPPED` payload so
   future incidents are diagnosable without source reading.
4. Save memory entry once verified: "TF PGWire returns TuplesOk for
   multi-row INSERT under extended query protocol — bisector must treat
   as success, not poison".

## Open questions for review

- Is `pure (fromIntegral (V.length pairs))` an acceptable stand-in for
  the real `rowsAffected`? Callers only use the count for tracing
  (`bulkInsertOtelLogsAndSpansTF`'s trace log), so I believe yes — but
  worth a second pair of eyes.
- Should we also short-circuit at the `retryTimefusion` layer (keep both
  swallows) as belt-and-braces, or trust the inner fix and remove the
  outer arm? I lean "remove the outer arm" — duplicated handling has
  already caused confusion once.
- Any other call site of `bulkInsertOtelLogsAndSpans` that depends on the
  bisector emitting `POISON_ROW_DROPPED` for "this slice failed for
  any non-transient reason"? A grep shows only two callers
  (`bulkInsertOtelLogsAndSpansTF`, line 1600's caller via the same
  helper), both telemetry ingest. Confirm before merging.
