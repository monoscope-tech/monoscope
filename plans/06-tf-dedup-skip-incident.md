# TimeFusion: cross-project scans fail — `DedupExec key 'id' not in input schema`

Live since **2026-08-09**. Handover doc; the candidate fix is in
[`tf-dedup-skip-fix.patch`](tf-dedup-skip-fix.patch).

## Symptom

The service map renders "No service activity in this range". It is not a rendering bug — the
`apis.service_dependency_edges` rollup stopped being written at **2026-08-09 16:15 UTC**, so a
24h window is genuinely empty. 38 `ServiceMapRollup` jobs are in `failed`, first at
2026-08-09 19:16, all with:

```
SELECT DISTINCT project_id FROM otel_logs_and_spans WHERE timestamp >= $1 AND timestamp < $2
  AND kind IN ('server','client','producer','consumer')

ERROR: Internal error: DedupExec key `id` not in input schema.
```

That is `ServiceGraph.projectsWithSpansInRange`, run every 5 minutes by
`dispatchServiceMapRollups`.

## Reproduced against prod

```
cross-project, project_id only   -> ERROR: DedupExec key `id` not in input schema
cross-project + deleted          -> OK
cross-project + id               -> OK
project-scoped, count(*)         -> OK
project-scoped, GROUP BY service -> OK
```

**Blast radius is limited to queries with no `project_id` equality.** Everything
project-scoped — log explorer, dashboards, charts — is unaffected. Verified, not assumed.

## Cause

Three independent lines of evidence agree that the swept-partition dedup skip is the trigger:

1. **Timing.** `f1f0b90` (2026-08-09) — *"perf(read): enable the swept-partition dedup skip by
   default"* — flipped `timefusion_read_dedup_skip_swept` to `#[serde(default = "d_true")]`.
   Same day the failures start.
2. **Behaviour.** The trigger matrix above is exactly the projection-shape dependency the skip
   path introduces.
3. **Code.** In `ProjectRoutingTable::scan` (`src/database.rs`), `pre_skip_dedup` omits the
   dedup keys from the scan projection:

   ```rust
   let augment = if pre_skip_dedup { Vec::new() } else { dedup_keys.iter().chain(dedup_tiebreak.iter()).collect() };
   ```

   but the skip is then re-decided per leg, and can come back **false** — at which point a
   `DedupExec` is built over a scan that no longer carries `id`/`timestamp`. Two places it can
   flip back:

   - `let skip_dedup = pre_skip_dedup && output_projection.is_none() && dedup_skip_allowed(..)`.
     The middle conjunct is meant to mean *"no dedup-key columns were augmented in"*, but
     `output_projection` also turns `Some` when the **tombstone marker alone** is augmented —
     and `otel_logs_and_spans` declares both dedup keys and a tombstone.
   - the union (cross-project) path, which per the comment at `src/database.rs:15152`
     *"never grants `skip_dedup`"* — while `pre_skip_dedup` was computed once, above it.

   The second is the better fit for "only cross-project queries fail".

## State of the fix — NOT verified, do not ship as-is

`tf-dedup-skip-fix.patch` narrows the first of those (drops the `output_projection.is_none()`
conjunct). It compiles and the suite's dedup tests pass, **but I could not build a test that
reproduces the failure**, so the patch is unproven. Attempts and why they failed:

- A query projecting only `name` on a swept partition: green with *and* without the fix — the
  skip gate is never reached.
- `dedup_skip_allowed` refuses without a time window, so any test whose query has no timestamp
  bound never reaches the gate. Note `count_is_identical_with_and_without_the_dedup_skip` — the
  test cited as the validation gate for enabling this flag — has **no timestamp predicate**,
  so it likely never exercised the skip either. That is plausibly how this shipped.
- Adding literal timestamp bounds still did not reach it; the remaining unknowns are whether
  `try_fast_resolve` hits and whether the swept partition's fingerprint certifies in-harness.
  Confirming needs instrumentation inside `scan`, and `src/database.rs` was being actively
  edited by another session, so I stopped rather than contend for it.

## Options, cheapest first

1. **`TIMEFUSION_READ_DEDUP_SKIP_SWEPT=false`** on the prod TF service. No code, reverts
   `f1f0b90`'s trigger for every caller, and is what the flag's own doc prescribes. Costs the
   read optimisation. Needs a TF restart — which is the risky part (see
   `tf_deploy_wal_lock_churn`, `tf_stopfirst_update_takes_app_down`).
2. **monoscope one-liner.** `projectsWithSpansInRange` selects `project_id, deleted` instead of
   `project_id` alone; verified against prod to return rows where the current query errors.
   Restores the rollup on the next tick with **no TF restart at all** — the only option that
   avoids that risk. Leaves the TF bug live for any other cross-project caller.
3. **Fix TF properly.** Requires a reproduction first. Whoever owns the skip path can likely
   get there quickly with the notes above.

## Backfill

`ServiceMapRollup` is idempotent per bucket, so once the dispatcher works the missing
2026-08-09 → now range can be re-driven by enqueuing the buckets. The map only ever renders a
selected range, so recent buckets matter most.
