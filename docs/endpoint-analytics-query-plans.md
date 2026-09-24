# Endpoint Analytics query-plan evidence

Captured on 2026-09-18 against the configured TimeFusion store for project
`87576849-4941-49d3-a15d-680fef88a1a8`, endpoint hash `f14abbef`, with a fixed
end time of `2026-09-18T00:00:00Z`. The two windows were 24 hours and 3 days.

## Planner findings

| Query | 24-hour physical plan | 3-day physical plan | Decision |
| --- | --- | --- | --- |
| Apdex | One project/date-pruned scan; 8 Parquet file groups; partial then final aggregate | Three date partitions; 6 + 8 file groups; partial then final aggregate | Complete-result cache. The endpoint hash remains a post-scan array filter. |
| Status breakdown | Same pruned scan; partial/final grouped aggregate; `TopK(20)` | Three date partitions; grouped aggregate; `TopK(20)` | Complete-result cache. A hash index is not available in this store. |
| Downstream operations | Two scans of the same project/date partition, then a partitioned hash join on `(trace_id, parent_id/span_id)`, aggregate, and `TopK(20)` | Both join legs expand to three date partitions | Use the five-minute `apis.endpoint_dependency_edges` rollup for tables and dependency trends. Keep the remaining raw join behind the complete-result cache. |

`project_id` is a full scan filter and storage partition. The timestamp bounds
prune date partitions. `hashes @> ARRAY[...]` appears under `partial_filters` and
in a `FilterExec`, after file selection; it is not an index lookup. TimeFusion's
SQL endpoint does not expose PostgreSQL indexes, so there is no project/time/hash
B-tree or GIN index to select. Adding a PostgreSQL index would not change these
TimeFusion plans.

TimeFusion accepts `EXPLAIN SELECT ...` but rejects
`EXPLAIN (ANALYZE, BUFFERS) ...` at the parser. Consequently it does not report
actual rows, join cardinality, buffer hits, disk reads, or per-operator aggregate
time through pgwire. Those fields are unavailable, not zero. Engine-side scan
telemetry is the authoritative source for bytes read. The earlier sequential
measurements in `plans/dashboard-query-and-panel-convergence.md` remain the
runtime evidence: at 24 hours Apdex took 9.739 seconds, status breakdown 6.367
seconds, and downstream operations 16.744 seconds; the 3-day requests exceeded
the 20-second client deadline.

## Cache contract and performance target

Endpoint-scoped raw SQL results are cached only when `var-endpointHash` is
present. The identity includes project, endpoint hash, canonical time range,
environment, resolved canonical SQL/KQL query identity, rollup interval, backend, and
decoder. Live ranges are canonicalized to the rollup boundary, bounding
staleness to one interval. Explicit `from`/`to` ranges remain exact. Errors are
never stored. Concurrent misses for the same identity share one in-process
flight; completed results are stored in PostgreSQL's `query_cache`, so all
replicas can reuse them.

The deployment acceptance target is:

- cached 24-hour endpoint widgets settle in under 2 seconds;
- cached 7-day endpoint widgets settle in under 10 seconds;
- one cache miss executes per key per process during a concurrent burst;
- no endpoint span-ID or trace-ID dashboard constant is introduced (constants
  truncate at 1,000 first-column values and cannot preserve span relationships).

The targets require post-deployment browser and engine telemetry validation;
local tests prove identity isolation, request coalescing, and result reuse, not
production network latency.
