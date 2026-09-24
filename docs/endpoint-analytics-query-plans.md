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

The JSON and streaming routes share one cache for endpoint-scoped raw SQL.
Requests without `var-endpointHash` do not use this cache.
The key includes project, endpoint, time bounds, environment, service, SQL, KQL,
rollup interval, backend, and decoder.

Live windows align to a 15-second boundary, including the default live window.
Explicit `from`/`to` windows keep their exact bounds.
Each result expires 60 seconds after the query snapshot, even for fixed windows.
Cache reads do not extend this lifetime. Late telemetry can appear after expiry.
The live boundary excludes less than 15 seconds of recent data.
Expiry bounds reuse of the captured result, not upstream ingestion delay.

Only successful complete results enter the cache. Results larger than 1 MiB do
not enter PostgreSQL. Concurrent callers can still share these results.
Cache read or write failures increment an error counter. Requests still run the
backend query or return its successful result.

One process shares cache lookup, query execution, and persistence as one operation.
The process releases the operation only after persistence finishes.
Failures wake all waiting callers. A later request can try again.
Other replicas reuse PostgreSQL results, but simultaneous cold requests across
replicas can still execute separately.
The existing cleanup job removes raw-cache entries older than ten minutes when
it next runs. Expiry checks apply on every lookup, independently of cleanup.

The deployment acceptance target is:

- cached 24-hour endpoint widgets settle in under 2 seconds;
- cached 7-day endpoint widgets settle in under 10 seconds;
- one cache miss executes per key per process during a concurrent burst;
- no endpoint span-ID or trace-ID dashboard constant is introduced (constants
  truncate at 1,000 first-column values and cannot preserve span relationships).

The targets require post-deployment browser and engine telemetry validation;
local tests prove identity isolation, request coalescing, and result reuse, not
production network latency.


## Measurements

All metric labels use bounded values. Project IDs, endpoint hashes, and SQL text
are not metric labels.

| Metric | Labels | Meaning |
| --- | --- | --- |
| `monoscope.dashboard.query.cache_outcomes` | `backend`, `outcome` | Raw request outcomes: `raw-hit`, `raw-miss`, `raw-coalesced`, `raw-error` |
| `monoscope.dashboard.endpoint_cache.duration` | `backend`, `outcome` | Cache operation duration in milliseconds, including lookup, wait, query, and persistence |
| `monoscope.dashboard.endpoint_cache.result_size` | `backend`, `outcome` | Serialized cache payload size in bytes |
| `monoscope.dashboard.endpoint_cache.backend_queries` | `backend` | Backend query attempts, including failures |
| `monoscope.dashboard.endpoint_cache.avoided_queries` | `backend` | Successful cache hits and shared results that avoid another backend query |
| `monoscope.dashboard.endpoint_cache.errors` | `backend`, `operation` | Cache failures during `lookup` or `store` |
| `monoscope.dashboard.endpoint_cache.oversized` | none | Complete results excluded from PostgreSQL because they exceed 1 MiB |

For one measurement window, use these formulas:

- Request count: sum the four `raw-*` outcome counts.
- Durable hit rate: `raw-hit / request count`.
- Reuse rate: `(raw-hit + raw-coalesced) / request count`.
- Backend queries avoided: increase in `endpoint_cache.avoided_queries`.
- Cold latency: duration distribution for `raw-miss`.
- Cached latency: duration distribution for `raw-hit`.
- Shared-request wait: duration distribution for `raw-coalesced`.
- Result size: p50, p95, and maximum of `endpoint_cache.result_size`.

Duration metrics exclude HTTP transport and browser rendering.

Backend attempts can finish after the measurement window starts or ends.
Compare counters over a stable window rather than expecting exact short-window
subtraction. Keep error rates visible alongside cache hit rates.

The integration benchmark uses 1,000 text rows and a 100-millisecond backend
wait. A PostgreSQL sequence counts actual executions. For each window, it sends
one cold streaming request and ten requests alternating between JSON and
streaming. All responses must contain the same rows, and the sequence must
remain at one. This proves reuse independently of timing noise.

Local PostgreSQL timings do not establish production TimeFusion latency.
The selected windows exercise cache identity; the synthetic query does not scan
24 hours, three days, or seven days of telemetry.

## Operational tradeoffs

Repeated requests avoid backend scans. Concurrent requests in one process also
share work before a durable result exists. PostgreSQL makes completed results
available to other replicas without another cache service.

A cold request still runs the full backend query and adds a cache lookup and
write. Hits add a PostgreSQL read, JSON decoding, and serialization. Each stored
result adds database storage and write-ahead log traffic. The per-result limit
is not a global storage budget; the hourly cleanup job removes expired rows.

The cache cannot make late telemetry immediately visible. The 60-second expiry
limits reuse, and live alignment can omit the latest 15 seconds. Exact windows
retain their requested bounds. Use hit rate, error rate, result size, and latency
together when assessing whether the saved backend work justifies these costs.


## Local measurements (2026-09-24)

The full local integration run produced these results. Each row has one cold
request and ten cached requests. Timings include handler work and stream
consumption, but exclude HTTP transport and browser rendering.

| Requested window | Result bytes | Cold (ms) | Cached median (ms) | Cached range (ms) | Hit rate | Backend queries avoided |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 24H | 95,073 | 125.32 | 5.87 | 3.53–6.89 | 90.9% | 10 of 11 |
| 3D | 95,073 | 122.46 | 2.91 | 2.17–5.72 | 90.9% | 10 of 11 |
| 7D | 95,073 | 130.66 | 3.19 | 2.10–4.34 | 90.9% | 10 of 11 |

Total: 33 requests, three backend executions, and 30 avoided executions.
The sequence assertion verifies the query count; timing is not a pass condition.
These results demonstrate cache reuse. They do not establish production hit
rates or production TimeFusion latency. The configured CLI account could read
project identity, but its telemetry facet request returned HTTP 500 before
deployment, so no production performance baseline was established through it.

The browser regression test records the click and visible tab state in the
browser's performance timeline. Three isolated repeats measured 163.8, 115.6,
and 119.4 milliseconds against the unchanged 1,500-millisecond limit.
The previous test measured Playwright scheduling and assertion polling too.
Its intermittent failures did not reflect the browser transition alone.
The scroll regression now waits for its initial visible row before checking
that subsequent page loads preserve that row. All six repeated browser cases
passed after these test fixes.
