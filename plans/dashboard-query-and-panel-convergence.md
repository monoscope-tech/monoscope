# Dashboard query and panel convergence

Requested scope: six issues covering table sorting, details panels, trace widgets,
endpoint query cost, log query parameters, and CI coverage.

## Findings and decisions

1. Table headers sort rendered text after the server limit. Replace this with a
   widget refetch. Only declared sortable fields can become SQL identifiers.
   Handwritten SQL must expose an explicit order placeholder before LIMIT, with
   a default order. Preserve numeric ordering even where results encode as text.
   Validate a row outside the original top 20 enters after sorting.
2. Issues duplicates the details panel lifecycle. Extend the shared panel with
   an inline layout choice, retain the Explorer resizer, and use one-third width
   for Issues. Verify collapsed, open, close, drawer, and mobile states.
3. Overview's traces query is actually an operation aggregate: count and mean
   duration plus an arbitrary representative trace. Preserve its aggregates in
   a standard table and expose recent traces through the shared logs widget.
   Delete the obsolete type and renderer, including tests and stale references.
4. Measure endpoint SQL at 24 hours, 3 days, and 7 days before restructuring.
   Constants serialize first-column lists into browser requests, and time out
   to empty sentinels. Do not materialize unbounded trace IDs or discard the
   endpoint timestamp/span relationship. Choose an optimization from evidence.
5. Introduce LogDataQuery and RecordParam. Preserve all public parameter names,
   including target-spans, and typed cursor/direction values. Update callers by
   field name and test decoding.
6. Reproduce and fix DashboardWidgetsSpec compilation. Execute the new doctests.
   Audit guards for extraction gaps, parser suppression, and CI fingerprint
   inputs. Add regression fixtures for any discovered gap.

## Execution and validation

Implement the record and panel refactors, table sorting and trace convergence,
then measured endpoint changes and CI fixes. Use the requested advisory agent
for decisions. Run focused frontend checks and the existing Haskell watcher;
run doctests and appropriate integration checks. Finally use make ci-signoff
with relevant checks. Record actual commands, results, measurements, and any
outstanding checks here; never attest a failed or unavailable check.

## Results

Research and implementation are complete. The proposed endpoint-ID constant was
rejected after measurement because it truncates and changes the data contract;
the safe server-side cache/rollup follow-up is recorded below.

## Implemented behavior

- Table headers request a widget-scoped `table-sort` and keep it on refresh.
  The server accepts declared columns and applies ordering before the limit.
  Handwritten SQL uses `ORDER BY {{table_sort}}` with `default_sort` in YAML.
  Declare `sortable: true` on each supported output column. Numeric outputs
  must remain numeric SQL values; the table decoder accepts those values.
  KQL tables sort declared fields unless the column sets `sortable: false`.
- Issues uses `DetailsPanelLayout` in the shared details component. Its inline
  panel starts collapsed, opens at one-third width on large screens, and fills
  smaller screens. Explorer and dashboard modes retain their existing layouts.
- Overview now uses a logs widget for individual traces and a regular table for
  operation counts and mean duration. Migration 0184 preserves saved aggregate
  widgets when retiring the old type.
- LogDataQuery replaces the positional handler arguments. The route maps
  `targetSpans` to the existing `target-spans` parameter.
- The hyperscript guard parses the complete browser grammar, including a leading
  JavaScript feature and later event handlers. Regression fixtures cover the
  supported literal forms. CI fingerprints now include the vendored parser and
  Makefile. The doctest Makefile target disables the separate dev-test target.
- Duration table cells now convert their declared source unit before formatting;
  millisecond SQL values no longer render as nanoseconds. The retired trace-widget
  migration supplies usable columns when a saved widget had none, or only the
  removed latency column.
- Endpoint Analytics uses supported single-stage KQL for browser request/error
  series and per-bucket distinct sessions. The previous inline expression group
  and chained summaries parsed but silently discarded part of their query.

## Endpoint measurements and advisor decision

The current template has four raw-SQL widgets on Overview and one on Latency.
Only the active tab runs. The latter repeats Overview's downstream series.
These are per-query measurements, not browser tab-load measurements.

A recent endpoint sample from project `87576849-4941-49d3-a15d-680fef88a1a8`,
hash `f14abbef`, was measured sequentially against configured TimeFusion. Each
query used the same fixed end time, a 15-second server timeout, and a 20-second
client deadline. The baseline came from the original endpoint template.

| Widget | 24 hours | 3 days | 7 days |
| --- | ---: | --- | --- |
| Apdex | 9.739 s | client deadline | client deadline |
| Downstream operations, Overview | 16.744 s | client deadline | client deadline |
| Status code breakdown | 6.367 s | client deadline | client deadline |
| Downstream dependencies | 10.462 s | client deadline | client deadline |
| Downstream operations, Latency | 9.975 s | client deadline | client deadline |

The sample returned no downstream rows. These measurements establish query
cost; they do not establish downstream result equivalence. Connection overhead
is included. The server timeout did not always end work before the client deadline.

**Deferred: endpoint ID constants.** The advisor found that SQL constants cap
results at 1,000 rows and keep only the first column. They also travel through
browser request parameters. Using them for endpoint span IDs would truncate data
and lose the timestamp/span/trace relationship used by downstream joins.
Raw SQL currently bypasses the KQL timeseries cache. The safe follow-up is a
complete-result SQL cache with request coalescing, or server-side endpoint
rollups, with equivalence tests and repeated benchmarks before adoption.
No claim is made that this change fixes long-range endpoint query performance.

The table shell does now avoid fetching a second time when HTML was prefetched.
This saves a duplicate request for eager tables; the default endpoint tables
are lazy, so it does not solve their scan cost.

## Validation record

- Haskell watcher: compiled all 138 modules after the main refactors.
- Dashboard integration suite: 37 examples, 0 failures, against local services.
  Includes the regression where sorting returns rows outside the original 20.
- Frontend: 902 tests passed in 55 files; two file-loading failures resulted from
  launching from the wrong directory. Those two suites passed from
  `web-components`: another 41 tests. Total: 943 passing tests.
- `make ci-selftest`: passed, including the vendored-parser fingerprint test.
- UI detector: no findings in changed UI source.
- Final `make test-doctests`: 1,614 examples, 0 errors, 0 failures.
- Unit tests: 314 examples, 0 failures. This includes duration-unit rendering
  and the Endpoint Analytics KQL forms. The run first exposed two invalid query
  expectations; the template and assertions were corrected before the passing run.
- Focused frontend validation: hyperscript parsing, table navigation/sort, and
  detail-panel row selection — 17 tests, all passing.
- Migration 0184 executed in a rolled-back PostgreSQL transaction. Trace widgets
  with missing columns, latency-only columns, and nested placement all converted.
- `make ci-selftest`: all checks passed, including parser fingerprints and refusal
  to attest a check whose inputs changed while it ran.
- `make ci-signoff CHECKS='frontend doctests unit-tests ui-tests hlint'`:
  frontend, doctests, unit tests, and UI tests passed and were attested. The
  container UI run passed 946 tests in 57 files. HLint was unavailable in the
  local runner and was not attested.
- GitHub still needs to run build, CLI tests, integration tests, Weeder, and
  HLint. TimeFusion's amd64 image cannot run on this arm64 laptop; the focused
  dashboard integration suite had already passed against the available local
  services. No failed or unavailable check was attested.

Concurrent work committed part of this implementation while execution continued.
Other changes in the workspace were retained. Two subsequent test compilation
errors from that work required a record-dot access and an unused fixture argument.

## Follow-up validation (2026-09-18)

- Added complete-result caching and in-process singleflight for endpoint-scoped
  raw SQL. Live ranges execute against the same rollup-aligned bounds used in
  cache identity; explicit ranges remain exact. Failed queries are not stored.
- Captured fixed-window TimeFusion physical plans for Apdex, status breakdown,
  and downstream operations at 24 hours and 3 days. TimeFusion rejects
  `EXPLAIN (ANALYZE, BUFFERS)`, so actual row/buffer counters are unavailable;
  the evidence and optimization decision are in
  `docs/endpoint-analytics-query-plans.md`.
- Unit suite: 328 examples, 0 failures. This includes all built-in sortable SQL
  contracts and concurrent raw-query coalescing.
- Route decoding: 2 examples, 0 failures, including `target-spans` and malformed
  cursor/direction rejection.
- Details-panel component regressions: 14 examples, 0 failures under Node 22.
- Migration 0184 ran against a clean PostgreSQL 16 instance. Nested trace
  widgets converted recursively, the retired latency column was removed,
  missing columns received defaults, and customized columns/titles survived.
- `make ci-signoff CHECKS="build doctests unit-tests ui-tests weeder hlint"`
  passed and published build, doctest (1,617 examples), unit (328 examples), and
  UI (947 examples) attestations. Weeder reached the repository's existing
  dead-code inventory and failed; HLint did not run after that failure.
- The host HLint is too old to parse `MultilineStrings`, while the local CI
  runner does not provide the `hlint` capability. HLint remains outstanding.
- A full container integration sign-off compiled but was killed with exit 137
  by the local Docker memory limit, both with normal and single-job builds. No
  integration attestation was published. Against the same real PostgreSQL 16,
  Timescale Toolkit, MinIO, and native TimeFusion services, the seven changed
  integration areas passed as separate processes: 10 examples, 0 failures.
  These cover route decoding, migration 0184 snapshots, raw-result cache reuse
  and failure isolation, every built-in sortable column in both directions,
  and exactly one details container on Issues, Log Explorer, and dashboards.
- Separate frontend and end-to-end sign-offs passed and published attestations:
  the production Tailwind/Vite build completed, and Playwright passed 72 browser
  tests with 5 fixture-dependent skips. `make ci-status` leaves integration,
  Weeder, and HLint for GitHub. Frontend, build, doctests, unit tests, CLI tests,
  UI tests, and end-to-end tests have reusable attestations. The skipped issue
  investigation fixture means the full visual/resizing browser matrix, along
  with production cache-latency targets, remains post-deployment acceptance
  work.
