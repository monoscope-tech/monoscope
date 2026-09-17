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

Research complete; implementation and measurements in progress.

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
- Initial `make test-doctests`: ran all 1,611 examples; one new example produced
  an ambiguity warning. The example was annotated and a final run is pending.
- Unit tests found an exact-trigger assertion that excluded the new sort event.
  The assertion now permits extra events and checks that eager tables do not
  refetch on intersection. Final rerun pending.
- Local CI signoff built and attested frontend, then stopped on a compile error
  in concurrently edited OpenAPI code. That error was fixed in the workspace;
  final signoff is in progress. No failed check was attested.

Concurrent work committed part of this implementation while execution continued.
Other changes in the workspace were retained. Two subsequent test compilation
errors from that work required a record-dot access and an unused fixture argument.
