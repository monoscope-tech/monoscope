# Cached chart computation chunks

The user reported no visible chart progress after the transport streaming rollout.
A simplified All traces SQL probe for project 28f62f01 over seven days produced no rows before a ten-second diagnostic timeout.
The authenticated real KQL endpoint started streaming at 0.425 seconds during the diagnostic setting experiment but remained incomplete at 15 seconds.
Further testing found that SET leaked through the shared SessionContext across connections. After restoring repartition_aggregations=true, a fresh connection planned 24 partitions and the real endpoint again produced no frames before 15 seconds. The intermediate 0.425-second measurement is not a baseline under normal settings.
A diagnostic connection without aggregate redistribution produced its first row in 0.19 seconds, but this is not safe to ship alone.
A historical scan advertised descending timestamps and returned out-of-order rows. An ordered aggregate then returned the same bucket/status twice, with 39 rows each. The default partitioned aggregate returned one row with 78. Monoscope's pivot keeps the first matching series value, so consuming the split groups would undercount.

## Implementation

- Keep `time_bucket` ordering support for sources that honor their ordering contract.
- In Timefusion, prevent aggregates from closing groups based solely on merge-on-read storage ordering. A real sort can still establish valid ordering. Keep scan and LIMIT plans unchanged.
- Share Monoscope's cache policy between streaming and JSON endpoints. Persist only successful complete results. Refresh changed boundary buckets and replace rows that became empty.
- Align overlap reads to bucket boundaries. Preserve the first partial bucket when reading cached data. Keep count totals and rates consistent after merging and trimming.
- On eligible streaming cache misses longer than one day, execute recent time buckets first. Grow the chunk width to at most a day. Use exclusive internal upper boundaries and the original inclusive final endpoint. Fix `bin_auto` to the original interval before splitting.
- Only split a terminal timestamp-binned summarize with ordinary filters/source selection. Global sorting, limits, computed time keys and raw SQL keep their existing execution semantics.
- Merge completed chunk results and send cumulative chart frames. Use the cancellable result reader for each chunk. Cache the full result after all chunks succeed.

## Release checks

- Test historical out-of-order input within and across Arrow batches against full aggregate counts.
- Retain the honest ordered-source early-output tests for `time_bucket`.
- Compare chunked results with the complete query, including events exactly on boundaries.
- Prove a streaming cache hit works after telemetry is removed from the test database.
- Exercise existing cache, parser, streaming error and cancellation tests.
- Run both repository release gates, deploy through their workflows, and measure real seven-day chart frames and repeated cached requests.

The diagnostic optimizer setting initially leaked across connections because the server shared a mutable SessionContext. The original repartition_aggregations=true was restored and verified through a fresh connection plan. The protocol fix creates one independent SessionContext per connection and shares it between that connection's simple and extended handlers. Catalogs, runtime resources and the plan cache remain shared. No default partition setting changes.

Local checks passed: 9 cache integration tests (including the three previously pending failure checks), 25 widget integration tests, 14 cache unit tests, 127 parser tests, and 21 browser tests. Timefusion's aggregate guard passed 1,394 main tests and 62 end-to-end tests. The connection isolation extension is checked separately before its final release gate.

## Production follow-up

The first rollout deployed Monoscope `8da575b35` and Timefusion `c49743be`.
A browser check of the seven-day All traces chart improved from 17.1 seconds to 2.9 seconds with complete cached data. Timefusion's live aggregate matched 1,219,085 captured raw rows across 1,199 groups. Separate connections retained independent query settings.

The user's fourteen-day `status_code == "pickup_accepted"` query exposed further issues. The first completed chunk arrived in 2.7 seconds but had no matching points. Larger historical chunks took tens of seconds, and the stream disconnected before completion. The browser's automatic refresh could also abort the active stream and replace it with a non-streaming request.

The follow-up keeps background refreshes from interrupting active requests, retains a visible search message for empty partial data, and repeats the latest stream frame every five seconds while a query is pending. This keeps the HTTP response active and lets a disconnected reader cancel the scoped SQL producer promptly. A heartbeat does not claim additional completed work. Successful empty results can use the cache; cached errors still require a new query.

All traces also needs a group dimension that the existing rollup does not carry: `level` in `coalesce(status_code, level)`. A retained equality or `IS NOT NULL` filter on `status_code` proves that this fallback is unreachable. Timefusion can simplify that filtered grouping while keeping the original output aliases and dimension filters. Nullable and OR cases continue to require the fallback and decline the unsupported rollup route. The unfiltered chart retains its original grouping semantics.
