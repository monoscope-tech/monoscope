# Streaming charts across Timefusion and Monoscope

## Scope

Implement locally. Do not deploy Timefusion or Monoscope.
Preserve complete JSON responses for API, CLI, export, and existing dashboard callers.
Add progressive delivery for browser charts. Do not split arbitrary SQL into independent queries.

## Timefusion

1. Keep the existing `time_bucket` name, text and interval inputs, epoch origin, and microsecond output.
   An alias to `date_bin` changes accepted arguments and output types.
   Implement its constant-width ordering contract using the same rule as `date_bin`.
   Reject zero, negative, and overflowing widths. Use floor division before the epoch.
2. Exercise sorted aggregation with an ordered source and a delayed input stream.
   Prove that the first completed group arrives before input completion.
   Preserve output ordering through aggregation where possible. Do not change global execution settings.
3. Stream extended protocol responses directly from the portal.
   Bound finite fetches and preserve suspension, completion, schema, cancellation, and connection reuse.
   Flush available rows when the input pauses. Do not wait for a full socket buffer.
   Preserve the simple protocol and SQL cursor paths.

## Monoscope

1. Add a scoped libpq single-row consumer using existing Haskell bindings and row decoders.
   Keep one pooled connection for the query. Cancel and discard failed or abandoned connections.
2. Add an authenticated NDJSON chart endpoint with partial snapshots and a terminal complete or error frame.
   Reuse query compilation, backend selection, decoding, and timestamp conversion.
   Use a bounded queue for backpressure. Cancel the producer when the response closes.
   Keep complete-result caching on the existing endpoint. Never cache partial results.
3. Read stream frames incrementally in widgets. Preserve stale-request protection and abort handling.
   Render available points with an explicit loading state. Finalize statistics only after completion.
   Keep prior successful data during background refreshes. Treat premature EOF as failure.
   Hold the browser concurrency slot until the response body finishes.

## Validation

- Compare time buckets with `date_bin` for positive and negative timestamps, nulls, and supported widths.
- Prove early aggregate delivery with deterministic input gates.
- Prove early pgwire delivery, bounded fetch resume, terminal errors, cancellation, and repeated cursor exhaustion.
- Compare streamed final charts with ordinary results. Exercise disconnects and malformed requests.
- Test browser frame boundaries, Unicode, partial updates, completion, errors, aborts, and concurrency.
- Run focused Rust, Haskell, and TypeScript tests, formatting, type checks, and local integration probes.
- Record commands and results here. No production benchmark or deployment is required for implementation.


## Prior art and execution limits

[DataFusion ordering analysis](https://datafusion.apache.org/blog/2025/03/11/ordering-analysis/)
explains why ordered aggregation can emit completed groups before the input ends.
Constant-width `date_bin` preserves timestamp ordering. The Timefusion UDF now declares the same property.
Unordered aggregation, a blocking sort, or a different physical plan can still delay the first row.
This change does not guarantee early output for every 30-day or 60-day query.

[PostgreSQL result modes](https://www.postgresql.org/docs/17/libpq-single-row-mode.html)
separate client buffering from server execution. Monoscope uses the existing single-row binding,
then sends chart snapshots at most ten times per second. No libpq 17 requirement or new FFI is needed.
The result reader drains through the final null result. Failed or abandoned streams cancel and discard their connection.

The browser receives cumulative snapshots. Monoscope retains the chart result for the final pivot and statistics.
The transport queue holds at most two frames. This bounds queued delivery, but not the total chart dataset.
Scalar aggregates still require completion. Text and JSON widgets keep their existing complete-result decoders.

## Local results

- Browser parser, partial rendering, refresh, count, stale-response, and concurrency tests: 21 passed.
- TypeScript type check passed. The interface detector returned no findings.
- Timefusion ordered aggregation, date-bin equivalence, finite fetch, TCP streaming, and cancellation tests passed.
- Late-error and exhausted-cursor cases passed over a local TCP connection.
- Monoscope library and integration test binary built successfully.
- All 25 dashboard-widget integration examples passed against local PostgreSQL.
  These include final-result equivalence, decoder error frames, cancellation before input completion, and pool reuse.
- No deployment was performed.


Commands used:

```sh
# Timefusion
cargo test --lib streaming_tests --no-default-features
cargo test --lib server:: --no-default-features
cargo test --lib interval_to_micros --no-default-features
cargo fmt --check

# Monoscope
cabal build monoscope:lib:monoscope monoscope:test:integration-tests -j4
USE_EXTERNAL_DB=true DB_HOST=127.0.0.1 \
  dist-newstyle/build/aarch64-osx/ghc-9.12.2/monoscope-0.1.0.0/build/integration-tests/integration-tests \
  --match Pages.DashboardWidgets +RTS -N2 -RTS
npm --prefix web-components test -- chart-stream widgets-auto-refresh widgets-count chart-fetch-seq
npm --prefix web-components run typecheck
```

The Rust checks cover 46 server tests, two ordering/equivalence tests, and the existing interval parser test.
The ordered aggregate fixture uses one execution partition and declared input ordering.
No global partition setting changed. Representative production plans still need inspection before rollout.
The tests establish delivery and correctness; they do not establish a 30-day or 60-day production latency improvement.
