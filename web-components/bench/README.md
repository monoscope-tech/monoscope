# Query editor rewrite: benchmarks and verification

The rewrite removes synchronous schema scans from typing and focus. A worker builds a path index, resolves fields and values, and returns a bounded suggestion list. CodeMirror owns editing, highlighting, history, diagnostics, and the completion popup. The shared element keeps the existing query actions and event payloads.

The old resolver enumerated every field again for each candidate. A cache helped subsequent requests, but the first lookup could freeze the main thread. The visual builder also wrapped a global schema method on every mount and retained listeners and callbacks. Both paths have been replaced.

## Reproduce

Use Node 25.2.1 (the runtime used for these results), install the root and web-components dependencies, and install Playwright browsers. Run these commands from `web-components`:

```sh
npm run bench:query:schema
npm run bench:query:build
npm run bench:query:serve
```

Keep the preview server running. In a second terminal:

```sh
npm run bench:query:browser
npm run test:query:browser
```

The browser regression command builds and starts its own preview server when port 3099 is free. If a preview is already running, rebuild the harness after source changes before testing it.

The baseline is read from commit `d1344345b9f38dd6e948afd1b8f719be6f02cfa2`. A shallow checkout must fetch that commit before building the comparison harness. Production builds do not reference it.

Set `QUERY_BENCH_TRACE=1` when running the browser benchmark to capture Chrome CPU profiles in `bench/traces/`. Profiling adds overhead; use an unprofiled run for performance comparisons. Cold-focus profiles from this session are saved locally as `traces/baseline-cold.cpuprofile` and `traces/codemirror-cold.cpuprofile`.

The browser command fails if the shipping editor exceeds these budgets: typing p95 above 50 ms normally or 100 ms at 4× slowdown, a typing task above 50 ms, warm completion p95 above 100 ms, a page error, or retained editor instances after teardown. It also checks active subscriptions and pending requests.

## Schema results

These are uncached resolver measurements from the actual old source, compared with index construction plus the first indexed lookup. Timings are milliseconds. The old resolver runs in an isolated Node worker with a five-second watchdog so the benchmark itself cannot freeze indefinitely.

| Shape | Fields | Old resolver | Build index | First lookup |
|---|---:|---:|---:|---:|
| Flat | 1,000 | 12.97 | 0.55 | 0.05 |
| Flat | 5,000 | 2,152.02 | 2.72 | 0.13 |
| Nested | 5,000 | 699.54 | 3.53 | 0.16 |
| Deep | 5,000 | 1,395.47 | 4.63 | 0.12 |
| Flat | 100,000 | Not run | 117.27 | 3.21 |
| Nested | 100,000 | Not run | 169.08 | 2.85 |
| Deep | 100,000 | Not run | 344.45 | 2.68 |

The 100,000-field indexing work runs in the browser worker, not in the input handler. The benchmark covers 100, 1,000, 5,000, 10,000, and 100,000 fields. Full measurements: [schema-results.json](schema-results.json).

## Browser comparison

The production-built harness compares four cases: a textarea reference, the original component, the original Monaco component with the new completion backend, and the shipping CodeMirror component. The Monaco comparison keeps its original suggestion UI and uses the same worker backend as CodeMirror.

Typing measurements cover a 40-character query at 50 ms between keys. Latency runs from keydown to two animation frames; this is a repeatable rendering proxy, not a field INP measurement. Initialization includes the engine's dynamic import and mounting. Cold completion includes popup scheduling for CodeMirror and the original completion refresh for Monaco, so those cold values are not identical UI milestones. Heap numbers describe the main page, excluding worker heaps.

See [browser-results.json](browser-results.json) for initialization, completion, typing p95, layout counts, heap sizes, errors, and the resulting text. The report below summarizes the latest unprofiled run with the committed dependency lockfile. The production query-editor chunk is 114.91 KB compressed; Monaco is isolated in the YAML chunk.

| Editor | CPU | Initialize (ms) | Warm completion p95 (ms) | Typing p95 (ms) | Main heap (MB) |
|---|---:|---:|---:|---:|---:|
| Textarea reference | 1× | 0.4 | 0.0 | 7.8 | 1.99 |
| Textarea reference | 4× | 0.4 | 0.0 | 9.1 | 1.99 |
| Original Monaco | 1× | 355.1 | 3.0 | 8.2 | 19.01 |
| Original Monaco | 4× | 877.1 | 12.3 | 27.2 | 18.21 |
| Monaco + new backend | 1× | 301.7 | 0.3 | 10.6 | 17.67 |
| Monaco + new backend | 4× | 963.7 | 34.5 | 20.9 | 17.69 |
| CodeMirror rewrite | 1× | 43.0 | 0.3 | 10.8 | 5.56 |
| CodeMirror rewrite | 4× | 118.1 | 1.0 | 15.0 | 5.55 |

Small, warm schemas did not consistently make the old editor slow. The severe defect was cold schema resolution and accumulated lifecycle work. CodeMirror was selected for its much smaller query-only bundle, lower initialization cost and main-page heap use, and passing interaction budgets. It is not faster than every Monaco case on every metric.

## Lifecycle and integration

With 100,000 flat, nested, or deep fields and 4× page CPU slowdown, 100 additional mount/unmount cycles retained exactly one editor: the mounted instance. DOM node counts stayed at 49 and listener counts at 51. Each case ended with one client, one subscription, and zero pending requests. See [stress-results.json](stress-results.json).

Verification completed:

- 154 query, completion, schema, validation, builder, and public-call tests.
- 35 YAML import/export, live-tail, and inline-handler tests.
- 21 browser scenarios across Chromium, Firefox, and WebKit: click-to-open and reopen, custom dropdown presentation, keyboard selection, undo, multiline input, large schemas and queries, isolation, reconnects, schema retry/deduplication, event routing, loading-input adoption, theme changes, and active-option announcements.
- Seven application end-to-end tests against the isolated local database: five query-editor tests, dashboard visualization synchronization, and applying generated KQL in live tail.
- TypeScript typecheck, production Vite build, full `monoscope-server` development build, Haskell template formatting, and `git diff --check`.

The restored dropdown keeps the guidance, type badges, field/operator groups, and keyboard footer. It caps width at 640 px and uses 29 px rows. Type labels measured at least 7.09:1 contrast in light mode and 9.92:1 in dark mode; APCA label contrast exceeded Lc 60 in both themes. CodeMirror renders at most 35 options and owns selection and ARIA; the worker still bounds schema suggestions to 20.

The visual builder shows the first 100 matching fields; its search still covers the full schema. Validation uses the existing server endpoint, with the original 400 ms validation and 500 ms query-update delays. Native input remains usable during module loading, and its text, selection, and focus transfer during upgrade. Monaco remains available only for YAML editing.

These synthetic localhost measurements ran on a busy Apple Silicon Mac. They are not customer traces or physical low-end-device measurements. Application tests use local demo fixtures, not customer sessions. Manual screen-reader testing and a native OS IME session remain unverified. Deployment was not performed.
