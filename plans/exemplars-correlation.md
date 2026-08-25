# Metric ↔ trace ↔ log correlation (exemplars)

Stream: `ws-exemplars`. Goal: opening a span or log shows its related metrics; a metric
chart point links to a representative trace.

## 1. What we already have (verified, not assumed)

- **OTLP ingestion already parses exemplars.** `src/Opentelemetry/OtlpServer.hs:1440`
  (`exemplarsToJSON`) extracts `trace_id`, `span_id`, `timestamp`, `value`,
  `filtered_attributes` for gauge/sum, histogram and exponential-histogram points, and
  writes them to `MetricRecord.exemplars :: AE.Value`
  (`src/Models/Telemetry/Telemetry.hs:418`, bulk-inserted at :2844).
- **The column exists in both stores.** `static/migrations/0002` and `0108_otel_metrics.sql`
  both declare `exemplars JSONB`. TimeFusion's `otel_metrics` mirrors it.
  **No migration is needed for this work — `0138_` stays unclaimed.**
- **Exemplars survive into TimeFusion and carry real trace ids in production.**

  ```sql
  -- prod TF (timefusion.s.past3.tech), bounded + LIMIT per tf_ops_query_gotchas
  SELECT project_id, metric_name, resource___service___name, timestamp, exemplars::text
  FROM otel_metrics
  WHERE timestamp > now() - interval '20 minutes'
    AND exemplars::text LIKE '%9e7195d92a37d8bf2abc7a24b09b14e7%'
  LIMIT 10;
  ```

  returned 6 rows in ~1s, e.g.

  ```
  00000000-…-000000000000 | rpc.server.duration | product-catalog | 2026-08-25 11:39:58.385560+00 |
    [{"filtered_attributes":{},"span_id":"7ceddda7531e9d51",
      "timestamp":"2026-08-25T11:39:45.552358097Z",
      "trace_id":"9e7195d92a37d8bf2abc7a24b09b14e7","value":2.149803}, …]
  ```

  So the whole ingest→TF→query path is intact; the gap is entirely in the product.

  Two facts that fall out of that probe and constrain the design:

  1. **Most rows have `exemplars = []`.** Collector-scraped metrics (`container.*`,
     `postgresql.*`, `system.*`) have no trace context at all. Only SDK-instrumented
     metrics whose recording happened inside a sampled span carry exemplars — in the
     demo project that is the `rpc.server.*` and `http.server.*` families.
  2. **An exemplar's timestamp is not its row's timestamp.** Cumulative histograms keep
     one exemplar *per bucket*, and a bucket that has not been hit recently keeps a
     weeks-old exemplar. The `rpc.server.duration` row exported at 11:39:58 today carries
     exemplars from 2026-08-02. Every link must use the **exemplar's own timestamp**, and
     the UI must filter by it, or we deep-link into a trace that retention deleted and
     (worse) call `traceH` with a timestamp far from the trace's real one — the documented
     TF-OOM / 504 path.

- **`exemplars::text` works identically on TF's variant type and PG's `jsonb`.** That is the
  one SQL shape both backends share, so the correlation queries can go through the existing
  `Hasql.withHasqlTimefusion` / `enableTimefusionReads` selection like every other metric
  query — and integration tests keep working against local PG.

## 2. Vendor survey

### Datadog — the model we copy

Datadog has **no single universal correlation**. It has four join keys, applied at different
granularities, and the UI never makes the user choose between them:

1. **Unified service tagging** — `env`, `service`, `version`, injected by every SDK into every
   signal. The coarse join; it makes filter vocabulary identical across products.
2. **Infrastructure identity** — `host.name`, `container.id`.
3. **`runtime_id`** — the process that emitted the span, used to attach runtime metrics.
4. **`trace_id` / `span_id`** — the only exact join, and it is logs↔traces only.

On the **span detail panel** under the flame graph, the tab strip is the correlation surface:
`Span Info`, **`Infrastructure`**, **`Metrics`**, `Logs`, `Processes`, `Network`, `Security`,
`Profiles`/`Code Hotspots`, `Span Links`. Two of those are "related metrics", split by what the
metric is *about*: **Infrastructure** = the machine (host/container CPU, memory, I/O, keyed on
`host.name`/`container.id`); **Metrics** = the process runtime (heap, GC, threads, keyed on
`runtime_id`). Neither is a metric explorer — both are **pre-scoped charts**.

**The single most important UI detail, shared by both: the trace's own time interval is drawn
as an overlay on every chart.** Without it the chart is decoration; with it, it answers "was my
request inside the spike?". The Logs tab has the mirror affordance — hovering a log draws a
vertical line at its timestamp on the flame graph.

The **log side panel** has a `Trace` tab (exact `trace_id`, with a *View Trace Details* button)
and a `Metrics` tab showing infrastructure metrics in a **±30-minute window around the log** —
a concrete number worth copying, since a log has no duration of its own.

From a **metric chart**, "Context Links": clicking a data point opens a menu — *View related
traces / logs / profiles / hosts / containers / processes / RUM*. The link is built from three
ingredients merged: widget filters + dashboard template variables, the specific group clicked,
and the point's tags. **Time window rule: for timeseries and heatmap widgets the range is the
clicked bucket, not the dashboard range**; for every other widget type it is the full widget
range. Traces and logs open **in a side panel with samples first** — you stay on the chart.

Datadog is also explicit about the failure mode rather than hiding it: *"traces and logs are
sampled independently"*, so a `trace_id` may name a trace that was never retained, and the
panel renders a "trace unavailable" state instead of a broken link. The same applies to us:
trace metrics are computed on 100% of traffic while the traces behind them are sampled.

There is **no tab named "Related" or "Connections"** — the absence is itself the finding.
Datadog puts correlation *on the chart and on the span*, not behind a tab named after the idea.

### Grafana / Prometheus / OpenTelemetry

- Grafana renders exemplars as **diamond markers** layered on the time-series panel. The
  tooltip lists the exemplar's labels and value; next to `traceID` sits a
  **`Query with <datasource>`** button. **Clicking opens the trace in a split panel to the
  right — the metric graph stays on screen.** The same "don't navigate away" instinct as
  Datadog's side panel.
- Config is `exemplarTraceIdDestinations` on the Prometheus datasource (label name → tracing
  datasource). Exemplars come from a *separate* `/api/v1/query_exemplars` request, not from
  `query_range`, and are unavailable for instant queries.
- The reverse direction is Tempo's **"Trace to metrics"**: a query template where `$__tags`
  interpolates the clicked span's own attributes (`requests_total{$__tags}` →
  `requests_total{pod="nginx-554b9", cluster="us-east-1"}`), with **tags absent from the span
  silently omitted**, plus `spanStartTimeShift`/`spanEndTimeShift` (default ±2m) widening the
  window around the span. Exactly the tag+window tier, spelled as configuration.
- **The exemplar wire model, and why ours look the way they do.** OTLP `Exemplar` carries
  `trace_id`/`span_id` as raw bytes, `time_unix_nano`, `as_double`/`as_int`, and
  `filtered_attributes` — precisely what we already ingest. The SDK default filter is
  **`TraceBased`**: only measurements recorded inside a *sampled* span are eligible. The
  default reservoir for an explicit-bucket histogram is
  `AlignedHistogramBucketExemplarReservoir` — **at most one exemplar per bucket** — which is
  exactly why our production `rpc.server.duration` rows carry ~14 exemplars spanning weeks.
  Summaries have no exemplar field at all, and the OTel→Prometheus spec says exemplars on
  gauges and summaries SHOULD be dropped.
- Consequence to design for: exemplars are **"one kept per bucket", not "the worst one"**. A
  p99 spike from 500 slow requests yields one uniformly-weighted trace id per bucket. We
  therefore sort by value descending and label the list "representative", rather than implying
  the exemplar *is* the worst request.

### Honeycomb / New Relic

- **Honeycomb** is events-native like us: a span *is* an event, so metric→trace is not a join,
  it is reading a field off the row the aggregate was computed from. Its documented
  representative-item rule is worth copying verbatim: **"the slowest trace with a child span
  and with a value less than or equal to the value of your selected point"**, and the waterfall
  opens **pre-positioned on the first span matching the query's filter**, not at the root.
  Its trace sidebar carries a **minigraph** placing the selected span against its peers.
  Honeycomb's own OTel-metrics position is that metric→trace drill-down is *not* possible —
  its `Correlations > Metrics` panel runs a parallel metrics query over **the same time window
  and any compatible filters** and shows it side by side. Side-by-side, not drill-down.
- **New Relic** stamps `entity.guid`, `trace.id`, `span.id`, `hostname`, `entity.name` onto
  every log record (even over plain text, via an `NR-LINKING|…|` wire format). Its span detail
  pane leads with a **Performance** tab — average duration/throughput for that span's operation
  versus baseline — which is the metric hop. NRQL's `FROM Span, Log WHERE trace.id = …` is a
  **union filtered on a shared attribute, not a join**.

### Conclusion

Everyone converges on the same shape: **an exact id join where the data carries one, a
tag + time-window join otherwise, surfaced as a tab inside the thing you already have open,
with the subject's own time interval overlaid on the correlated chart.** Nobody makes the user
navigate to a correlation page.

Note the structural point this survey makes about us: Monoscope is in Honeycomb's camp — our
spans are rows, so span→metrics by service+window needs no new key. Exemplars matter for the
case a projection over spans cannot cover: **SDK-emitted metrics whose underlying measurements
were never rows** — which, per §1, is exactly the demo project's `rpc.server.*` family. We
build the exemplar path because production already carries the data; we do not build
Prometheus-style exemplar plumbing on top of it.

## 3. Where a correlation link belongs in our product

| Surface | Link | Verdict |
|---|---|---|
| **Span detail panel** (`LogItem.detailTabs`) | new "Metrics" tab: metrics whose exemplars name this trace, then metrics from the same service in the span's window | **build** — highest value, and the tab machinery already exists |
| **Log detail panel** (same function) | same tab; exemplar tier is usually empty (log records carry no trace context by design), service tier still works | **build** — free, same code path |
| **Metric detail page** (`metricsDetailsPage`) | new "Exemplars" tab beside the existing "Related metrics": representative traces for this metric, sorted by value | **build** — this is the metric→trace direction |
| **Metric chart** (`metricDetailChart` → `widgets.ts`) | exemplar diamonds overlaid on the series; click opens the trace | **build** — Grafana's affordance, on the one chart where `metric_name` is unambiguous |
| Trace waterfall | per-span metrics | **skip** — the waterfall already opens the span detail panel, which now has the tab. Duplicating it there adds a surface without adding a capability. |
| Dashboard widgets | exemplar diamonds on arbitrary widgets | **skip for now** — a widget's KQL can aggregate across metrics, group by anything, or not be a metric query at all, so there is no single `metric_name` to fetch exemplars for. Needs query-AST introspection; out of scope. |
| Service map | service → its metrics | **skip** — another stream owns the service map, and it is a navigation surface, not an investigation one. |
| Issue page | metrics around an issue | **skip** — an issue links to its trace, and the trace's span panel carries the tab. One hop, no new code. |

## 4. Design

### 4.1 Data access (`Models/Telemetry/Telemetry.hs`)

One exemplar type and one parser, shared by both directions:

```haskell
data MetricExemplar = MetricExemplar
  { metricName :: Text, serviceName :: Text, metricUnit :: Text
  , traceId :: Text, spanId :: Text, exemplarTime :: UTCTime, value :: Double }
```

Two bounded queries, both `withHasqlTimefusion`, both `timestamp BETWEEN` + `LIMIT`:

- `exemplarsForTrace pid traceId (lo, hi) limit` — `WHERE exemplars::text LIKE '%<trace>%'`,
  then parse the JSON in Haskell (aeson) and keep only the exemplars actually naming that
  trace. Never do JSON extraction in TF SQL (`tf_coalesce_array_literal`, variant quirks).
- `exemplarsForMetric pid metricName (lo, hi) limit` — `WHERE metric_name = … AND
  length(exemplars::text) > 2`, flatten, filter by **exemplar timestamp** inside `(lo, hi)`,
  dedupe by `trace_id`, sort by value descending (slowest first — that is what an on-call
  wants).

Trace-id matching is hex-to-hex: ingestion stores `byteStringToHexText (exemplar.trace_id)`
and spans store `context___trace_id` in the same lowercase hex, so the comparison is direct.

The service tier reuses `otel_metrics_meta` (PG catalog, already indexed on
`(project_id, service_name, metric_name)`) — no new table, no scan of raw metrics.

### 4.2 Span/log → metrics (tab)

`detailTabs` gains a `tab-metrics` entry whose content is an HTMX shell
(`hx-get`, `hx-trigger="intersect once"`) pointing at a new fragment endpoint
`/p/:pid/log_explorer/:id/:ts/related_metrics`. `panelClass` is the full literal
`group-has-[.tab-metrics:checked]/dtab:block` so Tailwind's scanner sees it
(`tailwind_scanner_no_runtime_concat`). The handler renders, in order:

1. **Recorded in this trace** — the exemplar-exact tier. Metrics that recorded a datapoint
   inside this very trace, each with its value and a link to the metric detail page. This is
   the tier Datadog does not have (it has no metric↔trace exact join at all), and it is the
   strongest statement the UI can make: *this number came from this request.*
2. **Metrics from `<service>`** — the tag tier, which is what Datadog's Infrastructure/Metrics
   tabs are. Per the survey these must be **charts, not a list of names**, so the tab renders
   real `metricWidget` timeseries for the service's metrics, reusing the same component the
   metrics pages use. Names alone would be a directory, not a correlation.

Empty state names which tier is empty and why, rather than a generic "nothing found" — Datadog
renders an explicit "trace unavailable" instead of a dead link, and the same honesty applies to
"this metric family carries no exemplars".

**Known gap vs Datadog:** those charts inherit the explorer's time range rather than being
scoped to the span's own interval with that interval shaded, because `Widget` has no
`from`/`to` of its own (it reads page params via `forward-page-params`). The survey is clear
that the overlay is the detail that makes the chart worth showing, so this is the first
follow-up — it needs a `Widget` time-range field, which is a wider change than this stream.

### 4.3 Metric → trace (tab + chart overlay)

- `metricsDetailsPage` gains an **Exemplars** tab (third `navigatable` tab), lazily loaded
  from `/p/:pid/metrics/exemplars/:metric_name`, listing representative traces: value,
  exemplar timestamp, service, and a link that opens the trace.
- The trace link is the existing log-explorer overlay contract, not a new page:
  `/p/:pid/log_explorer?showTrace=<traceId>/?timestamp=<exemplarTime>` (percent-encoded).
  `Log.hs`'s `traceOverlay` reads `showTrace` on load and fires `loadTrace`, so a plain
  `<a href>` from anywhere in the product deep-links into the trace overlay.
- `widgets.ts` gets an optional exemplar scatter series on the metric detail chart, points
  placed at (exemplar timestamp, value), clicking one navigates to the same URL. Fed by the
  same endpoint in JSON form, capped at ~100 points.

### 4.4 Deliberately out of scope

Dashboard-widget exemplars, waterfall/service-map/issue-page entry points, span-metrics
generation, and any change to the ingestion path (it is already correct).

## 5. Tests

Integration (`test/integration/`): ingest a trace, then ingest a metric whose exemplar
carries that trace's `trace_id`/`span_id` **as raw bytes** (the existing 4.1d exemplar sets
only `timeUnixNano`/`asDouble`, so it cannot be reused as-is), then assert both handlers
render the correlation. Doctests for the pure exemplar parsing/ranking.
