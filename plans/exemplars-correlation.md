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

### Datadog (the model we copy)

Datadog's correlation rests on **unified service tagging**: every telemetry type — metric,
span, log, profile, RUM event — carries `env`, `service` and `version` as reserved tags. It is
the join key. Trace-level `trace_id`/`span_id` injection into logs is a second, exact key on
top of it. So Datadog has exactly the two tiers we build below: an *exact* id join where it
exists, and a *tag + time-window* join that always works.

Concretely, on a **span's detail (flame-graph) panel** Datadog shows a tab strip —
`Info`, `Errors`, `Metrics`/`Infrastructure`, `Logs`, `Processes`, `Profiles`, `Code Hotspots`,
`Network` — where:

- **Metrics / Infrastructure** shows host-, container- and runtime-level metrics for the
  host and container that emitted the span, scoped to the span's own time window. It is not
  a search: the tags are read off the span (`host`, `container_id`, `service`, `env`) and used
  as the metric query's filter. Sparse or missing tags simply yield fewer rows.
- **Logs** shows logs correlated by `trace_id` first, falling back to service + host + window.
- The window is the span's own start/end, widened for readability — not the page's time
  picker.

From a **metric graph**, Datadog's graph context menu ("View related …") carries the clicked
point's timestamp *and* the graph's current tag scope into APM trace search, log search or the
profiler. The user never types a query; the link is constructed from the point.

The two takeaways we adopt verbatim: (a) the correlation is a *tab in the existing detail
panel*, not a separate page; (b) the link out of a chart carries **point timestamp + tag
scope**, not just "go to traces".

### Grafana / Prometheus / OpenTelemetry

- Prometheus stores OpenMetrics exemplars — `{trace_id="…"} <value> <timestamp>` appended to a
  sample — in a fixed-size circular buffer, at most **one exemplar per series per scrape**.
  They are explicitly best-effort and not retained with the series.
- Grafana renders them as **diamond markers on the time-series panel**, layered over the line.
  The Prometheus datasource's `exemplarTraceIdDestinations` config maps an exemplar label
  (`trace_id`) to a Tempo datasource, so the diamond's tooltip gets a "Query with Tempo" link.
- The reverse direction is Tempo's **"Trace to metrics"** datasource setting: a query template
  with `$__tags` interpolated from the span's own tags, so opening a span runs a metrics query
  scoped to that span's service/instance. That is the same two-tier idea, spelled as config.
- The OTel spec's exemplar fields are exactly what we already ingest: `trace_id`, `span_id`,
  `time_unix_nano`, `value`, `filtered_attributes`. Default SDK reservoirs keep 1 exemplar per
  bucket (histograms) or 1 per aggregation cycle (sums/gauges) — which is precisely the
  staleness behaviour observed in our production data above.

### Honeycomb / New Relic

- **Honeycomb** has no metric-first model: everything is a wide event, so "related metrics"
  is a `BubbleUp`/group-by over the same events. Nothing to copy directly, but it validates
  ranking by *shared dimensions* — which our existing `relatedMetricScore` already does.
- **New Relic** joins by entity GUID (its version of unified service tagging) and derives
  span-metrics from spans, so a metric point can always be decomposed back into the spans that
  produced it. We do not have span-metrics, so exemplars are our only exact metric→trace edge.

### Conclusion

Everyone converges on: **exact id join where the data carries one, tag + time-window join
otherwise, surfaced as a tab inside the thing you already have open.** Nobody makes the user
navigate to a correlation page. We build exactly that.

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

1. **Exemplar matches** — metrics that recorded a datapoint *inside this very trace*, each a
   row with metric name, value, and a link back to the metric detail page.
2. **Service metrics** — every metric `service.name` emitted, from the catalog, linking to the
   metric detail page pre-scoped to that service and the span's time window.

Empty state names which tier is empty and why, rather than a generic "nothing found".

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
