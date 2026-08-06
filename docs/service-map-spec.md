# Service Maps — implementation spec

Status: **implemented** on branch `worktree-service-map`. Synthesised from four research passes
(prior-art, data-model, rendering, integration), then adversarially reviewed against the tree.
Every decision below is checked against the root `CLAUDE.md`.

## What shipped vs. what this spec describes

Built and verified (`build.log` green at 124 modules; `build-test-dev.log` at the known
13-failure env-dependent baseline — CLI-E2E and live-GitHub only):

- `Models/Telemetry/ServiceGraph.hs` — payload types, `buildServiceGraph`, `traceEdgeSamples`,
  the rollup slice query, upsert and range read.
- `Pkg/Components/ServiceMap.hs` — shared Lucid shell, legend, empty/error/truncated states and
  the accessible dependency table.
- `Pages/ServiceMap.hs` + route `/p/:pid/service_map`; `Utils.explorerNavTabs_` now the single
  source of the Explorer tab strip (three duplicated copies deleted).
- `Map` tab in the trace view; `web-components/src/service-map.ts` (echarts `graph`, layered
  layout, hover adjacency, click isolation, search dimming) with 19 vitest cases.
- Migration `0117`, `ServiceMapRollupTick`/`ServiceMapRollup` jobs, `ENABLE_SERVICE_MAP_ROLLUP`
  (off by default).
- Prerequisite bug fix: the trace projection selected neither `kind` nor `status_code`, pinning
  the trace header's Errors stat to 0 for every trace. Fixed with total `parseSpanKind` /
  `parseSpanStatus` (the stored values do not match the `WrappedEnumSC` encodings), guarded by
  `traceSpanRecord_clientAndErrorSpans_decodeKindAndStatus`.

Deliberately **not** built, contrary to sections below:

- **Node duplication on revisit** for the trace map (§3, §5.4). Both maps keep real cycles and
  draw a back-edge. Duplication needs a path-aware key, and synthetic `A#2` keys would have to
  be kept out of the KQL drill-out links (§10 risk 11). Deferred, not forgotten.
- **The node-detail side panel** (§2 "Click node → HTMX-swaps the detail panel") and the
  `service_map/node` handler. Click currently isolates the transitive upstream ∪ downstream;
  the per-node metrics live in the tooltip and the dependency table. `onNodeClick` is wired in
  the renderer, so the panel is an additive change.
- **The top-150 cap / edge-prune hardening pass** (§9 stage E step 20) beyond the cap already
  applied in `buildServiceGraph`; there is no synthetic 300-service test yet.
- **Real-browser verification.** jsdom computes no layout, so the layered layout, dimming,
  roam/drag and theme repaint are unit-tested at the selection layer but never rendered. Verify
  at 1 / 2 / 30 / 200 nodes before publicising.
- **The rollup join has never run against real TimeFusion** — only Postgres, via the integration
  tests. This is the single biggest operational risk (§10 risk 4); keep the env flag off until
  it is smoke-tested there.

---

## 1. Goal + non-goals

### Goal

1. **Global service map** — a URL-addressable page at `/p/:pid/service_map` showing how data flows
   through the whole system for a time range: entry points, instrumented services, service→service
   calls, and inferred (uninstrumented) dependencies — databases, queues, third-party HTTP peers.
   Deterministic left-to-right layered layout; left = closest to the customer, right = likely root
   cause. Hover gives RED metrics; click isolates upstream/downstream; click-through hands a KQL
   query to the Events tab.
2. **Trace-level service map** — a 4th tab (`Map`) in the trace view beside Waterfall / Timeline /
   Services, answering "how did *this* request move through the system", with per-service share of
   execution time, per-hop call counts and errors, and a jump back to the entry span in the
   waterfall.
3. Both maps share **one** Haskell payload type, **one** TS renderer, and **one** visual grammar.

### Non-goals (v1, explicitly)

- **No arbitrary KQL filtering of the global map.** The global map is scoped by time only (plus
  client-side node-name search). Rationale in §5: the global map is served from a rollup, and a
  rollup cannot answer an arbitrary predicate. Datadog ships the same split — Service Map
  (unfiltered) vs Request Flow Map (filtered) are separate products. The map is a *drill-out*
  surface: clicking pushes KQL into the Events tab, which is where filtering already lives.
- No new charting dependency (no dagre, elkjs, cytoscape, d3, React). No new web component.
- No force-directed layout at all in v1 (not even as a toggle — unrequested configurability), and no
  edge animation (see §2: echarts' `lines` `effect` requires a coordinate system the `graph` series
  does not provide; `emphasis.focus:'adjacency'` already carries the signal).
- No node grouping by team/namespace, no minimap, no `<other>` fan-out folding — deferred to v2
  behind measured need (§10).
- No new signal at ingest time. No ingest-time edge pairing (rejected, §5).
- No SLO-grade percentiles: the map's p95/p99 come from a log-scale histogram (~19% error) and must
  link out to Events for exact numbers.

---

## 2. UX spec — global service map

### Screen

```
┌ Explorer ─────────────────────────────────────────────────────────────────────────────┐
│ [ Events ] [ Metrics ] [•Service Map•]              [ Last 24h ▾ ]  [ ⟳ ]              │
├───────────────────────────────────────────────────────────────────────────────────────┤
│ 🔎 filter services…                                      128 nodes · 340 edges · 24h   │
├──────────────────────────────────────────────────────────┬────────────────────────────┤
│                                                          │  checkout                  │
│   ⟶◯ gateway ──────▶ ◯ checkout ══════▶ ◇ postgres       │  service                   │
│      (entry)      ╲       │  ╲              orders       │  ──────────────────────    │
│                    ╲      │   ╲                          │  12.4k req   206 req/s     │
│                     ▶ ◯ auth   ╲──▶ ▭ kafka              │  1.9% errors  ▓▓▓▓▓░ 24%   │
│                           │         orders.v1            │  p50 12ms p95 210ms p99 1.1s│
│                           ╲                              │                            │
│                            ▶ ◇ redis                     │  Upstream                  │
│                                                          │   gateway   12.4k  1.9%    │
│                        (dimmed: everything not reachable │  Downstream                │
│                         from the selected node)          │   postgres  40.2k  0.0%    │
│                                                          │   kafka      2.1k  0.0%    │
│                                                          │   auth      12.4k  0.1%    │
│                                                          │  [ View traces ] [ Events ]│
├──────────────────────────────────────────────────────────┴────────────────────────────┤
│ Dependencies  (Lucid table — always rendered, sortable, no-JS fallback)                │
│ caller     → callee      kind      requests   errors   p95      throughput             │
│ gateway    → checkout    service     12.4k     1.9%    210ms    206/s                  │
│ checkout   → orders      database    40.2k     0.0%      3ms    670/s                  │
│ checkout   → orders.v1   queue        2.1k     0.0%      1ms     35/s                  │
└───────────────────────────────────────────────────────────────────────────────────────┘
```

### Layout

Deterministic layered DAG, left→right. **Not** force-directed. Position carries meaning:

1. Break cycles with a DFS gray-set (back-edges recorded, drawn as curved edges — see below).
2. Layer by longest path from entry nodes (`layer v = 1 + max(layer preds)`; roots = in-degree 0).
3. Order within a layer by 4 median/barycenter sweeps (down, up, down, up), deterministic
   tie-break on node id, so the picture is byte-identical across refreshes and users build spatial
   memory of their own architecture.
4. `x = layer * 190`, `y = (i - (n-1)/2) * 96`, fed to echarts as fixed `x`/`y` with `layout:'none'`.

Layered is the only mode in v1. Dense-cycle graphs still layer (back-edges are recorded and drawn
curved); if real data proves that insufficient, a force mode is a one-line `layout:'force'` switch
added later behind measured need — not shipped speculatively.

### Node encoding

| dimension | encoding |
|---|---|
| identity | fill = `getServiceColors` hash (same colour as the waterfall legend) for services; neutral `bgRaised` for inferred nodes |
| type | **shape**: `circle` = instrumented service, `diamond` = database, `roundRect` = queue/topic, `triangle` = external HTTP, `circle`+arrow-in glyph = entry |
| inferred | dashed border (`borderType:'dashed'`) **in addition to** the shape — two signals, never colour alone |
| volume | `symbolSize = 26 + 24 * sqrt(callShare)`, clamped [26, 64] |
| health | red border, width `2 + 4*errorRate`, **only when errorRate > 0**. Healthy nodes get no colour — "colour is signal, not decoration"; a green dot on every healthy node is noise. |
| label | service name below the node (echarts labels are single-line; metrics live in the tooltip and detail panel) |

### Edge encoding

- width = `1 + log2(1 + calls)`, clamped [1, 6]
- colour = `lineStyle.color:'source'` (inherits caller identity) → lerped to `textError` as error rate
  rises; ≥1% error paints the edge red outright
- `type:'dashed'` when the callee is inferred
- `edgeSymbol: ['none','arrow']`, `edgeSymbolSize: [0,9]`, `autoCurveness:false` with per-link
  `curveness` (0.06 normal, 0.2 for a back-edge / bidirectional pair)
- edge label (hover only) = the concrete peer + rate, e.g. `orders · 670/s · p95 3ms`
- **no flow animation.** echarts' animated-trail `effect` lives on the `lines` series, which requires
  a coordinate system (`cartesian2d`/`geo`); the `graph` series draws its own edges in its own
  auto-fitted pixel space, so an overlay cannot be aligned to it without reimplementing the fit.
  Direction is already carried by the arrowhead and by `emphasis.focus:'adjacency'` on hover.

### Inferred-dependency nodes

Synthesised for leaf `client`/`producer` spans with no matching child span. Key precedence
(a `COALESCE`, see §5): `peer.service` › `db.namespace` › `db.system` › `server.address` ›
`network.peer.address` › span name. Kind: `database` when a `db.*` attribute is present, `queue`
when `kind='producer'`, else `external`. Host keys are normalised — lowercased, port stripped —
before use, to bound cardinality.

### Interactions

- **Hover node** → tooltip: requests + req/s, error rate %, p50/p95/p99, share-of-traffic bar;
  `emphasis.focus:'adjacency'` + `blur:{itemStyle:{opacity:0.14}}` dims the rest.
- **Click node** → *isolate*: compute transitive upstream ∪ downstream ourselves and drive
  `dispatchAction({type:'downplay'})` on everything else (echarts' `focus:'adjacency'` is 1-hop
  only; full-path isolation is ours). Stays on the map. Simultaneously HTMX-swaps the right-hand
  **detail panel** (`htmx.ajax('GET', …/service_map/node?key=…, '#service-map-detail')`) — Lucid
  renders the card, never JS.
- **Click edge** → same panel, hop-scoped.
- **`View traces` / `Events`** in the panel → link to
  `/p/:pid/log_explorer?query=resource.service.name=="x"&since=…` (edge: `… and kind=="client"`).
  This is where filtering lives.
- **Search box** dims non-matching nodes (fuzzy, substring) rather than removing them, so paths
  never silently sever.
- **Escape / click background** clears isolation.

### Time scoping

The page carries the standard `Components.timepicker_` + `Components.refreshButton_` in
`bwconf.pageActions`, exactly as `/log_explorer` does (`Pkg.Components.TimePicker`, imported
`qualified as Components` — `timepicker_`, `refreshButton_`, `parseTimeRange`). `since`/`from`/`to`
ride the URL. The refresh path is the standard nav swap — `hx-get` the page URL itself with
`hx-select`/`hx-target` on the graph container plus `hx-ext="forward-page-params"` — so the response
is HTML, not JSON (see §7). Range is bounded server-side (max 7d in v1); absent params default to
whatever `parseTimeRange` returns, exactly as the explorer does.

### States

- **Empty range** — plain Lucid empty state: "No spans in this range." No illustration, no mascot.
- **No rollup yet** (new / low-traffic project, first bucket up to 10 min away) — distinct
  *"Collecting dependency data…"* state with the expected first-data time. Must not read as "empty".
- **Single service** — render the one node plus its inferred deps. Never an "unavailable" message.
- **Query failed** — `ServiceGraph.error :: Maybe Text` carries a sanitised message and the UI shows
  an error state. A failed query must never render as an empty graph (parity with `LogResult.error`).
- **Huge graph** — server caps at top-150 nodes by request volume and drops edges below 0.1% of
  total traffic *unless they carry errors* (never hide a failing path). `truncated :: Bool` on the
  payload drives a visible "showing top 150 of N services" banner; `logAttention` fires server-side
  because silent truncation of a dependency graph is a data-loss-shaped bug.
- **Orphan spans** (parent missing) attach to an explicit dashed `?` unknown node, not dropped.

### Accessibility

The canvas is not the only representation. The same handler payload renders a **Lucid dependency
table** below the map (caller, callee, kind, requests, error rate, p95, throughput) — semantic,
searchable, keyboard-navigable, readable on a phone from a shared link, and functional with no JS.
This is required, not a stretch goal: WCAG AA is the floor, and a canvas blob alone breaches it.

---

## 3. UX spec — trace-level map

Same renderer, same grammar, different semantics:

| | global | trace |
|---|---|---|
| unit | requests over a range | spans in one trace |
| node label | service name | `service` + **% of total execution time** |
| node metric | req/s, error rate, p50/p95/p99 | span count in this service, self time, wall time |
| hop metric | calls/s, error rate, p95 | **call count, total time across those calls, error count** |
| cycles | real information → curved back-edge, single node | **duplicated node** (a service re-entered downstream appears again) so the picture stays an acyclic flow |
| health | error rate on the edge | red border on a node whose **entry span** errored; red edge when the caller's **exit span** errored |
| data source | rollup fetch | server-derived from spans already on the page — **zero new queries** |

Hovering an errored node shows the entry span's operation + resource + error message with a
**`View entry span`** button that switches to the Waterfall tab and highlights that span row. This
cross-link is the highest-value affordance in the whole feature.

Node sizing uses share of trace duration rather than call volume. Everything else — shapes, dashed
inferred nodes, arrowheads, isolate-on-click, layered layout — is identical.

---

## 4. Navigation decision

**Global map: a third page in the Explorer group — `/p/:pid/service_map`.**

- `Metrics` is the precedent: sibling full page, own handler, own typed return, same time picker,
  listed as an Explorer sub-tab.
- Rejected — a `viz_type` radio inside `/log_explorer`: it forces the graph into the three-pane
  shell (facets + log list + details), giving the canvas ~50% width, and every
  `group-has-[#viz-*:checked]/pg:` rule would need a new arm. It also drags in the alert/monitor
  flow, which is meaningless for topology.
- Rejected — a new top-level sidebar entry: `menu` in `BodyWrapper.hs` holds six *product areas*.
  A service map is a view onto the span data the Explorer already owns.
- Rejected — a partial/`hx-get` panel: the map must be URL-addressable so a link can be shared
  during an incident (design principle 6).

**Wiring:** the Explorer tab strip is currently hand-duplicated in `Log.hs:778` and
`Telemetry.hs:235`. Adding a third tab to two copies would make it three. Extract **one**
`explorerNavTabs_ :: Projects.ProjectId -> Text -> Html ()` into `Pages.Components`, delete both
copies, and add `("Service Map", p "/service_map")` to the `"Explorer"` arm of `navFlyoutItems`
(`BodyWrapper.hs:889`). Use `navTabAttrs` verbatim — it is hx-boost based, so the `<a>` **must**
carry a real `href_`. `Pages.Components` is cycle-safe for this: `navTabAttrs` lives in `Utils`
(which `Pages.Components` already imports) and `BodyWrapper` imports `Pages.Components`, not the
reverse. All three line references above were checked against the current tree.

**Trace map: a 4th `navigatable()` tab in `#trace-tabs`.** It is a different *view of the same
object* already on screen, needs no new URL, and works on shared trace links because the data is
server-rendered into the page. It must stay a `<button onpointerdown="navigatable(…)">` + a
`.a-tab-content` panel — converting the strip to CSS radios breaks the `tab-visible` CustomEvent
that both the existing timeline chart and the new map's lazy init depend on (recorded regression).

---

## 5. Data model

### 5.1 Storage decision: **hybrid** (query-time for traces, rollup for the global map)

- **Trace map — pure fold, no query.** `getTraceDetailsForView` already returns every
  `SpanRecord` of the trace. The map is a fold over data the page holds.
- **Global map — rollup, not query-time.** Three reasons, in order of force:
  1. Postgres cannot serve it. `otel_logs_and_spans` in Timescale is documented legacy/best-effort,
     and prod's copy is intentionally empty (TF-only by design). (Retention in-repo is 30 days —
     `0009`/`0052` — but has been cut to 7 days operationally; either way the table is not the
     source of truth.) A Timescale continuous aggregate over raw spans is not available.
  2. On TimeFusion the required self-join (`child.parent_id = parent.context___span_id AND
     child.context___trace_id = parent.context___trace_id`) is byte-identical to the join shape that
     OOM-crash-looped prod on 2026-07-26 and 2026-07-29. `version_append: true` forces an unbounded
     `DedupExec` buffer per scan — a self-join instantiates it twice over the same window. A 24h
     default range × concurrent viewers is not survivable.
  3. The same join over a **5-minute slice** is 1/288th of the data, run **once per project** rather
     than once per page view, and produces a table small enough that any range answers in
     milliseconds from PG.
- **Ingest-time pairing: rejected.** Parent `client` and child `server` spans come from different
  SDKs with independent export intervals and can land minutes apart (plus hours-late DLQ replays).
  Pairing at ingest needs a durable multi-minute staging map on the hot path — strictly worse than
  doing the identical join five minutes later where the data already is.
- **No `Pkg.QueryCache`.** It is keyed by `(project, source, query_hash, bin_interval)` over
  `MetricsData` timeseries with bin-merge semantics; a graph has no bins to merge. And with a PG
  rollup the read is already sub-100ms, so v1 adds **no cache at all**. If measurement later demands
  one, add `serviceMapCache :: Cache ServiceMapKey ServiceMap` (`TimeSpec 300 0`) to `AuthContext`
  beside `hostStatsCache` — 5 lines, existing precedent.
- **KQL is not taught to JOIN.** `Pkg.Parser.Stats` has `data Sources = SSpans | SMetrics` and emits
  single-table SQL; the only JOIN in the parser is a `CROSS JOIN (VALUES …)` for percentile literals.
  Reuse KQL for *navigation out of* the map, not derivation into it.

### 5.2 Prerequisite bug fix (ship first, standalone)

`traceSpanRecord` (`Telemetry.hs:703`) hard-codes `kind = Nothing` and `status = Nothing`, and
`selectTraceSpanRows` (`:732`) selects neither `kind` nor `status_code`. Consequences: (a) the trace
map cannot classify hops; (b) the trace header's Errors stat
`V.filter (\s -> s.status == Just SSError) spanRecords` (`Telemetry.hs:853`) is **always 0** — a live
bug. (`convertOtelLogsAndSpansToSpanRecord`, `:318`, has the same two `Nothing`s with a `TODO`;
fixing it is optional for this feature but the same mapping applies.)

Fix = add `kind, status_code` to `TraceSpanRow` and to the `selectTraceSpanRows` projection, and
thread them into `traceSpanRecord`. **Decode them as `Maybe Text` and map with a total function** —
*not* via the `WrappedEnumSC` `HI.DecodeValue` instances. Two reasons, both verified in-tree and both
hard failures (`refineText` aborts the whole query, it does not yield `Nothing`):

- `status_code` is written **upper-case** — `"OK"`/`"ERROR"`/`"UNSET"` (`OtlpServer.hs:1167`) — while
  `WrappedEnumSC` round-trips `quietSnake` output (`"ok"`/`"error"`/`"unset"`). `"ERROR"` does not
  decode to `SSError`.
- `kind` is written lower-case and would decode, **except** log records are stored in the same table
  with `kind = "log"` (`OtlpServer.hs:1101`) and appear in trace fetches. There is no `SKLog`
  constructor, so a trace containing one log row would 500 the whole trace view.

So:

```haskell
spanKindOf = \case "internal"->Just SKInternal; "server"->Just SKServer; "client"->Just SKClient
                   "producer"->Just SKProducer; "consumer"->Just SKConsumer; _->Nothing
spanStatusOf = \case "OK"->Just SSOk; "ERROR"->Just SSError; _->Nothing
```

`SpanKind` also derives only `(Generic, Read, Show)` — add `Eq` (one word) so tests and the fold can
compare it; `SpanStatus` already has `Eq`.

**Extend, do not fork** — the same function serves waterfall, timeline, services and map. Failing
integration test first, per the mandatory bug-fix workflow.

### 5.3 Rollup producer SQL (runs on TF, one closed 5-minute bucket, per project)

Narrow 10-column projection; `kind IN (…)` never `OR`-of-eq (Utf8View OR-predicate bug returns
wrong rows); `kind` is tantivy raw-indexed so `IN()` routes through the index. No `COALESCE` on
array columns. Outer `LIMIT` is the cardinality guard. Every column referenced below exists on
`otel_logs_and_spans` (`0002_logs_traces_metrics.sql`) — verified. Note `project_id` is **TEXT**, not
UUID, on this table (bind `pid.toText`); it is UUID on the rollup table in §5.6.

Three DataFusion constructs to smoke-test against real TF before relying on them, since a fallback
is cheap in each case: `COUNT(*) FILTER (WHERE …)` (else `SUM(CASE WHEN … THEN 1 ELSE 0 END)`),
`GREATEST` (else `CASE WHEN`), and the two correlated `NOT EXISTS` subqueries (else `LEFT JOIN … IS
NULL`, which is what DF decorrelates them to anyway — write it that way if the plan looks bad,
because each anti-join is a second scan and therefore a second `DedupExec`; see risk 4).

```sql
WITH sp AS (
  SELECT context___trace_id tid, context___span_id sid, parent_id pid,
         COALESCE(resource___service___name,'unknown') svc, kind, status_code, duration, name,
         attributes___db___system___name db_sys, attributes___db___namespace db_ns,
         attributes___server___address srv, attributes___network___peer___address peer
  FROM otel_logs_and_spans
  WHERE project_id = $1 AND timestamp >= $2 AND timestamp < $3
    AND kind IN ('server','client','producer','consumer')),
svc_edges AS (
  SELECT p.svc src, 'service' src_kind, c.svc tgt, 'service' tgt_kind, c.status_code, c.duration
  FROM sp c JOIN sp p ON c.tid = p.tid AND c.pid = p.sid
  WHERE c.kind IN ('server','consumer') AND p.kind IN ('client','producer')),
inferred AS (
  SELECT p.svc src, 'service' src_kind,
         CASE WHEN p.db_sys IS NOT NULL THEN COALESCE(NULLIF(p.db_ns,''), p.db_sys)
              WHEN p.kind = 'producer' THEN COALESCE(NULLIF(p.srv,''), p.name)
              ELSE COALESCE(NULLIF(p.srv,''), NULLIF(p.peer,''), p.name) END tgt,
         CASE WHEN p.db_sys IS NOT NULL THEN 'database'
              WHEN p.kind = 'producer' THEN 'queue' ELSE 'external' END tgt_kind,
         p.status_code, p.duration
  FROM sp p
  WHERE p.kind IN ('client','producer')
    AND NOT EXISTS (SELECT 1 FROM sp c WHERE c.tid = p.tid AND c.pid = p.sid
                                         AND c.kind IN ('server','consumer'))),
entries AS (
  SELECT '' src, 'entry' src_kind, c.svc tgt, 'service' tgt_kind, c.status_code, c.duration
  FROM sp c
  WHERE c.kind IN ('server','consumer')
    AND (c.pid IS NULL OR c.pid = ''
         OR NOT EXISTS (SELECT 1 FROM sp p WHERE p.tid = c.tid AND p.sid = c.pid)))
SELECT src, src_kind, tgt, tgt_kind,
       CAST(FLOOR(4 * LOG(2, GREATEST(duration/1000, 1))) AS INT) lat_bucket,
       COUNT(*)::bigint                                          req_count,
       COUNT(*) FILTER (WHERE status_code = 'ERROR')::bigint     error_count,
       SUM(duration)::bigint                                     sum_duration_ns
FROM (SELECT * FROM svc_edges UNION ALL SELECT * FROM inferred UNION ALL SELECT * FROM entries) e
GROUP BY 1,2,3,4,5
ORDER BY req_count DESC
LIMIT 5000;
```

**Note on `peer.service`:** when present it names an inferred dependency directly (what Datadog
uses). It lives in the jsonb `attributes` only; v1 does *not* dive into jsonb on TF (poorly pruned).
Add it to the `CASE` head as soon as it is flattened.

### 5.4 Classification rules (single source of truth, mirrored in the trace fold)

| condition | edge / node |
|---|---|
| child `server`/`consumer` with a matched parent `client`/`producer` | service → service edge |
| child `server`/`consumer` with no in-trace parent | `entry` → service edge |
| leaf `client` with `db.*` attributes | inferred **database** node, keyed `db.namespace` › `db.system` |
| leaf `producer` | inferred **queue** node, keyed `server.address` › span name |
| other leaf `client` | inferred **external** node, keyed `server.address` › `network.peer.address` › span name |
| span whose declared parent is absent from the trace | edge from a dashed `unknown` node |

**Cycles:** global map keeps the real cycle and draws a curved back-edge (A→B→A is real
information). Trace map duplicates the node (Datadog's rule) so a single request reads as a flow.
Same builder, one `Bool` flag on the fold — not two functions.

**Error definition:** `status_code = 'ERROR'`. Note this differs from the waterfall's `spanHasErrors`
(exception *events* only). Pick one: **use `status_code = 'ERROR'` in the map and file a follow-up
to reconcile the waterfall** rather than shipping two silently different reds (§10).

### 5.5 Metrics

- edge `requests` = matched child-span count; `errors` = count filtered on `status_code='ERROR'`
- edge latency = the **callee** span duration (what the target took to serve), so node latency is a
  clean merge of its inbound edges and only one sketch is stored
- latency stored as a **dense fixed-width log-scale histogram**: one `BIGINT[]` of 48 counts,
  4 buckets/octave from 1µs (covers 1µs → ~2h; anything above lands in the top bucket). Exactly
  mergeable across buckets and ranges by element-wise addition, and avoids the wire incompatibility
  between TF's t-digest and Timescale toolkit's uddsketch. ~19% relative error on p95/p99 — fine for
  a map, never presented as authoritative.
  Dense, not the sparse `(SMALLINT[] idx, BIGINT[] count)` pair: 48 zeroed bigints is ~400 bytes
  before TOAST compression on a table that holds ~30 rows/bucket/project, and it deletes the sparse
  pivot on write, the `unnest(a,b)` join on read, and the index/count length invariant that nothing
  in the type system would have enforced. Merge is `array_agg`-free — a plain
  `zipWith (+)` in Haskell over the rows the scalar read already returns, so §5.7 collapses to **one**
  query.
- node stats = sum over inbound edges (entry edges included), so an isolated service still appears
- `throughput = requests / rangeSeconds`, computed at render time, never stored

### 5.6 Migration — `static/migrations/0117_service_dependency_edges.sql`

`0116_issue_hash_lookup.sql` is the current max. Migrations are append-only; never edit an applied
file.

```sql
BEGIN;
CREATE TABLE IF NOT EXISTS apis.service_dependency_edges (
  project_id       UUID        NOT NULL,
  bucket           TIMESTAMPTZ NOT NULL,
  source_key       TEXT        NOT NULL,
  source_kind      TEXT        NOT NULL,
  target_key       TEXT        NOT NULL,
  target_kind      TEXT        NOT NULL,
  req_count        BIGINT      NOT NULL,
  error_count      BIGINT      NOT NULL,
  sum_duration_ns  BIGINT      NOT NULL,
  lat_counts       BIGINT[]    NOT NULL,   -- dense, 48 buckets, 4/octave from 1µs
  updated_at       TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (project_id, bucket, source_key, source_kind, target_key, target_kind));
SELECT create_hypertable('apis.service_dependency_edges', by_range('bucket', INTERVAL '1 day'),
                         if_not_exists => true);
SELECT add_retention_policy('apis.service_dependency_edges', INTERVAL '30 days', if_not_exists => true);
CREATE INDEX IF NOT EXISTS service_dependency_edges_project_bucket_idx
  ON apis.service_dependency_edges (project_id, bucket DESC);
COMMIT;
```

The kinds are **in the primary key**: the producer SQL groups by them, so one `(src,tgt)` pair can
legitimately yield two rows (e.g. a peer seen once as a matched service and once as an inferred
external). With kinds outside the key the upsert would silently drop one of them — a data-loss-shaped
bug in a table whose whole job is completeness.

Upsert is REPLACE semantics (`ON CONFLICT … DO UPDATE SET col = EXCLUDED.col`) so re-running a
bucket is idempotent and absorbs late spans. Size ≈ 30 edges × 288 buckets ≈ 8.6k rows/project/day
≈ 1 MB/project/day.

`runPendingMigrations` (`System/Config.hs:447`) sorts filenames lexicographically, so `0117_…` runs
after `0116_…`; the two `0106_…` files are distinct names and order deterministically between
themselves. Numbering is settled — no open question here.

### 5.7 Read SQL (PG, any range, milliseconds)

One query — the dense histogram merges in the same `GROUP BY` as the scalars:

```sql
SELECT source_key, source_kind, target_key, target_kind,
       SUM(req_count)::bigint, SUM(error_count)::bigint, SUM(sum_duration_ns)::bigint,
       (SELECT array_agg(SUM(c) ORDER BY i)
          FROM unnest(lat_counts) WITH ORDINALITY AS h(c,i))   -- element-wise array sum
FROM apis.service_dependency_edges
WHERE project_id = $1 AND bucket >= $2 AND bucket < $3
GROUP BY 1,2,3,4;
```

(If nesting the aggregate proves awkward, the equally acceptable fallback is to select the raw
`lat_counts` per bucket-row and `zipWith (+)` them in Haskell — the row count is bounded by
edges × buckets and the fold is three lines. Pick whichever reads cleaner; do not ship both.)

Percentiles are read off the **merged** histogram in Haskell with a cumulative-count scan, so they
are correct for any range (a request-weighted average of per-bucket p95s is not).

### 5.8 Background job

Add to `data BgJobs`:

- `ServiceMapRollupTick UTCTime` — dispatcher, fanning out over `Projects.recentlyActiveProjectIds`
  the way the daily seeder already does.
- `ServiceMapRollup Projects.ProjectId UTCTime UTCTime` — one closed 5-minute bucket.

Ticks are **seeded, not self-re-enqueuing** — that is the actual in-tree pattern:
`seedJobs conn currentTime 288 300 BackgroundJobs.ServiceMapRollupTick` beside the existing
`seedJobs conn currentTime 1440 60 BackgroundJobs.PrometheusScrapeTick` (`BackgroundJobs.hs:403`),
i.e. 288 ticks × 300 s = 24 h, re-seeded by the same daily block. (`seedJobs`'s third/fourth
arguments are `count` and `step` in **seconds** — `:273`.)

Each per-project job runs the §5.3 SQL via `Hasql.withHasqlTimefusion True`
(`Data/Effectful/Hasql.hs:204`), folds `(edge, lat_bucket)` rows into the dense 48-slot array in
Haskell, and does one multi-row upsert into PG. It processes `[now-10m, now-5m)` so normal
export/ingest lag is already absorbed; the existing `HourlyJob` re-rolls the last 2 hours to pick up
DLQ replays (idempotent, upsert replaces). `enableBackgroundJobs` guard is already global in
`jobsRunner` (`:228`). Gate the rollup behind a new `enableServiceMapRollup :: Bool` field on
`System.Config.EnvConfig` (with a `.env.example` entry) for a staged rollout — the flag is a config
field, never an ad-hoc `lookupEnv`.

Instrument as a power user would want: a `service_map.rollup` span per run with project/bucket/edge
count attributes, and `logAttention` when the 5000-edge LIMIT is hit.

---

## 6. Haskell design

### New / changed modules

| file | change |
|---|---|
| `src/Models/Telemetry/Telemetry.hs` | add `kind`, `status_code` to `TraceSpanRow` + `selectTraceSpanRows` + `traceSpanRecord` (bug fix, §5.2); add `serviceMapRollupSlice` (TF producer SQL) and `serviceDependencyEdges` (PG read SQL) |
| `src/Models/Telemetry/ServiceGraph.hs` | **new** — the shared payload types and the single `buildServiceMap` fold used by both maps. Lives in `Models/`, not `Pages/`, to avoid an import cycle with `Pages.LogExplorer.Log` |
| `src/Pages/ServiceMap.hs` | **new** — page handler + node-detail handler, Lucid rendering (map shell, legend, dependency table, detail card) |
| `src/Pages/Components.hs` | **new** `explorerNavTabs_`; used by Log, Telemetry, ServiceMap |
| `src/Pages/LogExplorer/Log.hs` | delete `logExplorerNavTabs_`; export `logDataEnv` (do not fork the time-range prologue) |
| `src/Pages/Telemetry.hs` | delete the inline nav copy; add the trace `Map` tab + panel; emit `traceGraphJson` beside `spanJson`; extend `initTraceCharts` |
| `src/BackgroundJobs.hs` | two new `BgJobs` constructors + handlers |
| `src/Web/Routes.hs` | two route fields + two bindings |
| `static/migrations/0117_service_dependency_edges.sql` | new |

Run `hpack` after adding files.

### Payload types (`Models/Telemetry/ServiceGraph.hs`)

```haskell
data NodeKind = NKEntry | NKService | NKDatabase | NKQueue | NKExternal | NKUnknown
  deriving stock (Eq, Generic, Ord, Show)
  deriving (AE.FromJSON, AE.ToJSON, FromField, HI.DecodeValue, HI.EncodeValue, ToField)
    via WrappedEnumSC 'Nothing "NK" NodeKind
  -- serialises as entry/service/database/queue/external/unknown and round-trips the *_kind columns

data MapStats = MapStats
  { requests :: !Int64, errors :: !Int64
  , p50Ns :: !Int64, p95Ns :: !Int64, p99Ns :: !Int64
  , throughputPerSec :: !Double }
  deriving stock (Generic, Show) deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MapStats

data ServiceNode = ServiceNode
  { key :: !Text, label :: !Text, kind :: !NodeKind, inferred :: !Bool
  , durationShare :: !(Maybe Double)   -- trace map only: % of total execution time
  , stats :: !MapStats }
  deriving stock (Generic, Show) deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ServiceNode

data ServiceEdge = ServiceEdge
  { source :: !Text, target :: !Text, peerLabel :: !(Maybe Text), stats :: !MapStats }
  deriving stock (Generic, Show) deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ServiceEdge

data ServiceGraph = ServiceGraph
  { nodes :: !(V.Vector ServiceNode), edges :: !(V.Vector ServiceEdge)
  , rangeSeconds :: !Double, truncated :: !Bool, error :: !(Maybe Text) }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier DAE.CamelToSnake] ServiceGraph
```

`error :: Maybe Text` is deliberate parity with `LogResult.error`: a failed query renders an error
state, never a misleadingly empty graph. `NodeKind` is a sum type, not `isDatabase`/`isExternal`
booleans, so the renderer's `case` is exhaustive and illegal combinations are unrepresentable.

**One builder, both maps** (no `fooV2`):

```haskell
newtype LatencyHist = LatencyHist (IntMap Int64) deriving (Monoid, Semigroup) via ...

data EdgeSample = EdgeSample
  { source :: !(Maybe (Text, NodeKind)), target :: !(Text, NodeKind)
  , requests :: !Int64, errors :: !Int64, latency :: !LatencyHist }

data CycleMode = KeepCycles | DuplicateOnRevisit   -- global vs trace

buildServiceMap :: CycleMode -> Double -> [EdgeSample] -> ServiceGraph
```

The trace path emits one `EdgeSample` per matched span pair (`requests=1`, singleton histogram);
the rollup path emits pre-aggregated rows. Percentiles are read once off the merged histogram.

### Handlers (`src/Pages/ServiceMap.hs`)

```haskell
newtype ServiceMapGet = ServiceMapPage (PageCtx ServiceMapPageData)

data ServiceMapPageData = ServiceMapPageData
  { pid :: Projects.ProjectId, currentRange :: Maybe (Text, Text)
  , focus :: Maybe Text, dataUrl :: Text, graph :: ServiceGraph }

instance ToHtml ServiceMapGet where
  toHtml (ServiceMapPage (PageCtx conf d)) = toHtml $ PageCtx conf $ serviceMapPage d
  toHtmlRaw = toHtml

newtype ServiceMapNodeDetail = ServiceMapNodeDetail (Text, ServiceGraph)  -- ToHtml → Lucid card

serviceMapH     :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text
                -> ATAuthCtx (RespHeaders ServiceMapGet)
serviceMapNodeH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text
                -> ATAuthCtx (RespHeaders ServiceMapNodeDetail)
```

The page handler renders the graph server-side (it is cheap from the rollup), so the dependency
table and the shared-link view work with no JS, and the canvas hydrates from the JSON the same
response embeds. **There is no separate JSON endpoint**: a `Get '[JSON]` route cannot be swapped by
HTMX (every widget precedent — `Widget.hs:630`, `:663` — `hx-get`s *HTML* and `hx-select`s a node
out of it), and a second handler recomputing the same graph would be exactly the forked-function
this repo forbids. Refresh/time-change is an `hx-get` on the page URL with `hx-select` of the graph
container.

Both handlers use `logDataEnv` (exported from `Log.hs`, which already resolves auth + config + clock
+ `Components.parseTimeRange`) — no second time-range parser. Do **not** collapse these to
`RespHeaders (Html ())`; the typed newtypes are what make §8 structural. No `ToSchema` is needed:
only `ApiV1Routes` feeds `toOpenApi` (`Routes.hs:780`).

### Routes (`src/Web/Routes.hs`)

Insert into `LogExplorerRoutes'` after `aiSearchPost` (line 496) — it is already mounted at
`"p" :> ProjectId :> LogExplorerRoutes` (line 471), so no new record:

```haskell
  , serviceMapGet :: mode :- "service_map" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "focus"
      :> Get '[HTML] (RespHeaders ServiceMap.ServiceMapGet)
  , serviceMapNodeGet :: mode :- "service_map" :> "node" :> QPT "key" :> QPT "since" :> QPT "from"
      :> QPT "to" :> Get '[HTML] (RespHeaders ServiceMap.ServiceMapNodeDetail)
```

Bindings after line 883 in `logExplorerServer`:

```haskell
    , serviceMapGet     = ServiceMap.serviceMapH pid
    , serviceMapNodeGet = ServiceMap.serviceMapNodeH pid
```

plus `import Pages.ServiceMap qualified as ServiceMap` near line 96.

---

## 7. Frontend design

### Approach

Plain exported TS functions in **one new file**, `web-components/src/service-map.ts` (~250 lines).
No Lit component, no new dependency. echarts is a runtime global loaded lazily from
`/public/assets/deps/echarts/echarts.min.js`; the shipped 1.07 MB build already contains the full
`graph` series (`edgeSymbol`, `curveness`, `autoCurveness`, `edgeLabel`, `roam`, `adjacency` all
verified present). Register lazily in `index.ts`:
`['[data-service-map]', () => import('./service-map')]`, and publish
`window.serviceMapChart` for the trace page's inline `initTraceCharts` — but the module must
`await ensureECharts()` itself rather than assuming `window.echarts` (the race `index.ts` already
warns about).

### Required small refactors (no duplication allowed)

- `widgets.ts`: export `ensureECharts` (currently module-private, line 22) and add
  `subscribeChartTheme(cb): () => void` over the private `themeCallbacks` set (line 135).
  `getChartStyles` is already exported.
- Move `resolveColor` out of `charts.ts` into `colorMapping.ts` so waterfall, timeline and map share
  one service→hex path and a service is the same colour everywhere.
- Register in `widgets.ts`'s `chartDisposers` (disposed on `htmx:beforeSwap`) and observe with
  `sharedResizeObserver`; also listen for `loglist-resize` / `toggle-sidebar` like the waterfall.

### Layout algorithm

Pure, exported, DOM-free — `breakCycles`, `layerGraph`, `orderLayers`, `assignCoords` — so
`web-components/test/service-map.test.ts` (vitest is already configured) can test them.
~80 lines total: DFS gray-set cycle break → longest-path layering → 4 barycenter sweeps →
`x = layer*190`, `y = (i-(n-1)/2)*96`. Edge *routing* is not needed: echarts draws its own quadratic
beziers from `curveness`. **Do not add dagre (~90 KB + graphlib) or elkjs (~1.4 MB)** — neither buys
anything at tens of nodes; the escape hatch (dynamic-import elkjs) is documented, not taken.

Because `layout:'none'` fits the supplied coordinate range into the box, small graphs stretch: pad
the virtual extent to a minimum of 3 layers wide × 4 rows tall with invisible bounds. Verify
visually at 1 / 2 / 30 / 200 nodes — this is the most likely "looks broken" failure.

### echarts option sketch

Derived client-side, **not** fields on the wire payload (§6 sends only `key/label/kind/inferred/
duration_share/stats` and `source/target/peer_label/stats`): `errorRate = errors/requests`,
`callShare = requests / max requests`, an edge's `inferred` = its target node's kind ≠ `service`,
`back` = the back-edge set returned by `breakCycles`, and `x`/`y` = `assignCoords` output. Keeping
them out of the payload is why there is one `MapStats` for both maps.

```ts
const s = getChartStyles();
chart.setOption({
  animation: false, backgroundColor: 'transparent',
  tooltip: { trigger: 'item', appendToBody: true, backgroundColor: s.tooltipBg,
             borderColor: s.tooltipBorderColor, borderWidth: 1, textStyle: { color: s.tooltipTextColor },
             formatter: p => p.dataType === 'edge' ? edgeTip(p.data) : nodeTip(p.data) },
  series: [{
    type: 'graph', layout: 'none', left: 32, right: 32, top: 28, bottom: 28,
    roam: true, scaleLimit: { min: 0.35, max: 4 }, draggable: true, selectedMode: 'single',
    symbol: 'circle', symbolSize: 44, cursor: 'pointer',
    edgeSymbol: ['none', 'arrow'], edgeSymbolSize: [0, 9], autoCurveness: false,
    categories: [{ name: 'entry' }, { name: 'service' },
                 { name: 'database', symbol: 'diamond' }, { name: 'queue', symbol: 'roundRect' },
                 { name: 'external', symbol: 'triangle' }, { name: 'unknown', symbol: 'circle' }],
    label: { show: true, position: 'bottom', distance: 9, fontSize: 11, color: s.textColor,
             formatter: p => p.data.label },
    edgeLabel: { show: false, fontSize: 10, color: s.textColor, formatter: p => p.data.tip },
    lineStyle: { color: 'source', width: 1.5, opacity: 0.55, curveness: 0.06 },
    emphasis: { focus: 'adjacency', scale: 1.08, edgeLabel: { show: true },
                lineStyle: { width: 3, opacity: 1 }, label: { fontWeight: 'bold' } },
    blur:    { itemStyle: { opacity: 0.14 }, lineStyle: { opacity: 0.06 }, label: { opacity: 0.14 } },
    select:  { itemStyle: { borderColor: s.brandColor, borderWidth: 3 } },
    data: nodes.map(n => ({
      id: n.key, name: n.key, label: n.label, category: n.kind, x: n.x, y: n.y, fixed: true,
      symbolSize: 26 + 24 * Math.sqrt(n.callShare),
      itemStyle: { color: resolveColor(n.key, colorsMap),
                   borderType: n.inferred ? 'dashed' : 'solid',
                   borderColor: n.errorRate > 0 ? s.errorColor : (n.inferred ? s.borderWeak : 'transparent'),
                   borderWidth: n.errorRate > 0 ? 2 + 4 * n.errorRate : (n.inferred ? 1.5 : 0) } })),
    links: edges.map(e => ({
      source: e.source, target: e.target, value: e.stats.requests,
      lineStyle: { width: 1 + Math.log2(1 + e.stats.requests),
                   color: e.errorRate > 0.01 ? s.errorColor : undefined,
                   type: e.inferred ? 'dashed' : 'solid',
                   curveness: e.back ? 0.2 : 0.06 },
      tip: `${fmtRate(e.stats.requests)} · p95 ${fmtDur(e.stats.p95Ns)}` })) }] }, true);
chart.on('click', p => p.dataType === 'node'
  && htmx.ajax('GET', nodeHref(p.data.id), '#service-map-detail'));
```

`colorsMap` is the server-rendered `getServiceColors` map (Tailwind class names like `bg-blue-400`,
which `resolveColor` runs through `tailwindToHex`), serialised into the response exactly as
`colorsJson` already is at `Telemetry.hs:960` / `Dashboards.hs:883`.

Theming: node **fill** comes from the service-colour hash (identity, theme-invariant); stroke,
label, tooltip and background come from the OKLCH semantic tokens via `getChartStyles` +
`toEChartsColor`, re-applied on `subscribeChartTheme`. Dark-mode parity for free, no second palette.

### How data arrives

Split per "Lucid owns HTML; HTMX owns HTML arrival":

- **Markup** — page shell, tab strip, legend swatches (`getServiceColor`), empty/collecting/error
  states, dependency table, node detail card — is Lucid. Never HTML built in JS.
- **Graph model** — nodes/edges consumed by a canvas renderer — is *not* markup, so it travels as
  JSON, but it is **embedded in the Lucid response**, not fetched from a JSON endpoint: the graph
  container holds a `<script type="application/json" id="service-map-data">` that `service-map.ts`
  reads on init. Refresh/time-change re-fetches the *page* (`hxGet_` the page URL, `hxSelect_`/
  `hxTarget_` the container, `hxExt_ "forward-page-params"` so `since`/`from`/`to` ride along) and
  re-inits on `htmx:afterSwap`. This mirrors how every widget in `Widget.hs` works — HTMX moves
  HTML, always — and means the shared link, the no-JS view and the canvas all read one response.
- **Node detail** — server-rendered Lucid swapped into `#service-map-detail` by
  `htmx.ajax('GET', …)` on node click.
- **Trace map** — the JSON is serialised server-side beside `spanJson` in `tracePage`, so the shared
  trace link needs no fetch at all. Init lazily on the panel's first `tab-visible`, guarded by a
  `dataset.mapInit` flag (echarts renders 0×0 into a hidden container).

---

## 8. Test plan

Integration first, per repo policy. Verify by reading `build-test-dev.log` under
`TEST_MATCH=/ServiceMap/ make live-test-dev` — never `cabal test`.

### Test-harness prerequisite: **none — the helpers already exist**

Earlier drafts called for widening `mkResource`/`mkSpanRequest`/`ingestSpanLinked` with span-kind and
service-name parameters across ~12 call sites. That is wrong and must not be done:

- `withSpanKind :: Span'SpanKind -> ExportTraceServiceRequest -> …` and
  `withSpanStatus :: Status'StatusCode -> …` already exist (`TestUtils.hs:1379`/`:1383`), together
  with `ingestSpanReq` (`:1376`) — and their doc comment states verbatim that they exist "so tests
  can build the client→server and producer→consumer pairs a service graph is derived from without
  every `mkSpanRequest` call site growing two more positional arguments."
- `mkResource apiKey extras` (`:1676`) already filters any default whose key is re-supplied, so
  `mkResource key [mkAttr "service.name" "gateway"]` overrides the `test-service` default. Extras win.

So fixtures compose the existing pieces and touch **zero** existing call sites:

```haskell
ingestSpanReq tr . withSpanKind PT.Span'SPAN_KIND_CLIENT
  $ mkSpanRequest trId spanId (Just parent) "pg.query" [] Nothing
      [mkAttr "db.system.name" "postgresql", mkAttr "db.namespace" "orders"]
      (mkResource apiKey [mkAttr "service.name" "checkout"]) ts
```

If that turns out to be repeated more than ~4 times, the only allowed addition is one local `where`
helper inside the spec file — not a change to `TestUtils`.

### `test/integration/Models/Telemetry/TraceSpanKindSpec.hs` (bug fix, written FIRST, must fail)

Ingest a 2-span trace where one span has `status_code = ERROR`. Assert
`getTraceDetailsForView` returns `kind == Just SKClient` for the client span and that the rendered
trace page's Errors stat is `1`. Fails on master (always 0), passes after §5.2. Named
`traceSpanRecord_errorSpan_countsInHeader`.

### `test/integration/Pages/ServiceMapSpec.hs` (new, auto-discovered by hspec-discover)

One shared 4-span fixture, ingested with `ingestSpanReq` + `withSpanKind`/`withSpanStatus` after
`createTestAPIKey`:
gateway SERVER `GET /checkout` → checkout SERVER → checkout CLIENT `pg.query`
(`db.system.name=postgresql`, `db.namespace=orders`) → checkout PRODUCER `orders publish`.

1. `empty project → nodes and edges empty, error is Nothing` — guards the swallow-error-as-empty
   failure mode `error` exists for.
2. `derives cross-service, entry, database and queue edges` — after running the rollup job for the
   fixture bucket, assert the edge set is
   `[(entry,gateway),(gateway,checkout),(checkout,orders),(checkout,orders publish)]`, node kinds are
   `[NKEntry, NKService, NKService, NKDatabase, NKQueue]`, and the `orders` node exists although no
   span carries `service.name=orders` — the inference is the whole point.
3. `error spans surface as edge errors` — flip one child span to ERROR; assert
   `errors == 1` on exactly that edge and `0` elsewhere (pins the `status_code='ERROR'` definition).
4. `rollup upsert is idempotent` — run the rollup job twice over the same bucket; assert request
   counts are unchanged (REPLACE semantics, protects the hourly re-roll).
5. `time range excludes older spans` — ingest one span 24h earlier; assert absent for a ±60s window.
6. `shell smoke` — `(_, ServiceMapPage (PageCtx conf d)) <- testServant tr (serviceMapH …)`; assert
   `conf.pageTitle == "Service Map"`, `d.graph.nodes` is non-empty, and `Lucid.renderText` contains
   `id="service-map-data"` (the embedded graph JSON) and all three Explorer tab labels. Structural
   because the return is typed.

### `test/integration/Pages/TelemetrySpec.hs` (extend)

7. `trace map tab renders with derived graph` — `around withTestResources`, same 4-span fixture,
   call `traceH`, render, assert the HTML contains `id="service_map"`,
   `navigatable(this, '#service_map', '#trace-tabs', 't-tab-active')` (the exact 4-argument form the
   other three tabs use — `navigatable` itself takes 5 params, `BodyWrapper.hs:270`), both service
   names in the embedded graph JSON, and a `duration_share` for each service node.

### Doctests (`test/doctests/`) — pure invariants only

`buildServiceMap` with `DuplicateOnRevisit` on `A→B→A` yields three nodes (`A`, `B`, `A#2`) and no
cycle; with `KeepCycles` yields two nodes and a back-edge. Histogram merge round-trip:
`percentiles (h1 <> h2)` is monotone and `p50 <= p95 <= p99`.

### `web-components/test/service-map.test.ts` (vitest, no DOM)

`layerGraph`/`orderLayers` on: single node, disconnected components, a cycle, wide fan-out.
Assert determinism (two runs are identical) and that no edge points backwards after layering except
recorded back-edges.

---

## 9. Implementation plan

### Stage A — prerequisite bug fix (tree working, shippable alone)

1. Write `TraceSpanKindSpec.hs` asserting `kind`/`status` decode and the Errors header count.
   **VERIFY:** `TEST_MATCH=/TraceSpanKind/ make live-test-dev`, read `build-test-dev.log` — it must
   **fail**.
2. Add `kind, status_code` to `TraceSpanRow` (as `Maybe Text`), the `selectTraceSpanRows`
   projection, and the total `spanKindOf`/`spanStatusOf` mapping in `traceSpanRecord`; add `Eq` to
   `SpanKind`. **VERIFY:** read `build.log` (clean); `build-test-dev.log` shows the spec green, and
   the whole Telemetry/LogExplorer suite is no worse than baseline — a decode regression here breaks
   every trace view, so compare failure *names*.

### Stage B — shared model + trace map (no new queries, no migration)

3. Add `src/Models/Telemetry/ServiceGraph.hs` with `NodeKind`, `MapStats`, `ServiceNode`,
   `ServiceEdge`, `ServiceGraph`, `LatencyHist`, `EdgeSample`, `CycleMode`, `buildServiceMap`, and
   the span→`EdgeSample` fold. Run `hpack`. **VERIFY:** read `build.log`.
4. Add the doctests for `buildServiceMap` + histogram merge. **VERIFY:** `make test-doctests`.
5. *(no step — the test harness needs no change; see §8.)*
6. Add the trace `Map` tab button + `#service_map` panel in `Telemetry.hs`; serialise
   `traceGraphJson` beside `spanJson`. **VERIFY:** `build.log`.
7. Export `ensureECharts` + `subscribeChartTheme` from `widgets.ts`; move `resolveColor` to
   `colorMapping.ts` and update `charts.ts` call sites. **VERIFY:** read `web-components.log`.
8. Add `web-components/src/service-map.ts` (layout functions + `serviceMapChart` + window export)
   and register it lazily in `index.ts`; wire `initTraceCharts` to init on first `tab-visible`
   behind `dataset.mapInit`. **VERIFY:** `web-components.log`; visually check a trace at 1, 2 and
   30 services.
9. Add `web-components/test/service-map.test.ts`. **VERIFY:** `npx vitest run` in `web-components/`.
10. Add TelemetrySpec test 7. **VERIFY:** `TEST_MATCH=/Telemetry/ make live-test-dev`.

### Stage C — rollup pipeline (backend only, no UI change)

11. Add migration `0117_service_dependency_edges.sql`. **VERIFY:** restart the app with
    `MIGRATE_AND_INITIALIZE_ON_START=True`; confirm the table + hypertable exist.
12. Add `serviceMapRollupSlice` (TF producer) and `serviceDependencyEdges` (PG read) to
    `Models/Telemetry/Telemetry.hs`, plus the dense-histogram fold + upsert. **VERIFY:** `build.log`.
13. Add `ServiceMapRollupTick` / `ServiceMapRollup` to `BgJobs` + handlers + the `seedJobs` entry +
    the hourly 2h re-roll, behind `enableServiceMapRollup` (add to `EnvConfig` + `.env.example`); add the `service_map.rollup` span and the LIMIT-hit `logAttention`.
    **VERIFY:** `build.log`.
14. Add `ServiceMapSpec.hs` tests 1–5. **VERIFY:** `TEST_MATCH=/ServiceMap/ make live-test-dev`.

### Stage D — global map page

15. Extract `explorerNavTabs_` into `Pages.Components`; delete `logExplorerNavTabs_` and the inline
    copy in `Telemetry.hs`; add the Service Map entry to `navFlyoutItems`. **VERIFY:** `build.log`;
    Events/Metrics tabs still render active correctly.
16. Add `src/Pages/ServiceMap.hs` (three handlers, Lucid shell + legend + dependency table + detail
    card, empty / collecting / error / truncated states). Export `logDataEnv` from `Log.hs`.
    Run `hpack`. **VERIFY:** `build.log`.
17. Add the two routes + bindings + import in `Routes.hs`. **VERIFY:** `build.log`.
18. Wire the shell to `service-map.ts` (`data-service-map`, embedded `#service-map-data` JSON,
    page-URL `hx-get` + `hx-select` refresh with `hx-ext="forward-page-params"`,
    isolate-on-click, search box, detail-panel `htmx.ajax`). **VERIFY:** `web-components.log`;
    load `/p/00000000-0000-0000-0000-000000000000/service_map`.
19. Add ServiceMapSpec test 6 (shell smoke). **VERIFY:** `TEST_MATCH=/ServiceMap/ make live-test-dev`.

### Stage E — hardening

20. Server-side top-150 node cap + 0.1% edge prune (errors exempt) + `truncated` banner.
    **VERIFY:** integration test with 300 synthetic services asserts `truncated == True` and
    `length nodes == 150`.
21. `make fmt`, `make lint`, `weeder`. **VERIFY:** clean; then enable the rollup flag for a handful
    of projects and watch the TF query pool + `mor_delta_leg_sorts` before a full rollout.

---

## 10. Open questions / risks

1. **Two different reds.** The map counts `status_code='ERROR'`; the waterfall reddens rows via
   `spanHasErrors` (exception *events* only). Users will notice the disagreement. v1 uses
   `status_code`; a follow-up must reconcile the waterfall or show both explicitly. Decide before
   the feature is publicised.
2. **Late-arriving spans.** A child that lands after its bucket rolled up leaves a phantom
   `external` node for the parent's client span. The 5-min lag + hourly 2h re-roll cover normal
   lag; an hours-late DLQ replay still leaves a stale bucket. Accept and document, or widen the
   re-roll window when a replay is known to be in flight.
3. **Inferred-node cardinality.** `external` keyed on `server.address` can be per-tenant hostnames;
   `database` keyed on `db.namespace` can be per-tenant schemas. Normalisation (lowercase, strip
   port) plus the LIMIT bounds the table but truncates silently — the `truncated` flag and the
   `logAttention` are the mitigation. Watch the first week of real data.
4. **The rollup join still runs on TF with `version_append`/DedupExec active**, just over 1/288th
   the data. Staged rollout behind the env flag is mandatory; this is the single biggest operational
   risk in the plan.
5. **Messaging attributes are not flat columns on spans** (only on `otel_metrics`), so v1 names
   queue nodes from `server.address`/span name. Adding `attributes___messaging___system`,
   `___destination___name`, `___operation` is three `attrText` lines + a PG migration + three TF
   schema entries — cheap, but it touches the TimeFusion repo, so it is deliberately out of v1.
6. **No KQL on the global map** is the accepted cost of the rollup. If users demand a filtered map,
   the answer is a *second*, explicitly-bounded query-time path (Datadog's Request Flow Map), not
   bending the rollup — and it must be range-capped and cached before it goes near TF.
7. **`layout:'none'` box-fitting** stretches small graphs and crams large ones. Minimum virtual
   extent is the mitigation; verify at 1/2/30/200 nodes before shipping. (This is also why no
   pixel-space `lines` overlay is possible — see §2 edge encoding.)
8. **echarts node labels are single-line and unstyled** — Datadog's node cards (name + rate + error
   badge) are not reproducible without a `graphic`/custom-series overlay. v1 accepts
   `label.position:'bottom'` with the name only; metrics live in the tooltip and the detail panel.
9. **Leaks and blank canvases**: the map renders inside HTMX-morphed containers and inside hidden
   tabs. A missed dispose leaks an echarts instance per navigation; rendering into a zero-size
   container yields a blank canvas. Both are silent — the `chartDisposers` registration and the
   `tab-visible` guard are not optional.
10. **The `status_code`/`kind` decode traps (§5.2) are the single most likely way to break
    production while "just adding two columns"**: `WrappedEnumSC` would reject the stored `"ERROR"`
    casing, and `kind = "log"` rows have no `SpanKind` constructor. Both fail the *whole query*, not
    one field, so a naive derive takes down every trace view. Total mapping from `Maybe Text` only.
11. **Trace-level cycle duplication + node keys.** `DuplicateOnRevisit` mints synthetic keys
    (`A#2`); those keys leak into the DOM (detail-panel lookups, isolate sets, the dependency table).
    Keep the synthetic suffix out of `label` and make sure nothing round-trips a duplicated key into
    a KQL `resource.service.name==` link.
