# Issue detail page — findings, comparison, and plan

Working doc. Scope: `/p/:pid/issues/:id` (`Pages.Anomalies.anomalyDetailPage`).

## 1. What we render today

One handler (`anomalyDetailCore`) → one renderer (`anomalyDetailPage`), which
branches five ways on `Issues.issuePayload`. The shell is shared; almost nothing
inside it is.

| | RuntimeException | LogPattern (+RateChange) | ApiChange | QueryAlert |
|---|---|---|---|---|
| Title | ✓ | ✓ (token-rendered) | ✓ | ✓ |
| Recommended action | ✓ | ✓ | ✓ | ✓ |
| Chips | severity, type | severity, type, level, service, count, first-seen | severity, type, created | severity, type, created |
| Chart | Error Frequency | Pattern Volume | Request Trend | **none** |
| Context panel | Error Details (4 facts) | — | Endpoint Details (4 facts) | **none** |
| Evidence | Stack Trace | Log Pattern + Sample | Field Changes | **query string only** |
| Activity | ✓ | ✓ | ✓ | ✓ |
| Investigation (trace/logs) | ✓ | ✓ | ✓ | ✓ (empty) |
| Similar patterns | ✓ | — | — | — |

### The four reference issues

| URL | Type | What actually renders |
|---|---|---|
| `7c4031a0` #112128 | log_pattern | pattern + sample message; "No trace data available" |
| `adedd1ec` #112126 | query_alert | **a query string and nothing else**; "No trace data available" |
| `7e6d04e5` #112125 | runtime_exception | "No stack trace captured" |
| `d06cf6ca` #112088 | runtime_exception | "No stack trace captured" |

## 2. Findings (verified against the demo project's data)

### F1 — QueryAlert is a dead end. *Severity: highest.*
`QueryAlertData` carries `queryName`, `thresholdValue`, `actualValue`,
`thresholdType`, `triggeredAt`. **We render exactly one of six fields.** Issue
#112126's title says "dropped threshold below 5.0"; the stored payload says
`actual_value: 0, threshold_value: 5, threshold_type: below`. The reader is told
a threshold was crossed and shown neither the number nor a chart of the series
that crossed it. `volumeChart_` is skipped entirely for this type because
`hashPrefix` has no query-alert prefix — but the alert *is* a KQL query we can
plot directly.

Also: `query_id` is persisted as `"QueryMonitorId {unQueryMonitorId = b408c446-…}"`
— a leaked `show` of the newtype. There is no link back to the monitor.

### F2 — Zero stack traces. *Severity: high.*
```
issue_type          n    with_stack  with_request_path
api_change         210       0            0
log_pattern        165       0            0
runtime_exception  151       0            0
query_alert         11       0            0
```
Every runtime exception in the demo project renders the "No stack trace
captured — common for browser console errors" empty state. These are Go backend
services, not browsers. The copy is wrong *and* the page's designated hero
section is empty on 100% of errors. What we do have and don't use:
`error_data.span_id`, `parent_span_id`, `trace_id`, `parent_hash`,
`root_error_type`, `root_error_message`.

### F3 — Counts are absent, and the ones we store are junk. *Severity: high.*
Sentry's first triage signal is *events* and *users affected*. We show neither.
`issues.affected_requests` / `affected_clients` are `1` on every row inspected
(placeholder), and `error_patterns.occurrences_1h` / `_24h` are `0` while the
issue is live. So the honest number lives only in the chart, which has no total.

### F4 — No attribute distribution. *Severity: high.*
Sentry's tag rail ("this happens on 1 of 4 hosts / only release 1.2.3 / 90% Chrome")
is what turns an error page into a diagnosis. We have the telemetry to compute it
and show none of it. Nothing on the page answers "is this everywhere or one pod?"

### F5 — `recommended_action` is boilerplate in the hero slot. *Severity: medium.*
"Review the changes and update your integration accordingly." /
"Review the query results and take appropriate action." — rendered as the
subtitle directly under the title, where Sentry puts the culprit (the
function/module that failed). Pure noise in the highest-value line on the page.

### F6 — `environment` is stored and never rendered. *Severity: medium.*
Empty in demo, but the column exists and every competitor treats env as a
first-class filter/badge. Same for `error_patterns.state`
(new/resolved/regressed) — we ship resolve/regress logic and never show state.

### F1b — The Investigation panel reserves 70vh of void it can never fill. *Severity: high.*
Screenshot of #112126 (`before-queryalert.png`): below the two-line query box the
page is **~1100px of empty space** containing the sentence "No trace data
available for this issue." `anomalyDetailCore` hardcodes `mTraceRef = pure
Nothing` for `QueryAlertP` and `LogPatternP` (Anomalies.hs:253-255) — we *know at
render time* the panel is empty, and we lay out `lg:h-[70vh]` anyway. A query
alert has no trace by construction; the Trace tab should not exist for it.

### F7 — Structural inconsistency. *Severity: medium.*
Four bespoke layouts. A reader who learns the error page has to relearn the
log-pattern page. Facts that are common to all issues (service, environment,
first/last seen, volume, state) appear in a "Details" card for two types, as
chips for one, and nowhere for the fourth.

## 3. Competitive research

Read from the vendors' own docs and annotated product screenshots, not from
memory. Artefacts saved in the session scratchpad.

### 3a. Sentry — from their own labelled breakdown diagram

Sentry annotates its five zones explicitly (`issue-details-breakdown`):

1. **Header** — breadcrumb `Issues / JAVASCRIPT-SHGH`; then
   **`TypeError`** (bold) + `'NoneType' object has no attribute 'split'`
   (regular weight, *same line*); then a level dot + **culprit**
   `sentry.tasks.process_commit_context`. Far right: **`Events 678`** and
   **`Users 320`** as the two largest numbers on the page.
2. **Workflow** — `Resolve ▾` `Archive ▾` 🔔 `⋯` | `Priority High ▾` `Assignee ▾`.
3. **Event Search & Filter** — `All Env. ▾` `14D ▾` `Filter events…`.
4. **Trends & Aggregates** — one band: `459 events` / `200 users` at left, a bar
   histogram in the middle, and an **Issue Tags** mini-panel at right showing a
   *single* tag's distribution as % bars (`Chrome 61%`, `Firefox 20%`,
   `+4 more 5%`).
5. **Event Details** — a **sample navigator** (`‹ ›  First | Last | Recommended |
   All Events`) and, below it, everything about *that one event*:
   event id + age, a `Jump to:` mini-TOC, a row of **Event Highlights** chips
   (user, browser, OS), then **Stack Trace** (`Most Relevant` / `Full Stack Trace`
   toggle; top frame expanded with real **source context**, erroring line
   gutter-highlighted; other frames one-line collapsed), then **Breadcrumbs**.
6. **Right rail** — Last seen / First seen each *with the release they occurred
   in*; Issue Tracking (Asana/GitHub/JIRA); Activity + comment box; People
   (participating / viewed); Similar issues; Grouping.

Sentry has **two distinct tag surfaces**: aggregate (`Issue Tags`, % bars, in the
trends band) and per-event (`Tags`, a two-column key/value table with dot-notation
shown as a tree, values linked, and a per-row `⋯` menu — "View other events with
this tag value" / "View issues with this tag value").

### 3b. Datadog Error Tracking — from `error-tracking-overview-3`

1. **Status pill** `FOR REVIEW ▾` — issue state is editable inline, top-left.
2. **Title = the exception FQN** `org.springframework.data.cassandra.CassandraInvalidQueryException`;
   **subtitle = the message**. Then chips: service `product-recommendation`
   (with a runtime icon) and an error-category chip `Code exception ▾`.
3. Tabs: **Review | Resolve | Activity**.
4. **AI verdict as one inline banner line** — "Bits AI Dev · Missing `purchases`
   table in Cassandra schema · Next Steps: Configuration change" + a
   `View Investigation` button. Not a chat drawer.
5. **Filter errors** input + time range `Past 1 Day` + refresh.
6. **Lifecycle card** — `First seen 1 year ago  a438054f → Last seen a few
   seconds ago  debb43ae` (**commit SHAs attached to both**), a large `108k`
   total beside a red bar histogram, then:
   - **Impact** — `191 users`, `5 views`
   - **Tags** — inline chips *with percentages*:
     `operation_name:repository.operation (100%)`, `env:prod (78%)`,
     `@version:07eb822f (52%)` + `View All`
7. **Right rail** — Ownership (Team, Assigned to), comment box.
8. **Error sample** — `★ Recommended ▾`, `Previous`/`Next`, `View All Errors`;
   sample timestamp + attribute chips (`env`, `version`, `resource`, `operation`);
   then a **tab strip over one evidence area**: `Stack Trace | Trace Waterfall |
   Events Timeline | Span Attributes | Container Tags | Host Tags | Grouping Info`.
   Stack trace has `Pretty`/`Raw` and **"Show 32 third-party frames"**.

### 3c. What both converge on

**Identity → Workflow → Aggregate → *one sampled event* → Evidence.**

- The aggregate band (**counts + histogram + tag distribution**) is a distinct,
  always-present zone. We have none of its three parts.
- **Events and Users are the largest numbers on the page.** We render no count.
- A **sample navigator** picks one event; everything below is about that event.
  We have this (`First` / `Recent`) but buried inside the Investigation card's
  tab strip, unlabelled as a sample selector.
- **Evidence is a tab strip over one area**, not scattered cards. Datadog puts
  seven tabs there; we have two (`Trace`, `Logs`) spread across the page with
  Stack Trace and Activity as separate sibling cards.
- Only the *evidence* varies by error source. The skeleton never does.
  We invert this: our skeleton varies by type and the evidence is fixed.

Things worth stealing that I would not have predicted:
- **Datadog's inline `key:value (%)` tag chips** are far cheaper to build than
  Sentry's bar list and answer the same question. This is the pattern for P4.
- **Release/commit attached to first-seen and last-seen** on both products.
- **Datadog's AI as a one-line verdict + CTA**, versus our hidden chat drawer.
- **"Show N third-party frames"** — frames are ranked, not just dumped.

## 4. Discriminating checks run before planning

| Question | Answer | Consequence |
|---|---|---|
| Do the spans carry `exception.stacktrace` we're dropping at ingest? | **No.** The exception span event for trace `04e7b47d…` carries only `{message, type}`. otel-demo's Go services genuinely never send stacks. | F2 is *not* an ingestion bug. The span-chain fallback **is** the fix. |
| Is the span chain rich enough to synthesise frames? | **Yes.** The errored span `83e2fb2cf72302a7` (`CheckoutService/PlaceOrder`, `status_code=ERROR`) has a real causal tree beneath it: `prepareOrderItemsAndShippingQuoteFromCart` → `CartService/GetCart` / `ProductCatalogService/GetProduct` / `CurrencyService/Convert`. | A cross-service call stack, frame-shaped, already in TF. |
| Does `Widget` support a threshold line? | **Yes, already.** `alertThreshold`, `warningThreshold`, `showThresholdLines`, `alertId`, `alertStatus` (Widget.hs:214-218). | P2 needs no new widget capability. |
| Is the boilerplate a constant we can compare against? | **Partly.** `Issues.defaultRecommendedAction` is suppressed; the query-alert string at Issues.hs:873 is a *second* hardcoded literal that isn't. | P6 is a two-line fix. |
| Is there breadcrumb-grade context in the trace? | **Yes** — the same trace carries `feature_flag.evaluation` span events (`productCatalogFailure`, `reason: targeting_match`). We already have `breadcrumbsFromSpanEvents`. | Sentry's beta Feature Flags section is directly buildable. |

## 5. Plan

The unifying move, taken straight from 3c: **stop branching the whole page on
issue type; branch only the evidence.** Everything above the evidence becomes one
shared band every type fills in.

Ordered by (reader value ÷ effort):

- **P1 — Fix QueryAlert (F1, F1b).** Render `thresholdValue` / `actualValue` /
  `thresholdType` as a stat pair; plot the alert's own KQL via `volumeChart_`
  with `alertThreshold` + `showThresholdLines` populated; link back to the
  monitor; and **suppress the Trace tab for types that can never have one**.
  *Verify:* integration test asserting the page contains the threshold, the
  actual value, a monitor link, and **not** the string "No trace data available".
- **P2 — Shared aggregate band (F3, F6, F7).** One fact row every type fills:
  state · service · environment · first seen · last seen · **total events over
  the selected range**. The count comes from the same hash query the chart runs,
  as a lazy HTMX fragment, so number and chart cannot disagree.
  *Verify:* one test rendering all four issue types and asserting the row exists
  in each.
- **P3 — Attribute distribution (F4).** Datadog's pattern, not Sentry's:
  inline `key:value (%)` chips over the issue's hash filter, bounded window,
  `LIMIT`, lazy fragment (this page already has the >56s/504 scar tissue —
  Anomalies.hs:257).
  *Verify:* test that the fragment endpoint returns chips for a known hash.
- **P4 — Span-chain stack trace (F2).** When `stackTrace` is empty, render the
  span subtree under the errored span as frames (service · operation · duration ·
  status), innermost first, and **fix the empty-state copy** that currently
  blames browsers for Go errors.
  *Verify:* test that a stackless runtime exception renders span frames.
- **P5 — Boilerplate suppression (F5).** Suppress both default strings; use the
  freed line for the culprit (`service · operation`).
  *Verify:* doctest on the predicate.

### Decisions forced by review

- **The monitor link needs a tolerant read now.** `query_id` is persisted as a
  `show` blob; P1 regexes the UUID out at render time. Writing new issues with a
  clean UUID is a two-line bonus; the backfill migration stays deferred.
- **The aggregate band's impact slot is per-type, never empty.** `hashPrefix` is
  `Nothing` for QueryAlert — the same hole that produced F1b. So:
  RuntimeException / LogPattern / ApiChange → total events over the range;
  **QueryAlert → the threshold-vs-actual stat pair.** The band never renders an
  empty cell, because the slot is a sum type, not a nullable count.
- **Try the widget before building a count endpoint.** `Widget` already carries
  `timeseriesStatAggregate` and `WidgetDataset.rowsCount`; if un-hiding the value
  gives the total, P2 needs no new fragment.
- **P4 carries P3's 504 risk.** The span-subtree query gets the same treatment as
  `traceRef`: ±5min around the known error timestamp, `LIMIT`ed, lazy fragment.

Deferred, and why: per-event Tags table (P3 covers the higher-value aggregate
first); release/commit on first-seen (no release tracking wired yet);
`query_id` `show`-blob repair (needs a migration + tolerant read — worth doing,
but it is a data bug, not a page design one).
