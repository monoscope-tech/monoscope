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

## 6. Shipped — P1 (commit `ff8d94ee2`)

Query alert #112126, before → after:

| | before | after |
|---|---|---|
| Threshold / actual | not rendered | `↓ 0` vs `THRESHOLD (BELOW) 5` |
| Chart | none | the alert's own KQL, threshold line drawn |
| Monitor link | none | `View monitor`, id recovered from the `show` blob |
| Subtitle | "Review the query results and take appropriate action." | suppressed |
| Below the fold | ~1100px of "No trace data available" | nothing |

Verified: page renders the threshold, the actual, and the monitor link, and no
longer contains "No trace data available"; `alertThreshold: 5.0` reaches the
widget config, where `widgets.ts` turns it into a markLine. Log-pattern and both
runtime-exception pages render byte-identically to before (`INVESTIGATION`,
`Stack Trace`, `Error Details`, `Pattern Volume` all still present).

### Critique of the shipped result — what a second pass found

1. **Fixed:** the `Monitor: <name>` row overflowed its card (`detailRow_` sets
   `whitespace-nowrap`). It also just repeated the page title, so it is gone.
2. **Fixed:** `ACTUAL 0` signalled "this is the bad number" with amber alone.
   Now carries a direction arrow — the second signal design principle 3 requires.
3. **Fixed:** the `bolt` icon used for "Triggered" existed in neither sprite
   sheet, so it silently rendered nothing. Added via `make fa-add`.
4. **Fixed (commit `dcfa1b58d`) — the chart could not show the crossing.** The
   series ran 2000–3000 against a threshold of 5, so the threshold line sat on
   the axis and the breach was a sliver at the right edge. Fixed by option (a):
   a query alert now defaults its window to ±2h around `triggeredAt` rather
   than `defaultSinceRange`'s age heuristic. The URL seeding had to follow — it
   wrote `since` unconditionally, which would have overridden the absolute
   from/to. **Result: y-axis 3000 → 250, and the drop to zero now occupies half
   the chart.** Options (b) `highlightFrom`/`highlightTo` and (c) log y-axis
   were not needed; (b) is still the right move if a trigger marker is wanted,
   but its dashed-line degradation needs `timeFrom`/`timeTo` set, which fights
   the page's time picker.
5. **Still open:** the Activity card spends ~160px to say "No activity yet."
6. **Verified in both themes.** Light-mode parity checked by rendering with a
   `theme=light` cookie; the amber `↓ 0`, the ink `5`, and the brand-blue
   monitor link all hold up. Headless recipe for future passes:
   `curl -H 'Cookie: theme=light' <url> | sed 's|<head>|<head><base href="http://localhost:8080/">|'`
   then screenshot the file — the server defaults anonymous sessions to dark,
   so a plain headless run only ever shows one theme.

## 6b. Shipped — the stack-trace empty state (commit `52f86472a`)

F2's copy half. 151 of 151 runtime exceptions in the demo project hit this empty
state, and it read *"No stack trace captured — common for browser console
errors. Check the User Journey for the events that led up to it."* — wrong for
readers whose errors are Go. **Correction:** the commit message for `52f86472a`
also claims the "User Journey" pointer was dangling. That is wrong — previewing
the page in a browser shows an explicit `USER JOURNEY · 62 events before error`
section in the Activity panel. Only the browser claim was false. The new copy
still stands (it names the runtime and points at the Trace tab), but the second
half of that commit message's rationale does not. It now names the runtime that
stayed silent and points at
evidence the page has: the Trace tab when a trace was captured, the Logs tab
when it wasn't. Guarded by a spec asserting the Go wording and the absence of
the browser claim.

Synthesising frames from the span chain (P4's other half) is still open. The
data is confirmed present: the errored span's subtree is a real cross-service
call path (`CheckoutService/PlaceOrder` → `prepareOrderItemsAndShippingQuote…`
→ `CartService/GetCart` / `ProductCatalogService/GetProduct`).

## 6c. Shipped — P2's impact count, and F8 (commit `143879427`)

### F8 — the Error Frequency chart timed out on every runtime exception. *Severity: highest.* *(found by previewing in a browser)*

Not in the original findings, because it is invisible from the HTML — the chart
fetches its own data. `hashes[*]==` lowers to
`jsonb_path_exists(to_jsonb(hashes), '$[*] ? (@ == "err:…")')`, whose cost tracks
the window:

| window | result |
|---|---|
| 24H | ~3s |
| 3D | **60s timeout**, `{"error": "Query timed out"}` |

`defaultSinceRange` returned `3D`/`7D`/`14D` for anything older than a day — so
every runtime exception past its first day rendered "Query timed out" where its
chart should be. Capping alone would not fix it: the reference issue last fired
38h ago, so a now-anchored 24H window answers fast and finds *nothing*. The
reader got a timeout or an empty chart, never the data.

Fixed by anchoring the default window on the issue's **last activity** rather
than on `now`, and capping it at a day. Verified in the browser: the chart now
renders a bar where it previously showed the timeout banner.

*(An apparent anomaly during this investigation — narrow windows returning 0 rows
via `/chart_data` while TimeFusion returned 1 directly — is `Pkg.QueryCache`,
which keys on the query and not the range, being poisoned by my own probing. Not
a product bug; noted so the next person doesn't chase it.)*

### P2's count — by extending the widget, not adding a fragment

§8 previously concluded this needed its own lazy fragment. That was the wrong
call and the repo's conventions say so: *extend the shared component; never build
a one-off beside it.* Re-reading the client showed the total already exists —
`setStatValue` runs on **every** chart-data fetch and fills `#<widget id>Value`
with `statScalar`, which defaults to the range sum. The page simply never
rendered that element, because `naked` suppresses the widget's whole header.

So the fix is a consolidation, not an addition: `widgetValueSlot_` is extracted
out of `renderWidgetHeader` and exported, so the header and any caller supplying
its own header around a `naked` chart share **one** definition and show the
**same** number. No new endpoint, no new query, no second total that could
disagree with the chart. `ERROR FREQUENCY` now carries its count badge.

### F9 — a log-pattern issue can reference a hash no telemetry carries. *Severity: medium. OPEN.* *(found by previewing in a browser)*

Issue #112128 says `14 occurrences`, `first seen 1 day ago`, and renders "No data
in this time range". Its chart is not at fault, and neither is F8's window
change — `pat:f07ab941` appears in **zero** rows of `hashes`, at every width
tried, including a 3h window centred on the issue's own `first_seen_at`
(`2026-09-02T10:04:54Z`). Other `pat:` hashes are present in the same period
(the reference error's span carries `{ad9cfe83, err:e03848c6, pat:634f4c70}`), so
this is not a missing-prefix or missing-column problem.

So the count in the chips (`14 occurrences`, read from `issue_data`) and the
chart (read from telemetry) disagree, and the page presents both without comment.
Not diagnosed further here: it is an ingestion/retention question about how a
pattern hash is written to `hashes`, not a page-design one. Worth chasing,
because "the issue says 14, the chart says none" costs the reader trust in both.

## 7. State of verification

- **`:8080` serving stale code — FIXED.** Root cause was in `build.log` all
  along: `runSettings threw: Network.Socket.bind: resource busy (Address already
  in use)`, then *"a service fiber died … shutting down"*. Two `cabal repl
  monoscope` ghcids were racing for the port; the loser's server died silently
  while its compiler kept reporting "All good", so the pane looked healthy and
  served someone else's build. By the time it was diagnosed both had exited and
  nothing was listening. Restarted with a fresh pinned pane
  (`tmux split-window … 'make live-reload | tee build.log'`; `make live-reload`
  runs `kill-live-reload`, which frees the port by process group). Now exactly
  one listener, and it is ours.
  **Keep gating screenshots on `curl <url> | grep <string-unique-to-your-change>`**
  — that is what caught this, and it is cheaper than diagnosing the topology.
- **The integration target does not compile in the working tree**, because
  `test/integration/EndpointDiscoverySpec.hs:436` (another session's
  *uncommitted* work) references an out-of-scope `env` field. Because it is
  uncommitted, **a detached worktree at HEAD compiles and runs fine** — that is
  the way around it, and it is how everything below was finally verified:

  ```bash
  git worktree add --detach <dir> HEAD
  cp -Rc dist-newstyle/src <dir>/dist-newstyle/src        # warm the build cache
  cp -R static/public/assets/web-components/dist <dir>/…  # BodyWrapper TH-splices its manifest
  cp .env cabal.project.local <dir>/
  cd <dir> && LOG_LEVEL=attention USE_EXTERNAL_DB=true \
    cabal test integration-tests --ghc-options=-O0 --test-show-details=direct \
    --test-options='--match=Anomaly'     # NB: the match value must contain no spaces
  ```

  Doing this immediately caught a real defect: the stackless example had been
  inserted into the *middle* of the slow-trace example, orphaning its last ten
  assertions onto the new one where `traceIdText` is out of scope. Fixed in
  `b3caa1cbd`. Nothing else would have caught it before CI.

What each change actually rests on — all verified before pushing:

| commit | compile | runtime | spec |
|---|---|---|---|
| `ff8d94ee2` query-alert page | ✓ | ✓ curl + screenshot | ✓ passes |
| `dcfa1b58d` chart window | ✓ | ✓ screenshot (y-axis 3000→250) | ✓ passes |
| `52f86472a` stack-trace copy | ✓ | ✓ browser (once `:8080` was fixed) | ✓ passes |
| `143879427` count + F8 window | ✓ | ✓ browser: chart renders, count badge shows | ✓ covered |

Final gate before the push: **26 examples, 0 failures** in `Pages.AnomaliesSpec`,
and **doctests 1456 tried, 0 failures**.

### Master was already red, and the deploy is gated on tests

The two cross-tenant examples added upstream in `f49798eaa` / `0b42a201e` fail on
a clean `origin/master` — baselined there before blaming the merge. Their
fixtures miss two schema constraints: `error_patterns.stacktrace` is NOT NULL and
neither INSERT supplied it, and `canonical_id` is a self-referencing FK pointing
at a UUID with no row. Fixed in `6320d2444` without changing what either test
asserts, because the deploy could not ship past them.

## 8. Remaining plan

P2's count shipped (§6c). **An earlier conclusion here was wrong and is worth
recording as a lesson:** I read `Widget.value` being server-only and concluded
"the widget cannot do this, build a fragment." The right move was to read one
layer further — `widgets.ts` already computes and formats the number — and the
gap was a *rendering* one, not a data one. Reaching for a new surface because a
shared component doesn't do something yet is how a codebase grows two of
everything. Establish that the component genuinely cannot be extended first.

Still open from §5:

- **P2's remaining half — the shared fact row.** State · service · environment ·
  first seen · last seen, filled by every type, so a reader who learns one issue
  page has learned all four. The count now has a home; these facts still appear
  as a "Details" card for two types, chips for one, and nowhere for the fourth.
- **P3 — attribute distribution.** Datadog's inline `key:value (%)` chips.
- **P4's other half — span-chain frames** for stackless exceptions.

Also worth doing, found while working and small: the Activity card spends ~160px
to render "No activity yet." The product register's rule is that empty states
teach the interface rather than announce emptiness.

## 9. Options for the remaining work — thinking out loud before building

Written before any of this is implemented, so the alternatives are on record
rather than reverse-engineered from whatever shipped. Grounded in §3's research
and in what the codebase already provides. Reviewed by the advisor before
implementation.

### A. The shared identity/impact band (P2's remaining half)

Both vendors put the same five facts in a fixed place: state, service,
environment, first seen, last seen. We scatter them — an "Error Details" card for
runtime exceptions, an "Endpoint Details" card for API changes, chips for log
patterns, nothing for query alerts.

1. **Extend the existing chip row.** Cheapest — the chips already exist under the
   title. But that row is currently *type-specific detail* (log level, occurrence
   count, change direction). Folding identity into it blurs "what is this issue"
   with "what is peculiar about this issue type", and the row is already long.
2. **A dedicated fact row under the title, rendered for every type.** Mirrors
   Sentry's Header→Workflow→Aggregate zoning. Requires the per-type panels to
   *give up* the four facts they currently duplicate, otherwise the page says
   "first seen" twice. That giving-up is the actual work, and the actual win.
3. **Keep per-type panels, standardise their contents.** Least disruptive, but it
   preserves the thing that makes the four pages feel unrelated, which is the
   whole point of §3c. Rejected.

**Leaning to 2.** The `detailRow_` helper already renders exactly this shape, so
the new code is a call site plus deletions from three branches. Net negative
lines, which is the right smell. Type-specific panels keep only what is genuinely
type-specific (method/path and runtime for errors; host and request count for
endpoints).

### B. Attribute distribution (P3) — the one I am least sure about

Datadog's `operation_name:… (100%)  env:prod (78%)  @version:… (52%)` is the
single highest-value thing we lack: it answers "is this everywhere or one pod?"

What already exists, and what it does not do:
- `SchemaCatalog.getFacetSummary pid "otel_logs_and_spans" from to` is already
  called in this very module (for the AI prompt), and `renderFacets` is an
  exported component in `Pages.LogExplorer.Log`. Tempting to reuse wholesale.
- **But it is project-wide over a window, not scoped to one issue's hash.** It
  answers "what does this project look like", not "what does *this error* look
  like". Reusing it would put a confidently-rendered, subtly-wrong panel on the
  page — worse than no panel.

So the options are:
1. **Hash-scoped aggregation, new query, reuse `FacetValue`/`FacetSummary` types
   and the `renderFacets` shape.** Correct answer, honest data.
   **Feasibility concern, and it is serious:** F8 just established that
   `hashes[*]==` is an unindexed array scan whose cost tracks the window. A
   `GROUP BY` over several attribute columns *filtered by that same predicate*
   is strictly more expensive than the count that already takes ~3s at 24H. This
   could easily be the next "Query timed out".
2. **Derive it from the trace we already fetch**, rather than a new aggregate —
   one trace's attributes, labelled honestly as one sample, not a distribution.
   Cheap and safe, but it is not the Datadog feature; percentages over n=1 are
   noise, and implying otherwise is worse than omitting it.
3. **Defer P3 until the predicate is cheaper.** Unsatisfying, but F8 is a live
   demonstration that shipping an expensive per-issue aggregate on this page is
   how the chart broke in the first place.

**Leaning to 1, but measure first and be willing to land on 3.** Concretely:
before writing any UI, time a hash-scoped `GROUP BY` over 3–4 attribute columns
at the page's default 24H window. If it is materially slower than the count, take
option 3 and say so in the doc. **Do not ship a second timeout onto this page.**

### C. Span-chain frames for stackless exceptions (P4's other half)

I proposed this in §5 and I now think it is **probably wrong**, which is worth
writing down before building it.

The argument for: the errored span's subtree is a real cross-service call path
(`CheckoutService/PlaceOrder` → `prepareOrderItemsAndShippingQuote…` →
`CartService/GetCart`), and it is frame-shaped.

The argument against, which I find stronger: **the page already renders that.**
The Investigation panel's Trace tab is a waterfall of exactly those spans, and
the Activity panel already shows `USER JOURNEY · 62 events before error`.
Building a third rendering of the same span data would be a new surface that
duplicates two existing ones — the precise mistake §8 records me making with the
count. Sentry's stack trace earns its place because it shows *source context and
in-app frames*, which we do not have and cannot synthesise from spans.

**Leaning to: do not build it.** Instead, make the existing evidence easier to
reach — the empty state now names the runtime and points at the Trace tab
(`52f86472a`), which may already be the whole fix. If anything more is wanted,
the cheap version is scrolling/deep-linking the Trace tab to the errored span
rather than rendering a parallel frame list.

### Budget for B, fixed before measuring

Committed ahead of the numbers so the decision is not negotiated with the result:

- Columns: `resource___service___name`, `resource___environment` (or its
  equivalent), `status_code`, `resource___service___version` — bounded
  cardinality only, never arbitrary attributes.
- **If the set of GROUP BYs exceeds ~3s at the page's default 24H window, defer
  P3** and record it. ~3s is what the count already costs at that window, and
  this page has the 504 scar twice over.
- **Coverage counts as much as cost.** If the columns are largely NULL in real
  telemetry, a fast query still renders an empty panel. Container/resource
  attributes are known to vary by receiver, so measure distinct-value counts in
  the same probe.
- Whatever the outcome, it ships as a lazy fragment, never inline.
- Perturb the query between timings — `Pkg.QueryCache` keys on the query and not
  the range, which already produced one false "0 rows in 1s" reading today.

### Decision on C: not building it (closed)

`traceH` already takes a `span_id` query param and explicitly supports finding a
span by id for nav ("Span lookup is not span rendering… nav has to FIND one span
by id"). `traceFragmentUrl` simply never passes one. So the whole defensible
remainder of C is **threading the errored span's id into the existing trace
fragment** so the Trace tab opens focused on the failure, instead of rendering a
third view of span data the waterfall and User Journey already show. That is an
argument for one extra parameter, not a new surface.

### Outcomes — what the measurements said, and what shipped

Recorded against the options above, which were committed (`051a884eb`) before any
of this was built.

**A — shipped as option 2 (`c4c3b421c`).** `issueFactRow_` renders service,
environment and first/last seen once, under the badges, for all four types; the
two panels gave up what they duplicated. Two things the review added that I had
not planned: omit absent cells rather than printing "Unknown service", and leave
issue *state* out of the row because `issueStatusStrip_` already owns it and a
second signal could disagree with it. Deleting the duplicates left "Error
Details" as a titled card around one line, so the runtime became a chip and the
card now renders only when there is request context. Verified: the row appears
exactly once on every type, and 26 examples still pass.

**B — deferred, on the measurement, as the budget said it would be.** Coverage
was fine (`service` 100%, `version` 95%, `status_code` 70% over a sample window),
so this is purely cost. A *single* hash-scoped `GROUP BY` at the page's then-
default 24H window took **48s** against a ~3s budget — 16x over, and that is one
column, not the three or four a distribution panel needs. **P3 stays unbuilt
until the predicate is cheaper**; the honest version of this feature needs an
index or a materialised per-issue rollup, not a UI change. Shipping it as-is
would have put a second timeout on the page we had just finished un-breaking.

**C — closed as not-to-build, and the useful remainder shipped instead
(`0ddf925b0`).** The waterfall and User Journey already render this span data;
a third view would have been the same mistake §8 records. What was genuinely
missing was that `traceFragmentUrl` never passed a span id even though `traceH`
resolves one for nav — so the Trace tab now opens focused on the failing span
instead of the trace root.

**And the measurement caught that F8 was only half-fixed.** The same probe showed
the *chart's own* count still cost ~48s at the 24H default — no longer an error
banner, but not a chart anyone waits for either. The width table
(4h 3.2s / 8h 10.3s / 24h 47.8s / 3d timeout) put the bracket at ±2h, which is
where the query alert already sat. That is the second time today the fix for
this page came from measuring rather than reasoning about it.

## 10. Second critique pass — the pages as they now stand

Re-screenshotted all four after A/B/C, because critiquing the versions I started
from would be critiquing work already replaced.

The header zone is now doing its job: `CRITICAL · Log Pattern · ERROR · 14
occurrences` then `First seen · Last seen · Service`, identical in shape across
types. That part is settled. What the screenshots surface is a different problem,
and it is the same one as F1b wearing a new hat.

### F10 — the page reserves space for evidence it does not have. *Severity: high.*

Log pattern #112128, full height, contains **three separate "nothing here" boxes
and ~900px of void**:

| region | says | height |
|---|---|---|
| Pattern Volume | "No data in this time range" | ~150px |
| Activity | "No activity yet." | ~150px |
| Investigation → Logs | "No events match in the selected time range" | `lg:h-[70vh]` |
| …and its details pane | nothing at all | 50% width, blank |

The header asserts `14 occurrences` directly above a chart that says none. That
contradiction is F9's data bug, but the *layout* amplifies it: a reader scrolls
a full screen of apologies to confirm the page has nothing to tell them.

**The details pane is a straightforward divergence, not a judgement call.** The
log explorer's own panel starts collapsed — `w-0 max-w-0 overflow-hidden`,
expanding when content arrives (`Log.hs:1797`). The issue page copied the panel
and hardcoded `lg:w-1/2`, so it permanently reserves half the Investigation area
for a detail nobody has selected yet. Same component, one call site kept the
behaviour and one dropped it — the same class of thing as the count in §8.

Options:

1. **Collapse the details pane to match the explorer, and let the Investigation
   panel size to its content instead of `lg:h-[70vh]`.** Fixes the void without
   deciding anything about empty states. Smallest, most obviously correct.
2. **Additionally, suppress the whole Investigation panel when the issue has
   neither a trace nor any matching logs.** Stronger, but it needs a *count*
   before render to know it is empty, and that is the expensive hash query
   again — exactly what B just measured at 48s. Rejected on cost.
3. **Collapse the three empty states into one honest statement** ("No telemetry
   found for this pattern in this window") with the one useful action (widen the
   range / open in Explorer). Better writing, and the product register asks empty
   states to teach rather than announce — but it is a bigger change and partly
   duplicates what F9 should fix at the source.

**Leaning to 1 now, and 3 written up as a follow-up.** 1 is a CSS-level fix
restoring behaviour that already exists one module over; 2 costs a timeout; 3 is
real work that I would rather do after F9 is understood, because if the data
were there none of those boxes would be empty in the first place.

### F10 outcome — shipped as option 1 (`edfa0271a`)

The details pane now starts at inline `width:0` and the resizer hidden, matching
what the log explorer's own panel does and what the web component already
expects: `toggleLogRow` opens the pane with `if (width < 50) sideView.style.width
= '550px'` and removes exactly `hidden`, `opacity-0`, `pointer-events-none`. The
call site was the odd one out, not the component.

`lg:h-[70vh]` deliberately stays. `virtualTable` renders `<log-list>` with
`h-full`, so an auto-height parent collapses it to zero — that trade turns half a
blank pane into a wholly blank tab. Option 3 (merging the three empty states)
stays deferred behind F9, because if the data were there none of them would be
empty.

**The pattern, now three for three.** This page has taken a shared component and
dropped a behaviour it already had, three times: the widget's value slot (§8),
the trace fragment's span id (§9 C), and now the details pane's collapsed initial
state. Each looked like a missing feature and was actually a missing line at one
call site. That is the argument for the CLAUDE.md rule, and it is where to look
for the fourth instance.

### F11 — the Logs tab hangs on "Loading events…" when the result is empty. *Severity: high. OPEN, not ours.*

Found while verifying F10, and **A/B tested against my own change first**: with
the collapse disabled the symptom is identical, so it is not F10's doing.

Evidence:
- The endpoint is healthy — `count: 0`, `hasMore: false`, well-formed, **0.52s**.
- The same `<log-list>` component renders **107 rows fine in the log explorer**,
  so the component is not broken in general.
- The issue page's tab with **0 rows** sits on "Loading events…" indefinitely
  (still there at a 40s virtual-time budget) instead of falling through to the
  "No events match in the selected time range" state it used to show.

So the empty-result path specifically fails to resolve its loading state. This
lives in `web-components/src/log-list.ts` (and possibly the recent `Log.hs` /
`LogQueries.hs` changes), which another session is actively editing — flagged
rather than fixed.

### F12 — the runtime-exception Logs tab costs ~47s. *Severity: high. OPEN.*

The trace-scoped query `kind=="log" AND context___trace_id=="…"` over the page's
window returns 14 rows in **46.7s**. `virtualTable` fetches on connect via
`initialFetchUrl`, not on tab activation, so **every** runtime-exception page load
fires this in the background even though Trace is the default tab.

Same family as F8 (an unindexed predicate whose cost tracks the window) and the
same shape of fix — but the earlier `+/-5min` narrowing already applies here, so
this one needs the query looked at rather than the window. `build.log` shows this
exact statement timing out and being retried by `retryTransientEff`, so it is
already visible in production telemetry.

## 11. `/impeccable critique` — 23/40, and what it changed

Full snapshot: `.impeccable/critique/2026-09-04T07-57-46Z__src-pages-anomalies-hs.md`.
Run dual-agent (independent design review + deterministic detector/browser
evidence). Verdict: **not AI slop — the chrome is 3s and the content semantics
are 1s and 2s.** Deterministic scan came back genuinely clean: 3 findings, all
one rule, all false positives traced to a line-level heuristic tripping on a
focus-ring radius. Accessibility measured better than expected — 0 unnamed
buttons or links, no positive `tabindex`, and 0 duplicate `class_` attributes.

Shipped from the backlog (`028550d95`, `8bd69ac47`, `fcf9a0396`):

- **P0 — an optional lookup took the whole page down.** The session-id query
  behind Session Replay was unguarded, so a transient TF error returned a bare
  500 for the entire issue — failing exactly when TF is degraded, which is when
  someone is reading about an incident. All three enrichment lookups now degrade
  and log. *Observed twice live before it was fixed.*
- **P1 — issue state was invisible here.** `issueStateBadge_` (REGRESSED /
  RESOLVED / REOPENED / ACK EXPIRED) was rendered only by the list card.
  **This corrects §9A, which claimed `issueStatusStrip_` already owned state —
  it does not**, it guards `archivedAt`/`acknowledgedUntil` and nothing else. The
  strip also short-circuited, so an archived+acknowledged issue showed one banner
  while the action bar offered to un-acknowledge a state the page never mentioned.
- **P1 — the heading outline was inverted.** `h1` was the breadcrumb number, the
  issue title an `h3`, and on three of four types the only `h2` was an
  empty-state string — a screen reader heard "No stack trace in this event"
  outrank the incident. Now `h1` (shell) → `h2` (issue) → `h3` (evidence), and
  every `detailCard_` title has a heading role it never had.
- **P1 — the count was the smallest thing in its row**, contradicting the very
  research this page was built from. Now a labelled `EVENTS` figure at headline
  weight, Datadog-style.
- **P2 — red was decoration.** Verified: the query-alert series fell through to
  `colors[hashString(name) % n]`, the *same* hash as an error-volume chart, so
  healthy checkout throughput rendered in error-red. Lone aggregates now use the
  brand colour; grouped series keep the hash, which is what the hash is for.

Guarded by a new spec (27 examples, 0 failures) asserting the outline, the fact
row, and the events figure.

**Also relabelled** the log-pattern chip to "N all time", because an unqualified
"14 occurrences" beside a chart reading "No data in this time range" reads as the
page contradicting itself rather than as two true numbers at different scopes.

### F13 — the trace id and its timestamp were not a matched pair. *Severity: highest this round. Partly fixed (`356968f5e`).*

Found while fixing F12, and much worse than F12 itself.

Every trace lookup on this page pins a ±5min window to the timestamp carried
next to the trace id, so the two must agree. They did not: `traceRef` paired
`recent_trace_id` with `updated_at`, and `updated_at` is bumped by activity that
does not set a new trace id. On the reference issue:

```
recent_trace_id  04e7b47d…   (a trace from 2026-09-02 08:04)
updated_at       2026-09-04 00:15
rows for that trace within ±15min of updated_at:  0
```

So the waterfall — the page's primary evidence when there is no stack trace, and
the thing the stackless empty state now explicitly sends readers to — was looking
in a window the trace is nowhere near. **This is what produced the "Couldn't load
this trace / A slow or unavailable read timed out" state** that Assessment B
caught in the browser and that source review alone had missed.

Fixed exactly where it can be: when the recent trace *is* the first trace (the
common case, and true here), `created_at` is the timestamp that genuinely goes
with it. When they differ, the old timestamp is kept — no worse than before.

**Closed (`a22e26fe9`).** Root cause: `recent_trace_id` is deliberately throttled
to at most one write per five minutes per pattern (rewriting it per occurrence
breaks HOT and bloats heap/TOAST on the hottest path in the product), while
`updated_at` is written on every occurrence — so the pair drifts apart *by
design*, and no reader could have paired them correctly. Migration 0145 adds
`recent_trace_at`, written in the same CASE branch as the id so they cannot
diverge again; nullable with no backfill, since the value is only knowable at
write time. Read via a dedicated scalar rather than widening `ErrorPattern`,
which is decoded positionally from explicit column lists in several queries.

### F12 — fixed (`356968f5e`)

The Logs tab scoped a *trace-scoped* query to the *page's* range. Measured on the
reference issue: **35.9s → 0.49s for the same 14 rows.** `virtualTable` fetches on
connect rather than on tab activation, so every runtime-exception page load paid
that cost even though Trace is the default tab. The fallback branch already
narrowed to ±5min and documented why; the trace branch never did.

## 12. The stack trace — the claim I made and then checked

I closed the last round saying Sentry's stack trace "earns its place through source
context and in-app frames, which we can't synthesise from spans". Half of that was
an untested assumption, and checking it changed the answer.

**Ingestion is fine.** `Telemetry.extractATError` reads the OTel-canonical
`event_attributes.exception.stacktrace`, with `extractATErrorFromRecord` as the
span-attribute fallback, and `OtlpServer` normalises `error.stack` /
`error.stacktrace` into that namespace at the edge. Nothing is dropped. The demo's
Go services genuinely send `{message, type}` and no frames — which is an SDK fact,
not a product defect.

**Presentation was the actual gap, and the parser already existed.**
`Pkg.ErrorFingerprint.parseStackTrace` parses frames per language (Go, JS, Python,
Java), fills `filePath` / `functionName` / `lineNumber` / `contextLine`, and marks
`isInApp` — and it is what `computeErrorHashes` is computed from. The page called
none of it and rendered a `<pre>` blob. **That is the fourth instance of the
pattern this doc has been tracking**, after the widget value slot, the trace
fragment's span id, and the details-pane collapse: the capability existed and one
call site didn't use it. §10 asked where the fourth would be; it was here.

Shipped (`6d247257b`): frames render Sentry-style — your code by function and
`file:line`, the runtime's folded behind "Show N runtime frames", ordering
preserved (the order is the call path; grouping runtime frames to the bottom would
misreport what called what), `contextLine` rendered when an SDK starts sending it,
raw text one disclosure away, and a `<pre>` fallback when the parser recognises
nothing. No demo error has a stack trace, so a spec with a real Java trace is the
only thing exercising it.

**And source context turned out to be ours too (`9db18970a`).** I twice called it
un-closable — "SDKs upload source maps and we have nothing to resolve against".
`Pages.CodeContext` exists, is documented as *"the source behind one stack
frame"*, resolves file+line to real source through the project's Git integration,
caches the blobs, and is wired to `GET /p/:pid/code_context`. Nothing had ever
called it but the route table. Each frame with a file and line now requests it
lazily — `intersect once` on a div inside a *closed* `<details>`, so a 40-frame
trace issues no reads until a frame is opened.

**That is the fifth instance of the pattern**, after the widget value slot, the
trace fragment's span id, the details-pane collapse, and the frame parser itself.
The lesson is now unambiguous: on this page, "we can't do that" has been wrong
five times out of five, and each time the capability was already in the repo.

### F14 — `dashboard-widget-types` e2e is flaky at roughly 50%, and it gates every deploy. *OPEN, not this page.*

Two of four deploys of this work failed on the *same* test —
`dashboard-widget-types.spec.ts:158 › a failed chart shows a local error and a
successful refresh clears it` — and the proof it is not a code defect is exact:

| run | code | result |
|---|---|---|
| `8c9501325` | includes both chart-colour commits | ✅ |
| `d8c87c4fb` | + one Lucid-only commit (no TS) | ❌ |
| `e361d766f` | stack frames | ❌ |
| `b66d526a1` | **identical code, +36 lines of markdown** | ✅ |

Identical code, opposite outcomes, so it is intermittent rather than caused by
any of it.

**Likely mechanism**, from reading the test against `widgets.ts`: the test
installs its `**/chart_data?**` failure route *after* `openDashboard`, but a
widget prefetches its data on load (`chartDataPrefetch` / `takePrefetched`). If
the mocked request is superseded, `showChartError` is skipped — it is guarded by
`if (!isStale())` — so the banner never appears and `toBeVisible` fails exactly
as observed. Waiting for the initial load to settle (`networkidle`, or awaiting
the first successful `chart_data`) before installing the route should make it
deterministic.

Not fixed here: an e2e fix cannot be verified locally (`scripts/e2e.sh` builds
nothing — a stale binary has invalidated bisects before), and blind-patching
another team's test is worse than handing over the diagnosis. But at ~50% on a
gate that blocks deploys, it is worth someone's morning.

### Still open after this pass

- **F9** — a log-pattern issue referencing a hash no telemetry carries (data).
- **F11** — the Logs tab hangs on "Loading events…" when the result is empty.
- The critique's own open questions, which I think are the strongest remaining
  design leads: whether a QueryAlert belongs on this page at all, and why the AI
  is still a hidden chat drawer when §3b recorded the opposite as Datadog's win.

### What I want reviewed before building

1. Is B's cost concern sufficient reason to measure-then-maybe-defer, or is there
   a cheaper shape for per-issue distribution I am not seeing?
2. Is C's "we already render this twice" argument right, or does a frames view
   genuinely read differently enough from a waterfall to earn its keep?
3. A is mostly deletion — any reason not to just do it?

Deferred, and why: per-event Tags table (P3 covers the higher-value aggregate
first); release/commit on first-seen (no release tracking wired yet);
`query_id` `show`-blob repair (needs a migration + tolerant read — worth doing,
but it is a data bug, not a page design one).

## 13. September 6 reference-page inspection

The inspection covered all seven URLs in the current brief on production.
All seven returned HTTP 200 without browser JavaScript errors during the observation period.
Main-document response times ranged from 217ms to 958ms. These measurements exclude deferred evidence requests.
Desktop pages had no document-level horizontal overflow. The mobile log-pattern page also had no document-level overflow.

The browser report is `/private/tmp/issue-details-inspection.json`.
Screenshots use `/private/tmp/issue-<first eight ID characters>.png`.
These are temporary local artifacts, not repository fixtures.

### Current fixes, validated locally

- Both issue routes discarded absolute `from` and `to` parameters.
  Widgets read the browser URL, but server sample queries used an independent default window.
  The handler now passes the requested `TimePicker` through to samples, controls, and log links.
- First/Recent links discarded the selected range. They now preserve that range.
- Log-pattern pages offered First/Recent and Trace controls without corresponding trace data.
  They now show Logs. Other issues without a trace also select Logs initially.
- The inline range object now uses JSON encoding and escapes HTML script delimiters.

Regression coverage includes both issue routes and two log occurrences on different days.
The selected absolute range must show only its matching occurrence.

### Remaining findings

- F11 is closed in the observed production pages: empty log results now leave the loading state.
- Stackless errors still allocate a large empty panel above the trace evidence.
  Activity height also leaves unused space beside that panel.
- Empty activity sections consume substantial mobile space before Investigation.
- The mobile absolute-range label clips inside its control despite the absence of document overflow.
- Several historical log patterns still show stored samples without matching retained events.
  The sample needs clear provenance, and the pattern/hash mismatch needs further investigation.
- Query alert `459e5987` displays actual value 2 beside an above-5 threshold.
  The stored payload and alert lifecycle need investigation before the page describes this as a breach.

### Additional prior art

[Better Stack's incident guide](https://betterstack.com/docs/uptime/working-with-incidents/)
puts lifecycle, evidence, and comments in the incident detail experience.
Its [AI investigation guide](https://betterstack.com/docs/ai-sre/analyzing-data/incidents/)
provides entry points in the incident timeline and near the page title.

[Sentry's issue details](https://docs.sentry.io/product/issues/issue-details/)
distinguish aggregate tag values from individual event context.
[Datadog's explorer](https://docs.datadoghq.com/tracing/error_tracking/explorer/)
places triage information near the top and provides diagnostic context for each error sample.
These patterns support moving available evidence ahead of empty panels and making sample scope explicit.

### Evidence layout implementation and local validation

All issue types now share one activity sidebar beside the evidence column.
The main column contains the chart, issue-specific details, Investigation, and replay when available.
On smaller screens, activity follows this evidence in DOM and visual order.
A tall activity timeline no longer creates an empty gap before Investigation.

Stackless errors now show compact explanatory text and a direct link to the trace or related logs.
The section title is "Error details" when no stack exists.
The CSS and application builds passed.
The issue-detail suite passed all 26 examples. The error-pattern suite passed all 30 examples.
The latter suite initially exceeded the local database connection limit while the browser server ran.
It passed after that server stopped, with one test worker.

Six browser cases covered runtime errors, log patterns, and query alerts at 1440px and 390px.
Each returned HTTP 200 with no JavaScript errors or document overflow.
Each contained one activity panel. Mobile activity followed Investigation, and dates survived occurrence links.
The final pass also confirmed that mobile titles no longer sit beneath the AI tab.

The asset suite exposed two different chunks with the same generic `dist` name.
One contains bundler helpers. The other contains the Buffer implementation.
The check now identifies module chunks through the served manifest and checks all bundles for duplicate requests.
Worker entry files are absent from that manifest, so only module chunks use the manifest membership check.
All 10 asset checks passed.

Deployment run `34047456390` completed successfully. All seven production reference pages returned HTTP 200 without JavaScript errors or horizontal overflow.
The local browser report is `/private/tmp/issue-layout-verified.json`.

## 14. Deferred event samples (local implementation)

The sample lookup used PostgreSQL directly, while the event list obeys the configured telemetry reader.
It also completed before the issue handler returned the page.

The page now returns the stored sample immediately and requests the latest matching event separately.
The new authenticated `/issues/:id/sample` route checks the issue's project before it reads telemetry.
It uses the configured telemetry reader with a four-second database statement limit and a five-second outer timeout.
The connection's previous statement limit is restored after success or failure.
TimeFusion ignores `SET LOCAL`, so the query uses the existing PostgreSQL protocol pool with bracketed cleanup.
The query retains the selected time range and pattern hash.
The original reference query ran on TimeFusion and returned no matching row without a query error.

The panel distinguishes loading, no match, lookup failure, and a captured event.
A captured event includes its UTC timestamp and a trace link when available.
A stored sample is explicitly outside the range filter.
Range changes request a new sample, and a retry action repeats an unavailable lookup.

The regression fixture now checks deferred rendering, event selection, the stored fallback, and project isolation.
The application and test builds passed, along with all 26 issue integration examples.
Browser checks at 1440px and 390px confirmed captured events, range updates, timestamps, trace links, and empty fallbacks without JavaScript errors or overflow.
The Retry button successfully replaced a failed lookup with a captured event.

An exclusive telemetry-table lock exposed a limitation in the original application-only timeout: the sample response waited 10.8 seconds.
With the database statement limit, the page returned in 19ms and the failed sample returned in 4.02 seconds, while the lock remained held.
The panel retained the stored sample and offered Retry.
Deployment run `34049237508` passed all CI stages and deployed successfully.
The production sample endpoint returned HTTP 200 with the correct empty-range fallback in 705ms.
All seven production reference pages passed again without JavaScript errors or overflow; shell responses measured 216–839ms.

## 15. Mobile time range control

At 320px, the absolute range button extended beyond the issue card.
The custom calendar also covered preset choices, leaving both visible at once.

The issue chart now gives the range button its own row on phones.
Dates wrap, while transport controls remain available below.
The popup stays within the mobile viewport and scrolls in short landscape views.
Custom selection hides the presets and provides a Preset ranges button to return.

A browser preview passed at 320px, 390px, 667px landscape, and 1440px.
Checks covered popup bounds, custom selection, returning to presets, and applying Last hour.
The application build and CSS build passed. The compiled page passed all four viewport checks without JavaScript errors.
The final check also selected two custom dates, verified the resulting one-day interval, and returned to Last hour.
Switching views moves focus to the new view and restores the calendar scroll position.
The browser verified focus in both directions and a zero scroll offset when the calendar opened.
Deployment run `34050684584` completed successfully.
Production checks passed at all four viewport sizes, including custom dates, preset selection, focus, and scroll reset.

### Follow-up: warning threshold provenance

`notifyQueryMonitorStatusChange` always passes `monitor.alertThreshold` to the issue constructor.
The caller also invokes it for warning transitions.
This can record an alert threshold beside a value that only crossed the warning threshold.
The reference monitor overview shows warning > 2 and alert > 5; the issue records actual 3 against threshold 5.
This supports the warning-path explanation, though historical configuration changes remain possible.

### Follow-up: absolute range timezone labels

`parseTimeRange` formats absolute display values without a timezone suffix.
`initTimeDisplay` passes those values to `new Date`, which interprets them in the browser's local timezone.
This can label UTC bounds as local clock times without applying the offset.
Verify the rendered label against the URL bounds in a non-UTC browser before changing the format contract.

## 16. Absolute range timezone correction

A browser check confirmed that absolute ranges displayed UTC clock values in every timezone.
For the same 06:04–10:04 UTC bounds, Berlin displayed 06:04–10:04 instead of 08:04–12:04.
New York also displayed 06:04–10:04 instead of 02:04–06:04.

Both range producers now preserve ISO timestamps with the UTC suffix for browser formatting.
The issue regression assertions and parser doctest expect explicit UTC timestamps.
The application and test builds passed, along with all 26 issue integration examples.
Six browser checks passed across issue detail and Service Map in UTC, Berlin, and New York.

A winter check then showed that the badge used today's offset: Berlin displayed UTC+02 beside January dates that used UTC+01.
The badge now names the browser timezone instead of claiming one fixed offset.
The final browser check covers winter and summer dates on both pages in all three timezones.
The timezone-label build passed. All 12 browser cases passed without JavaScript errors or document overflow.
Each displayed range matched the URL bounds converted to the browser timezone, including the different winter offsets.
Deployment run `34051412266` completed successfully.
All 12 production checks passed across issue detail and Service Map in UTC, Berlin, and New York, in winter and summer.

## 17. Warning threshold provenance

The notification builder received only a recovery flag, so it could not distinguish warning from alert state.
It always recorded the alert threshold in the issue and email.
A warning value of 3 could therefore appear against an alert threshold of 5, even when the warning threshold was 2.

The builder now receives the evaluated monitor state and selects its matching threshold.
The warning fallback follows the existing state machine when a prior warning remains inside its recovery band.
Generated issue titles use the monitor name, with the threshold kept in the evidence payload.
An open query issue refreshes that title on its next notification, so escalation cannot retain an old threshold in the title.

The existing warning-to-alert test now checks the recorded values and thresholds at both transitions.
It also seeds an old title before escalation and checks its replacement.
The insert now returns its persisted ID atomically, including on conflict.
Monitor notifications use that ID instead of the newly generated candidate ID, which can be discarded during an update.
The regression checks that warning and escalation emails point to the stored issue row.
The application and integration-test builds passed.
All 22 monitoring/RUM checks and all 26 issue-detail checks passed.
The transition regression confirmed both threshold snapshots, title refresh, and emails linking to the persisted issue.
Deployment run `34052634915` completed successfully, including integration and browser gates.

Historical snapshots can still reflect older configuration or an evaluation inside a recovery band.
A follow-up should label the panel as a recorded evaluation and avoid asserting a threshold breach when the stored values do not cross it.


## 18. Recorded query evaluations and Explorer handoff

Historical query alerts now label their values as a recorded evaluation.
The panel states the inclusive comparison and explains when the stored value does not meet the stored threshold.
It directs the reader to the monitor’s warning and recovery settings without inferring a historical cause.
The recorded payload remains unchanged.

The Query card now links directly to Explorer with the recorded query and selected time range.
The shared link handler removes stale range parameters when switching between relative and absolute ranges.
It retains the server-provided range when the current URL has no explicit selection.

The application and frontend builds passed, along with 26 issue integration examples and 35 frontend checks.
The integration cases cover both threshold directions, inclusive equality, and mismatched recorded values.
Four browser cases covered matching and mismatched values at 390px and 1440px.
They verified the explanatory copy, query preservation, relative-to-absolute changes, and navigation to Explorer.
No JavaScript errors or document overflow occurred. Desktop and mobile screenshots were reviewed.
Deployment run `34053278096` completed successfully.
Production browser checks passed at 390px and 1440px in light and dark themes.
Each check followed the Explorer link after changing range modes and preserved the complete query.
The first check briefly received the old page during rollout; a direct read confirmed the new markup before the successful repeat.
Screenshots confirmed that the historical 3-versus-5 evaluation receives the explanatory copy.


## 19. Missing pattern evidence: writer mismatch reproduced

A fresh production sweep returned HTTP 200 for all seven reference issues.
No JavaScript errors or document overflow appeared during the observation window.
Main-document response times ranged from 267ms to 1.04s; deferred evidence is excluded.
The query-alert copy deployment was still pending during this sweep.

A compiled probe against the current library reproduced two independent hash mismatches.
These are writer defects, not proof of the cause of every historical empty sample.

1. Drain returns the template matched at each event's insertion time.
   For `Connected to database primary` followed by `Connected to database replica`,
   the final tree contains only `Connected to database <*>`.
   The first event remains mapped to `Connected to database primary`, which is not persisted.
2. `flushDrainTask` builds event tags before applying `mergeByJaccard` to persisted patterns.
   The probe merged `user <*> logged in <*>` into `user <*> logged in`.
   Events retain the former hash, while the writer persists the latter pattern.
   Error-bearing flags also use the pre-merge hashes, so they can miss merged members.

The probe source and executable are `/private/tmp/issue-pattern-probe.hs` and `/private/tmp/issue-pattern-probe`.
Its output establishes the mismatch without production writes.
The next correction must preserve event-to-group identity through in-batch template generalization and Jaccard merging.
The persisted template, event tag, and error-bearing aggregation must use the same final identity.
Tests must include earlier events whose group changes later in the batch, merged members, and bounded-tree eviction.
Historical rows require separate assessment; changing the forward writer cannot repair existing event hashes.


### Writer correction in progress

Cached Drain groups now have a stable internal identity independent of template text.
Batch membership follows that identity through generalization and is emitted even after tree eviction.
Jaccard merging combines memberships before the writer derives hashes and error-bearing flags.
Batch frequencies count current events only; previously the writer added cached cumulative frequencies again.
The tree does not retain batch event IDs or apply the 500-ID diagnostic sample limit to writer membership.
Application and regression builds are in progress; this correction is not deployed yet.

### Additional trace finding

The payment reference `cc1c70a9` returned the trace fallback on initial load and after retry.
Its trace requests completed in 263–332ms, so the generic timeout explanation is not established by this observation.
The request pairs trace `17cd10403b697f3a56701efde797470b` with `2026-09-06T13:27:56.315404Z`.
The issue URL selects August 27. The actual trace timestamp still needs verification.
`batchUpsertErrorPatterns` currently stores the processing-time `now` as `recent_trace_at`.
That keeps two columns in one update but does not prove that the timestamp matches the telemetry event.
The browser evidence is `/private/tmp/issue-trace-retry-production.json`.
Investigate the trace timestamp and distinguish empty results from failed reads in a separate change.


The configured telemetry reader returned zero rows for that trace in both the August 27 issue window
and the ten-minute window around the linked September 6 timestamp.
Both bounded reads completed successfully with a four-second statement deadline.
This supports an empty-result state; it does not prove the trace is absent outside those windows.
The read-only probe is `/private/tmp/check-issue-trace-time.py`; results are in `/private/tmp/issue-trace-time-result.log`.


The production query screenshot also leaves two presentation follow-ups:
its historical generated title still embeds the old threshold, and the chart's y-axis stops at 1 while the recorded threshold is 5.
The detail header should prefer the recorded monitor name, and the chart should keep the threshold visible when it lies outside the returned series.
These require separate validation after the ingestion change.


## 20. Validation follow-ups for pattern identity and threshold axes

Commits `12f16ea0` and `ead6d241` appeared on shared master during local validation.
The application build completed. The extraction fixture needed its required summary field before its database assertion could run.
After that correction, all five extraction checks and all 26 issue-detail checks passed.
The broader 23-case log-pattern run then caught a real regression: logs without span IDs produced no patterns.
Their empty span ID had been mistaken for the seed-template sentinel by batch membership accounting.

Cancellation was requested for deployment `34055192437`. An external rerun started attempt 2 against the same pre-fix SHA; cancellation was requested again.
Attempt 2 is now confirmed cancelled, including the CapRover deployment job.
The follow-up now carries the existing telemetry row UUID in `BufferedSpan.eventId`.
Membership, error flags, and UPDATE-2 use that row identity, while span/trace fields retain the established lock ordering.
The database regression includes a row with no span or trace context.
The correction passes 5 extraction, 23 log-pattern, and 26 issue-detail integration examples (54 total).
The extraction parity assertion now requires a `pat:` tag, rather than any nonempty hash.
Local tests use PostgreSQL for both connections; real TimeFusion write validation remains a CI gate.

The chart-axis change passed 12 frontend checks and browser checks at 390px and 1440px.
For a series peaking at 1 and threshold 5, the axis spans 0–5.25; the threshold's pixel position is inside the plot.
The label moved from the clipped outside edge to `insideEndTop`; the mobile screenshot shows `Alert: 5` in full.
The rebuilt server passed four browser cases using its own asset references: relative and absolute ranges at 390px and 1440px.
All cases show the threshold inside the plot, with no JavaScript errors, missing assets, or page overflow.
The absolute-range fixture required its date field to match its event timestamp and its local cached query result to be cleared.
One overlapping frontend build temporarily removed the manifest during Haskell compilation; builds are now kept sequential.


### Telemetry backend compatibility

The first row-ID query used `uuid[]`, which the configured TimeFusion planner rejects as an unsupported SQL type.
A read-only `EXPLAIN UPDATE` succeeded after comparing TimeFusion's text IDs directly.
The writer now supplies separate match expressions to the existing dual-backend execution path:
PostgreSQL compares `o.id = u.event_id::uuid`; TimeFusion compares `o.id::text = u.event_id`.
Both forms keep project, age, and timestamp bounds. No production rows were updated by the planner probes.
The local PostgreSQL schema has no ID index, contrary to an earlier commentary assumption.
Its inspected plan uses a project index with the timestamp window as a filter; no ID-index performance claim is made.
The native PostgreSQL comparison avoids per-row UUID-to-text conversion.
Planner artifacts are `/private/tmp/pattern-update-plan.log` and `/private/tmp/pattern-update-pg-plan.log`.
The application and integration-test builds completed successfully. Deployment verification remains pending.
