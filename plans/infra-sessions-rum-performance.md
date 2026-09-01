# Infrastructure, Sessions and RUM page latency — findings, 2026-09-01

Measured against prod TimeFusion, demo project `00000000-0000-0000-0000-000000000000`,
one query at a time, windows escalating 5m → 1h → 6h → 24h.

Harness: `scripts/local/infra-sessions-perf-2026-09-01.sh` (infra + sessions, and every
candidate rewrite below), alongside the existing `scripts/local/rum-perf-2026-08-30.sh`.
Both stop a family at the first window that exceeds a threshold, because an unbounded
aggregate at a wider window is how prod TF gets OOM-killed.

## Baselines

| page / query | 5m | 1h | 6h | 24h |
|---|---|---|---|---|
| **Infra** — containers pivot, as shipped | 0.51 | 1.77 | **42.33** | **timeout (>60s)** |
| **Sessions** — `fetchSessions`, as shipped | 1.31 | 1.67 | 5.47 | **18.74** |
| **RUM** — 7 panels concurrent (2026-08-30) | — | — | — | ~20 cold / 0.35 warm |

The infra number is the headline: **over a 24-hour window the query does not complete at
all.** It hits the statement timeout, so `containersInWindowCached` never gets a value to
store, the 300s TTL never applies, and *every* load pays the full timeout and renders
nothing. That is the "too slow" report in its most literal form — it is not slow, it is
broken, and only at the wide windows.

## What is actually expensive, per page

### Infra: the pivot sorts the entire window to find the newest point per series

`containersInWindow` computes `row_number() OVER (PARTITION BY … ORDER BY timestamp DESC)`
across every metric datapoint in the picker's window, then keeps `rn = 1`. To display the
current CPU of ~50 containers over 24 hours it sorts every datapoint those containers
emitted all day — and each row of that sort also evaluates ~15 `variant_get(resource, …)`
lookups into a Variant blob.

The window is the wrong input for this. The values displayed are by definition the *newest*
datapoint per series, and a series that still exists reports every 15–60s, so its newest
datapoint is always in the last few minutes. Scanning further back can only find older
points that `rn = 1` then discards.

**Rewrite: take values from a freshness slice at the end of the window, not the whole
window.** `[max(fromTime, toTime − N minutes), toTime]`, N = 15. Short picker windows are
unchanged (the slice is the window); long ones stop paying for history they discard. Using
`toTime` rather than `now()` keeps a historical from/to range correct — it shows the state
at the end of the selected range.

| infra | 5m | 1h | 6h | 24h |
|---|---|---|---|---|
| current | 0.51 | 1.77 | 42.33 | timeout |
| freshness slice | 2.42 | 0.57 | 0.67 | **0.70** |

Flat, and it removes the timeout entirely: **63× at 6h, and 24h goes from not completing at
all to 0.70s.**

Semantic change, deliberate and worth stating: a container that stopped reporting hours ago
drops off a 24h view instead of appearing with hours-old numbers sorted among live ones. The
table is "what is running now", sorted by CPU; a dead container carrying stale CPU into that
ordering was misinformation, not a feature.

**Keeping window semantics for presence was measured and rejected.** A presence-only query
(distinct identity over the window, no window function, no `variant_get`) costs **8.77s at 6h
and 25.12s at 24h** — for 84 distinct series. It is the scan that is expensive, not the pivot,
so there is no cheap way to say "seen in the last 24 hours". The page therefore says what it
now shows: the result summary and the facet menus describe the freshness slice, not the
picker window. Leaving the old "Showing X of Y containers" copy against sliced data would
have been a new lie in the UI, which is the other half of what this work is fixing.

### Sessions: one CTE, scanned four times, carrying a blob it does not need

`fetchSessions` builds a `filtered` CTE and references it from `agg`, `svcs`, `hourly`, and a
subquery inside `summ`. **DataFusion inlines a CTE at every reference**, so that is four
scans of the window. Two independent costs on top:

- `filtered` projects `COALESCE(NULLIF(status_message,''), NULLIF(body::text,''))`. `body` is
  a wide ByteView blob, materialized for every row in the window — to populate `first_error`,
  which only the 20 rows of the displayed page ever show.
- `svcs` and `hourly` re-scan the whole window to compute per-session values for those same
  20 page rows.

Six rewrites were measured. The demo project is also the **heaviest** session-keyed project
(73.8k session-keyed rows/hour vs 18.0k for the next), so these are worst-case numbers.

| sessions | 6h | 24h | correct? |
|---|---|---|---|
| as shipped | 5.47 | 18.74 | — |
| without `body::text` in the scan | 4.22 | 13.23 | ✅ (needs page-scoped `first_error`) |
| fold `svcs`/`hourly`/`unique_services` into `agg` | 1.92 | 7.23 | ❌ loses per-bucket counts + `unique_services` |
| `GROUPING SETS` (both levels, one scan) | 5.01 | 17.16 | ✅ but no faster — two hash tables |
| two-level roll-up, TF-native array union | 3.27 | **8.72** | ✅ (TF-only `flatten`/`array_distinct`) |
| two-level, dual-backend plain SQL | 4.57 | 14.01 | ✅ both backends |

**Sessions is deliberately left alone tonight, and that is a finding, not an omission.**

The fold-into-`agg` variant that looked best (7.23s) is wrong: it returns distinct bucket ids
without counts, so the per-session sparkline loses its values, and it drops `unique_services`
entirely. Once corrected, every shape that works on **both** backends lands at 13–14s — about
1.3× — because `filtered`'s four scans are of a *narrow* projection, and widening the grouping
key to make the union expressible in plain SQL costs more than the extra scans save. The one
shape that reaches 8.72s needs `flatten`/`array_distinct`, which Postgres does not have.

1.3× does not justify rewriting the query that has previously crashed prod TF, unattended.
The recommendation, for a session with someone watching:

- Take `body::text` out of the window scan (`first_error` is display-only for 20 page rows) —
  worth 1.4× on its own and is the smallest possible change.
- Then decide explicitly whether the sessions path may use TF-native array functions. If yes,
  the two-level roll-up plus a page-scoped follow-up is ~1.75×. If Postgres must keep working,
  there is no worthwhile win here and the answer is F1, not SQL shape.

Note the structural point either way: `filtered` is referenced four times and **DataFusion
inlines a CTE at every reference**, so it is four scans. That is worth knowing before anyone
adds a fifth.

### RUM: already largely addressed; what remains is the scan filter

The 2026-08-30 work (per-panel deferred loading, `rumCache` TTL 15s → 300s) took the page
from ~28s blank on every visit to 0.33s structure / 3.87s cold / 0.35s warm. See
[`scripts/local/rum-perf-2026-08-30.md`]. What remains is cold cost, and it has a single
cause: `browserScope` is a four-branch OR of `IN`, `IS NOT NULL` and `LIKE 'Pageview %'`. It
is 3.2% selective and can neither prune row groups nor route to the Tantivy index — `name` is
`tokenizer:ngram3`, and `IS NOT NULL`/`LIKE` are not exact matches. ~31× read amplification.

That is not fixable from the query side. It needs F1 (below).

## The root-cause fix, and why it is not in tonight's change

All three pages share one cause: **TimeFusion reads far more than it returns, because the
predicates cannot prune or route.** The documented unlock is F1 from
`scripts/local/dashboard-subsecond-design-2026-07-15.md`: a low-cardinality category column
written at ingest, indexed `tokenizer:raw` and added to `sorting_columns`, filtered on
instead of the OR ladders. It still does not exist in either repo (checked 2026-09-01).

Deliberately **not** attempted unattended:

- It spans two repositories and needs a TimeFusion **deploy**, which is a manual WAL-lock
  procedure — stop-first takes the whole app down, start-first deadlocks. Not a thing to
  trigger with nobody watching.
- It only accelerates rows written *after* the change. Every existing window still needs the
  old OR predicate as a fallback, so pruning does not actually engage until old data ages out
  of retention — unless a backfill is written, which is its own project.

**Deploy checklist when it is picked up:** add the column to the ingest path
(`ProcessMessage`), add it to `schemas/otel_logs_and_spans.yaml` as a raw-tokenized indexed
column *and* a secondary sorting column, deploy TF with the documented `docker stop -t 120`
incumbent procedure, then switch the readers behind a config flag so the OR fallback can be
restored without a deploy. Validate with COUNT parity per window before trusting it.

## Rollups and materialized views: recommended against, for now

The 2026-07-15 decision explicitly rejected pre-aggregation ("fix and improve the db without
rollups first … Focus on TF"). Tonight's instruction reopens it. Recording the reversal so it
is not re-litigated from scratch: **rollups are now on the table, but the measurements say we
do not need them yet.**

The query-shape fixes above take infra from *timeout* to 0.70s and sessions from 18.7s to
7.2s, at zero storage cost, zero staleness, no new migration against prod, and no new
aggregation subsystem to operate. A rollup table would have to beat 0.70s to be worth its
own failure modes.

Where a rollup would genuinely help is the one thing shapes cannot fix — RUM's 31× read
amplification — and there F1 is both cheaper and more general, because it speeds up every
query with that predicate rather than the handful we pre-aggregate. If F1 proves undeliverable,
5-minute RUM buckets keyed `(project, service, bucket)` are the fallback; spec, not built.

One tension worth flagging rather than deciding silently: `CLAUDE.md` says not to invest in
Postgres-side complexity that the TimeFusion migration will delete. A rollup table in PG is
exactly that kind of investment.

## What is being implemented tonight

1. Infra: values from a freshness slice at the end of the window, and a result summary that
   says so.
2. Infra: the two usage charts get a height floor (see UI findings below).
3. No sessions SQL change, no new migration, no new job type, no TF deploy.

Verification for each is the harness above, same windows, before and after.

## UI findings on Infrastructure and RUM

Audited in a real browser at 1600×1000 with the freshly built stylesheet injected (the dev
servers splice a stale hashed CSS filename, so anything measured against the served page is a
measurement of last week's CSS).

### Fixed: the container usage charts had no plot area

`containerCharts_` renders two `WTTimeseriesLine` widgets side by side under
`.container-usage-chart`, which sized itself purely by `aspect-ratio: 4/1`. Side by side on a
1600px viewport each box is ~578px wide, so the ratio yields 144px, and once the widget's
title and padding are taken out the canvas got **56px**. A multi-series line chart with a
top-right legend has no room to draw at that size, so both panels read as empty even though
both `/chart_data` requests returned 200 with full datasets.

A `min-height: 13rem` floor takes the canvas to 140px. The ratio still governs growth on wide
screens; the floor only bites where the ratio collapsed.

### Fixed: infra tables were clipped, not scrollable

`#<id>_grid` (from the shared `Pkg.Components.Table`) was `overflow-hidden`, so anything wider
than the content area was silently cut off — no scrollbar, no way to reach it. Measured at
1600×1000 with the facet rail open:

| table | needs | gets | unreachable |
|---|---|---|---|
| Containers | 1460px | 1250px | 210px |
| Kubernetes | 1819px | 1250px | 569px |
| Images | 1784px | 1250px | 534px |

The reason this looked risky is that `overflow-x: auto` forces the browser to compute
`overflow-y: auto` when y is `visible` — which would make the div a scroll container and steal
the sticky `thead`'s scroll root. **Measured in the browser, that concern does not apply here:**
`overflow-hidden` had *already* set `overflow-y: hidden`, so the box was a scroll container in
both axes before the change and still is. Setting `overflow-x-auto overflow-y-hidden` alters
only the x axis; the computed `overflow-y` is `hidden` before and after, so sticky resolves
against exactly what it resolved against before. With it, `scrollLeft` reaches the full 210px
that was previously unreachable.

This is a shared component, so it affects every table — in the same direction: a table that
fits is unchanged, and one that does not now scrolls instead of hiding columns.

### Fixed: RUM Web Vitals read "No data" while the metrics were in the store

Both the Overview panel and the Performance tab reported `No data / 0 samples` for all five
Core Web Vitals. The metrics were there the whole time — the query just did not match their
names.

`vitalSamples` filtered `metric_name IN ('browser.web_vital.lcp', …)`, the OpenTelemetry
browser SDK's spelling. k6's browser module emits the same measurements as
`k6.browser_web_vital_lcp` — dot-free, behind its own prefix — and that is what the demo
project (and any k6-driven setup) actually sends. `vitalsFromSamples` had the same assumption
baked in a second time: it derived a vital's key by stripping the literal prefix
`browser.web_vital.`, so even if a k6 row had reached it, it would not have mapped to a vital.

Both now handle either spelling. The name list is built from `vitalDefinitions` so it cannot
drift from the vitals actually rendered, and the key is derived (everything after the last `.`
or `_`, lowercased) rather than stripped per known prefix, so a third emitter maps itself.
Verified at the query level against prod TF over 24h: LCP 394ms, INP 572ms, CLS 0.097,
TTFB 172ms, FCP 621ms — 617k samples that previously rendered as "No data".

### Audited and clean

RUM Overview, Sessions and Performance were checked at 1600/1280/820px with the freshly built
stylesheet: no horizontal overflow, no clipped tables, no collapsed canvases, no skeletons left
after 11s, and no `NaN`/`undefined`/`Invalid Date` leaking into the copy. The layout is sound;
the vitals bug above was the substantive defect.

### Noted, not a bug

`/p/<pid>/infrastructure` has no route and 404s. Nothing links to it — hosts, containers,
images, kubernetes and host-map are each their own route — so this is only reachable by typing
the URL. A redirect to `/infrastructure/hosts` would be polite, but nothing in the product is
broken by its absence.

## CI state at hand-off (2026-09-01 ~03:00)

**`Build and Test` is red on master, so `Deploy to CapRover` is skipped and none of tonight's
work is live yet.** Two Playwright e2e tests fail, reproduced locally (41 passed, 2 failed,
5 skipped — same as CI):

1. `log-list-virtual-scroll.spec.ts:10` — "deep paging and live delivery preserve the row under
   the reader". The reader's top row moves from `seed-02495` to `older-0-00100` on the first
   load-more.
2. `query-editor.spec.ts:41` — the query editor's top/bottom inset differs by 2px, not ≤1.

**Neither is caused by the log-list or infra work.** Established by bisect, not assumed: on a
clean tree at current master, reverting *only* `web-components/src/log-list.ts` to its
last-green version (`8a61f2e60`) still fails the same way. The retention constants, the layout
specifier hoist, the rAF-coalesced scroll handler and the `isRepositioning` buffer guard were
each reverted individually as well — no change.

What the timeline says instead: `8a61f2e60` was fully green (48 e2e tests, 43 passed, 5
skipped). The next push carried two commits, and the *other* one — `6c092c2d8`, the in-flight
query-box/query-editor/widgets work that was in the tree and got committed along with the log
explorer fix — is where the query-box geometry changed. That matches failure 2 exactly, and
failure 1 is on a spec that drives the real log-explorer page chrome. The same session has
since pushed `471f57fba` ("point the AI-search and editor-geometry specs at what the markup now
is"), which fixed a third failure but not these two.

**Next step for whoever picks this up:** the repro is 6 seconds —
`scripts/e2e.sh tests/log-list-virtual-scroll.spec.ts`. Bisect `6c092c2d8`'s frontend files
(`query-editor.ts`, `widgets.ts`, `types.ts`, `log-worker-functions.ts`) and
`src/Pkg/Components/LogQueryBox.hs` rather than the log-list, which is already ruled out.

### One more clue on the query-editor geometry failure

Measured directly against the running dev server with the **freshly built** `tailwind.min.css`
injected, the geometry is exactly right: shell 40px, control 40px, `topInset` 10, `bottomInset`
10 — difference 0, comfortably inside the spec's ≤1. Under what the e2e server serves, the same
assertion sees 2.

So the markup and the current stylesheet source agree with the spec; something about the
stylesheet the server actually serves does not. Worth checking before touching padding: compare
the hashed CSS the e2e server references against `static/public/assets/css/tailwind.min.css` on
disk, and confirm `make post-css` has run against the tree the e2e binary was built from. Chasing
this by adjusting insets would be fixing the wrong layer.

Left for the owner of that work — the query-box/editor changes are an active workstream and this
is their area, not the infra/RUM one.

---

## Correction (2026-09-01 ~04:15) — the local bisect above does not hold for spec 1

`log-list-virtual-scroll.spec.ts` **fails locally regardless of the source under
test**, so a local revert cannot rule anything in or out for it.

Measured, on a clean tree, reverting to `8a61f2e60` in widening circles and running
`scripts/e2e.sh tests/log-list-virtual-scroll.spec.ts` after each:

| reverted to `8a61f2e60` | result |
|---|---|
| `query-editor.ts`, `types.ts`, `log-worker-functions.ts` | same failure |
| + `log-list.ts`, `widgets.ts`, `LogQueryBox.hs`, `Log.hs`, `Components.hs`, `tailwind.css` | same failure |
| + `Table.hs`, `Widget.hs`, `stat-value.ts`, `chart-cli.ts` | same failure |
| restored to HEAD | same failure |

Every log-explorer-rendering file at the last-green commit, the spec itself
unchanged since then (`git diff 8a61f2e60..HEAD -- e2e/` touches only
`live-tail.spec.ts` and `query-editor.spec.ts`) — and it still reports
`older-0-00100` for `seed-02495`, deterministically.

So the conclusion above — "reverting only `log-list.ts` still fails, therefore the
log-list work is not the cause" — is **unsupported**: that revert fails for the
same reason every other revert does.

**Database state is not the variable.** The spec stubs `**/log_explorer/data**`
to an empty result and builds all 2,500 seed + 400 older rows synthetically via
`makeRows`, driving `list.fetchData` directly. Local and CI see identical data.

What is left is the environment: CI's Linux container Chromium vs local macOS
Chromium, i.e. row metrics or the timing of the post-remount anchor restore
(the spec polls `list.scrollSettling` for 20 × 25ms). Note `COUNT + PAGE`
(2,900) exceeds `RETENTION_LIMIT` on the **first** load-more, so a retention cut
and virtualizer remount fire immediately — under both the old single-threshold
constant and the new pair.

**Therefore:** only CI can discriminate for this spec. Either bisect through CI
(~40 min per round trip), or first make the spec deterministic — wait on an
explicit post-restore signal rather than a bounded `scrollSettling` poll — and
then bisect. Do not spend more time on local reverts; they are not measuring
what they appear to measure.

## Retraction (2026-09-01 ~04:50) — the correction above was wrong, and so was the bisect it corrected

**`scripts/e2e.sh` does not build anything.** It runs whatever
`cabal list-bin monoscope-server` already points at (line 40-41). So every
Haskell-side revert in both bisects — `LogQueryBox.hs`, `Log.hs`,
`Components.hs`, `Table.hs`, `Widget.hs` — changed nothing about the server the
browser was talking to. Only the TS and CSS reverts took effect, because those
are served as static files.

That is why "reverting to last-green still fails": the Haskell half of
last-green was never actually under test.

**After `cabal build monoscope-server`, the current tree is green: 43 passed, 0
failed, 5 skipped.** `611d1aa82`'s revert of the query-box wrapper fixed both
failures — `query-editor.spec.ts` and `log-list-virtual-scroll.spec.ts` — which
also confirms the query-box chrome, not the log-list work, was the cause of
both.

**Rule for anyone bisecting an e2e failure here:**

```bash
cabal build monoscope-server     # REQUIRED after any .hs change
make post-css                    # after any CSS change
cd web-components && npx vite build --mode development   # after any TS change
scripts/e2e.sh tests/<spec>
```

Skip the first line and you are testing a stale binary while believing you are
testing your revert. Note also that a `--mode production` vite build empties
`dist/` and changes the manifest hash that `BodyWrapper.hs` TH-splices, so the
server must be rebuilt after one or *every* spec fails on a broken page.

### Fixed: the query editor's text sat a pixel high

Monaco was configured `padding: { top: 8, bottom: 8 }` against `lineHeight: 20`, so a single
line came to 36px of content inside the 38px the shell reserves with `min-h-[38px]`. The wrapper
is a block, so nothing centred the slack and it all fell below the text: 9px of space above the
line, 11px below, in a box meant to line up with the control beside it. `9 + 20 + 9` makes one
line exactly 38. Measured against the real served stylesheet: 10 and 10.

The two numbers are coupled — change `lineHeight` or the shell's `min-h-[38px]` and the padding
has to follow. The comment at the call site says so.

## Local e2e environment is damaged (CI is not)

I deleted `static/public/assets/web-components/dist` to test whether a stale bundle explained a
local/CI disagreement. Rebuilding it (`npm run watch`, which is `vite build --watch --mode
development`) regenerates every directory and the manifest, and the globals the tests look for
(`filterByField`, `viewFieldPatterns`) are present in `js/index.*.js` — but the local e2e server
still serves a page where they never reach `window`, and 16 specs now fail locally that CI
passes. Recompiling the asset-hash splice (`touch src/Pages/BodyWrapper.hs`) did not fix it.

**CI is unaffected** — its `Build Frontend` job builds from source and has been green on every
run. Trust CI over a local e2e run until this is sorted. Whoever picks it up: the likely culprit
is the server binary's TH-spliced asset hashes not being invalidated by a manifest change in the
path `scripts/e2e.sh` builds through, so start by confirming which `index.*.js` the e2e server
puts in the page against `dist/manifest.json`.
