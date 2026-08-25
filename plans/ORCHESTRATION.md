# Parallel workstream orchestration (started 2026-08-25)

Baseline commit: `c71ec614f`. The intent was to keep master local until a coherent state was
ready — but a concurrent session working in this same checkout pushes master periodically, so
all four streams reached origin before that decision was made. See "Note on pushing" below.

## Streams

| Stream | Branch | Location | Migration no. | Status |
|---|---|---|---|---|
| Dashboards fix + e2e | `master` | main checkout | none | **done** — see below |
| Billing / new pricing | `ws-billing` | `.claude/worktrees/ws-billing` | 0136 | merged |
| Container monitoring | `ws-containers` | `.claude/worktrees/ws-containers` | 0137 (unused) | merged |
| Metric exemplars | `ws-exemplars` | `.claude/worktrees/ws-exemplars` | 0138 (unused) | merged |

## Rules every stream follows

1. **`.env` `DATABASE_URL` points at PRODUCTION.** Never run `monoscope-server`, `cabal run`,
   or anything that starts the app from a worktree — startup runs migrations against prod.
2. **Never `git push`.** Pushing master is a ~28 min production deploy. The orchestrator
   decides when a coherent state gets pushed.
3. Sharing between streams is by **local** merge into master; worktrees share the object DB.
4. Migration filenames are pre-assigned above to avoid the historical `0125` collision.
   Migrations are append-only and must be additive/safe.
5. Main checkout verifies compilation by reading `build.log` (ghcid watcher). Worktrees have no
   watcher, so they verify with `cabal build lib:monoscope --ghc-options="-O0 -j8"` in the
   worktree — never in the main checkout.
6. `e2e/` is owned exclusively by the dashboards stream.

## Dashboards stream outcome

Fixed, each guarded by a new test in `e2e/tests/dashboard-add-widget.spec.ts`:

- **"Add to dashboard" from the log explorer did nothing.** The dashboards list only became
  a destination picker when the request carried both `copy_widget_id` *and*
  `source_dashboard_id`; the explorer's chart has no source dashboard, so the modal opened
  as an ordinary list and clicking a row navigated away. The source is now optional — with
  one a row copies by id, without one it PUTs the chart's own JSON to the widget upsert.
- **The widget editor led with Logs.** Charts now lead; Logs stays available, at the end.
  The log explorer is unchanged.
- **The logs preview overflowed its frame** by ~260px and painted over the form below it.

Suite went 31 → 35 tests, all green. Drag, resize, duplicate and delete were already
covered by `dashboard-grid.spec.ts`; the drawer's save path and cross-dashboard copy were
not, and now are.

Found and written up rather than fixed: `plans/dashboard-variable-defaults.md`.

Pre-existing and unrelated: `make lint` fails to read `.hlint.yaml` (key/version mismatch);
CI runs hlint in its own container.

## Plan review (orchestrator, before merge)

**Billing** — `plans/billing-pricing-v2.md`. Accepted. Found the replay counting source
(`projects.replay_sessions`, one row per session, `session_id` globally unique), established
that Stripe needs no per-meter subscription item (meters are addressed by customer + event
name) while LemonSqueezy does (new `projects.billing_meter_items`), and left
`first_sub_item_id` untouched so no webhook changes. Dormant meters create **no** submission
rows rather than buffering a backlog — the right call given the provider-switch backlog leak
we have already hit. Pricing copy is inventoried but deliberately not edited; it must change
in the same breath as the provider-side meters being created, which is a human step.

**Exemplars** — `plans/exemplars-correlation.md`. Accepted. Verified against production
TimeFusion that exemplars survive ingestion and carry real trace ids, so no migration is
needed (0138 stays unclaimed). Caught a real correctness trap: a histogram keeps one exemplar
per bucket, so an exemplar's timestamp can be weeks older than its row's — every link must use
the exemplar's own timestamp or we deep-link into deleted traces and hit the known TF 504
path. Scope is honest about what it skips and why.

**Containers** — `plans/container-monitoring.md`. Under review.

## Pricing target (billing stream)

- $1 per 1,000,000 events (spans + logs) — existing `events_usage` meter.
- $1 per 10,000,000 metric datapoints — new meter, currently recorded but unsubmitted.
- $1 per 1,000 session replays (RUM) — new meter, no counting source yet.

## Merge outcome (all four streams on master)

Verified green on the merged tree: integration 745/0 (also 745/0 against a real, freshly
wiped TimeFusion), doctests 1251/0, unit 274/0, CLI 16/0, e2e 35/0. `Pages.GitSync / GitHub Sync E2E (Real API)` is gated on `GH_TEST_PAT` and either
skips or fails against live GitHub depending on the environment; it is unrelated to this work.

Five defects the merge itself produced or exposed, each fixed:

1. **`ingestMetric` gained a parameter in two streams at once.** Containers added a
   resource-attribute list so a test could emit a `k8s.pod.name` row at all; billing and
   exemplars still called the old arity. Git merged the definition cleanly and left the call
   sites stale — a semantic conflict no textual merge can see.
2. **Two warnings that ghcid suppresses but the real build treats as errors**
   (`-Wno-error=unused-imports`, `-Wno-error=unused-top-binds` are passed only to the watcher).
3. **Migration 0135 made every `def`-constructed monitor un-insertable**, because the
   generic `Default` gives `timeWindowMins = 0` and the new CHECK forbids it. Fixing that
   exposed a second staleness in the same fixture: `evaluateQueryMonitor` re-parses `logQuery`
   and ignores the stored SQL, so an empty query threw before notifying — silently, since the
   throw is caught and logged.
4. **Two container doctests were ambiguous on `namespace`**, which names a field of both
   `ContainerFilters` and `ContainerRow`.
5. **CI reads flipped from Postgres to a shared TimeFusion**, breaking 23 examples across seven
   specs — not the 8 first reported here. `bac2a9a5d` pinned `enableTimefusionReads = tfEnabled`
   in `Pkg.TestUtils`; `tfEnabled` is false locally and **true in CI**, so every telemetry read
   moved from per-example Postgres to one shared, asynchronous store. Two shapes, only one of
   them fixable: counts inflated by other specs' rows (fixable by per-example project isolation,
   which `createTestProject` now gives `ReportUsageSpec`), and counts *deflated* by write→read
   visibility (`expected 500, got 65`) — which no isolation or SQL fix can make synchronous.
   Reads are now pinned off; writes still go to a real TimeFusion. Reasoning and the red/green
   repro are in `plans/tf-reads-in-tests.md`.

   The pin was not wasted: it made CI briefly prod-like and surfaced a **real production bug** —
   `fetchLogPatterns`' fallback used a SQL `mode() WITHIN GROUP`, which DataFusion cannot plan,
   against the store production actually reads. Dormant since February, fixed in `13982e099`.

**Note on pushing.** A concurrent session in this same checkout pushed master, carrying all four
streams to origin. Nothing reached the servers: CI was red on (5) and the deploy job is gated on
tests. Migration 0136 did reach production, applied by the local ghcid server restarting after the
merge — it is additive only and applied cleanly.
