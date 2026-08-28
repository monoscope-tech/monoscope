# Overnight session notes — 2026-08-27/28

## Shipped to master (main checkout, deployed)

| Commit | What |
|---|---|
| `8b75fa588` | Monitor decode fix + billing-gate observability |
| `7c0cbf814` | preserveTimeRange → bundle; shared filter-menu builder |

### 1. Alerting was fully dark for three weeks — FIXED

`QueryMonitor.lastEvaluated` was `UTCTime` but `monitors.query_monitors.last_evaluated`
is nullable. Hasql fails the **whole row set** on one unexpected NULL, so
`getActiveQueryMonitors` threw and *no* monitor anywhere was evaluated.

One row created 2026-08-07 (`fb7d109f-…`, project `87576849`) did it. Failure counts
per week: 3 (May) → 719 → 4098 → 4234 → 1330. Total 10,384.

- `lastEvaluated` is now `Maybe UTCTime`; never-evaluated means **due now**.
- Migration `0140` makes `trigger_less_than` and `teams` NOT NULL (same latent trap:
  nullable column, non-`Maybe` field; `trigger_less_than` had no default either).
- Regression test: `neverEvaluatedMonitor_isDueImmediatelyAndDoesNotBlindTheOthers`.

**Verify after deploy:** `QueryMonitorsCheck` failures should stop; monitors resume.

### 2. Billing — diagnosed, partially fixed, one decision left for you

The log read you asked for first: `project_count = 12` at 2026-08-27 14:43 UTC, so the
scheduler is healthy and `recentlyActiveProjectIds` was **not** the problem. Eight
projects logged "Reporting usage"; only five logged "Usage to report" and advanced.

| Project | Plan | Watermark | Cause |
|---|---|---|---|
| DSI-APP | UsageBased | 2026-07-17 | `first_sub_item_id = ''` → silent skip |
| TestCorp (demo, `00000000…`) | SystemsPricing | 2026-07-17 | `first_sub_item_id = ''` → silent skip |
| Talstack Prod | GraduatedPricing | 2026-08-24 | **unexplained** — passed the gate, never returned from `getUsageTotals` |
| Community Fluency App | GraduatedPricing | 2026-08-24 | no telemetry in 24h → not in `recentlyActiveProjectIds` |
| Payment, First TEst | UsageBased | 2026-08-23 | same |
| Engine/API Prod | GraduatedPricing | 2026-07-17 | `active = FALSE` — correctly excluded |

`''` is empty, **not NULL**, so an `IS NOT NULL` audit passes — that is why an earlier
pass concluded "present on all 11".

**Shipped:** the gate now logs why it skipped (`logAttention` when a *paid* plan cannot
be billed), plus a `Counting usage` line that separates "skipped the gate" from "never
came back from the store". That last line is what will identify Talstack Prod at the
next 00:00 UTC DailyJob.

**NOT done — your call, deliberately:** populating `first_sub_item_id` for DSI-APP
(≈323M billable units per earlier analysis) or TestCorp (the public demo project) starts
real invoicing. Left untouched.

**Deadline:** `usageWindowStart` clamps to 7 countable days. The 08-23/08-24 group's gap
becomes permanently unbillable ~**08-31**. The 07-17 group is already past recovery.

### 3. Code-quality follow-ups — done

`preserveTimeRangeAttrs` no longer inlines 200 chars of JS onto four handlers of every
Explorer nav link; it is one delegated listener in `main.ts` keyed on
`data-preserve-time-range`. Rewriting stays just-in-time because the range changes
without a reload. `singleSelectFilter` gained a `multiSelectFilter` sibling over a
shared builder; Anomalies and Dashboards no longer hand-build `FilterMenu`.

## Refactor sweep — branch `refactor-sweep` (this worktree, NOT pushed)

Working in a worktree because the other session is editing `Dashboards`, `Anomalies`,
`Monitors`, `Settings`, `Infrastructure`, `Containers`, `BodyWrapper`, `ServiceMap`,
`Widget`, `ApiSpec`, `LogSpec` in the main checkout.

**Two integration failures in the main tree are theirs, not mine** — `ApiSpec:46` and
`LogSpec:784` assert `aria-label`s whose source changes have not landed yet.
Full suite otherwise: 758 examples, 2 failures, 28 pending.

### Reviewed

- `ProcessMessage.hs` — **no findings**. Dense but genuinely well-built: doctests on every
  pure helper, rationale comments, idiomatic combinators. Forcing changes would be churn.
- `BackgroundJobs.hs` — `processBackgroundJob` was 657 lines. ~40 arms correctly delegate
  to named functions, but `ReportUsage` had its entire 133-line implementation inline.
  Extracted to `reportUsageForProject`, matching the file's own convention. Payoff:
  the billing gate is now reachable from a test, and `BillingSpec` is the first spec
  this codebase has ever had for revenue-critical code.

### Notes for whoever continues

- The worktree watcher is **compile-only on purpose**. `make live-reload` there runs
  `--test :run Start.startApp`, which would boot a second server against the **prod**
  `DATABASE_URL` in `.env` and join the prod Kafka consumer group.
- Running `cabal test doctests` desynchronises `monoscope-shared`'s `.hi` vs `.dyn_hi`
  and breaks the next `live-test-dev` with "Dynamic hash doesn't match". Recover with
  `cabal build monoscope-shared monoscope-cli --ghc-options="-O0"`.
