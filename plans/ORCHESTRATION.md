# Parallel workstream orchestration (started 2026-08-25)

Baseline commit: `c71ec614f`. Master is 7 commits ahead of origin — **not pushed = not deployed**.

## Streams

| Stream | Branch | Location | Migration no. | Status |
|---|---|---|---|---|
| Dashboards fix + e2e | `master` | main checkout | none | **done** — see below |
| Billing / new pricing | `ws-billing` | `.claude/worktrees/ws-billing` | 0136 | in progress |
| Container monitoring | `ws-containers` | `.claude/worktrees/ws-containers` | 0137 | in progress |
| Metric exemplars | `ws-exemplars` | `.claude/worktrees/ws-exemplars` | 0138 | in progress |

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
