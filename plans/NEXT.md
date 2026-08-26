# Next steps — as of 2026-08-26

Closed since the 08-11 edition:

- **Certification measurement (was P1).** Answered from the code rather than the 24h window it
  asked for: the sweep can only certify today and yesterday
  (`timefusion_dedup_lookback_days`, default 1) while dashboards query 7-30 days, so
  `never_certified_pct = 100` is structural and no reading would have moved it. Survival plan
  closed, coverage plan opened — `timefusion/docs/plans/2026-08-12-dedup-certification-coverage.md`.
- **`fetchSnippet` had no cache (was P2).** Blobs are now cached on `(owner, repo, ref, path)`
  in `AuthContext`, so a stack trace costs one fetch rather than one per frame. Not via
  `Pkg.QueryCache` as suggested here — that is a timeseries cache and fits badly.
- **`plans/06-tf-dedup-skip-incident.md` was stale (was P2).** Marked resolved: TF `f8d7278`.
- **CapRover `start-first` (was TF P0).** Changed to `stop-first` via the CapRover API; every
  other field of the app definition preserved. `"FailureAction": "pause"` is still set, and
  still explains the wedge shape — left alone as a separate call.
- **Health-probe blindness (was TF P0).** The probe now reports per-stage timings and a
  runtime scheduling-lag sampler landed, so the next `exit 137` says which stage was slow and
  whether workers were starved. TF `d50d2e4`. The measurement is instrumented but **not yet
  read** — that needs a real incident or a loaded window.

Everything below is open. Ordered by "what bites first if ignored", not by size.

## From the 2026-08-25 parallel streams (dashboards, exemplars, containers, billing)

All four shipped to production. Each stream's plan doc carries its full reasoning; these are the
open threads pulled out of them so they are findable from here.

### P0 — billing: Stripe is wired end-to-end, two decisions are yours

Done 2026-08-26 (`plans/billing-pricing-v2.md` has the design):

- **Stripe meters and prices exist**, live and test. `session_replays_usage` was created
  with a "Session replays" price at `0.1` cents/unit — \$1 per 1,000.
- **Metric datapoints reuse `metrics_usage`**, which already existed in the account with a
  "Metrics" product priced at `0.00001` cents/unit — exactly the \$1 per 10M we want, and
  attached to nothing. The code named a meter that did not exist; it now names that one, and
  the duplicate created while working this out has been deactivated.

Still open, and both are genuinely yours rather than engineering:

- [ ] **Attach the metered prices to the live Stripe subscriptions.** All 9 carry exactly
      two items today: the base price and the `events_usage` overage. Metering is on by
      default now (the `ENABLED_USAGE_METERS` flag was removed — too many envs, and the
      real off-switch is provider-side), so meter events for metrics and replays are being
      sent. **A Stripe meter with no price attached receives events and bills nothing**,
      so nobody's Stripe bill has moved. Adding an item is what starts charging, and it is
      also the moment to watch: Stripe aggregates a meter over the whole billing period, so
      attaching mid-cycle can bill for usage already recorded that period. Attach at a
      period boundary, or accept that first invoice.

- [ ] **Decide what LemonSqueezy customers are charged.** LS is alive — 10 active
      subscriptions — but **a LS subscription carries exactly one subscription item and the
      API offers no way to add another**, so the three dimensions cannot be billed separately
      there the way they can on Stripe. The code already handles this safely: with no
      `billing_meter_items` row those meters stay dormant, so LS customers are billed for
      events exactly as before and nothing is silently wrong. The options are to leave LS on
      events-only, or migrate those customers to Stripe. Creating "LS metered variants" — the
      earlier instruction here — is not possible as written.

      **Recommendation, from the numbers (2026-08-27): migrate DSI-APP, leave the rest.**
      Only **5** paying projects are on LS, and only **two** have any volume at all in 90
      days: DSI-APP (675M requests / 675M metric datapoints) and Engine/API Prod (12.5M /
      8.6M); the other three are flat zero. That matters because metric datapoints *ride the
      events item* on LS and so bill at the events rate — `$1/1M` rather than `$1/10M`, about
      **10x what a Stripe customer pays** (`resolveMeterTarget`, and the comment above it says
      so deliberately). At DSI-APP's ~225M datapoints/month that is ~\$225/mo billed where
      Stripe would charge ~\$22.50. So enabling `MetricDatapoints` while DSI-APP sits on LS
      overcharges the one customer it actually affects. Moving that single subscription to
      Stripe removes the whole problem; the remaining four bill zero metrics either way and
      can stay on events-only indefinitely. See `dsi_app_metric_volume_billing_risk` in memory
      for the volume history.
- [ ] **Pricing copy stays unchanged until the attach happens**, or we display prices we do
      not charge. App: `Pages/Settings.hs`, `Pages/Components.hs`, `Pages/Onboarding.hs`.
      Landing: `pricing/index.md`, `index.md`, `assets/js/main.js`. All say events-only
      "\$1 per 1M".
- [ ] **Dormant-window usage is not backfilled.** It accrues in `apis.daily_usage` but cuts no
      chunks, so enabling a meter bills from that day forward. Anything owed for the dormant
      period is a deliberate manual reconciliation — buffering and draining it is the shape
      that leaked on a previous provider switch.

**The LS API key in `.env` is stale** and 404s on every subscription; the working one is in
`.env.prod`. Reading the wrong one makes LS look dead.

### P1 — the follow-up each stream named as its next step

- [x] ~~**Exemplars: scope the related-metrics charts to the span's own interval, shaded.**~~
      Done 2026-08-26. `Widget` carries its own `from`/`to` plus a highlight interval; a
      widget with them queries that window instead of the page's. Browser verification against
      production caught the follow-on: a 4.8ms span inside its padded 4-minute window is
      0.002% of the chart width, so below 1% the overlay is a dashed line at the instant
      rather than a band nobody can see. Original text:
      `plans/exemplars-correlation.md` §4.2 calls this the first follow-up. The charts currently
      inherit the explorer's time range because `Widget` has no `from`/`to` of its own. The
      vendor survey is unambiguous that the interval overlay is what makes the chart worth
      showing at all, so this is the difference between shipped and useful.
- [ ] **Dashboards: stop a required variable blocking the whole dashboard.**
      `plans/dashboard-variable-defaults.md`. Today an unset required variable covers the page
      with a modal, and dismissing it leaves every widget empty, because an unset variable
      interpolates to `''` rather than to a match-all. Datadog defaults template variables to
      `*` and never blocks. Needs a clause-level placeholder (the way `{{time_filter}}` already
      works) plus a template migration — scope it to `variable.required` and leave
      `tab.requires` blocking, since a drill-down tab genuinely has no all-values rendering.

### P2 — verification the streams could not do themselves

- [ ] **Nothing in the exemplars UI is browser-verified.** Booting the server locally migrates
      production, so the agent could not click through it. The Exemplars tab reveal was hardened
      with a click-triggered fallback rather than left to `intersect` (a `display:none`
      container cannot intersect), but the chart-in-a-lazy-fragment path wants real eyes.
- [ ] **Containers has no cluster facet** until collectors emit `k8s.cluster.name`; production
      currently sends only `k8s.cluster.uid`. Everything else deliberately out of v1 is listed
      in `plans/container-monitoring.md` §4.2 — no agent, no Orchestrator resource views, no
      Cluster Map, no images/SBOM, no throttling metrics, no container alerts.
- [ ] **Migrations 0137 and 0138 were reserved and never used.** Containers and exemplars both
      turned out to need no schema change. Do not renumber; just know they are free.

### P3 — hygiene from this session

- [ ] **`make lint` reports 22 hints, 0 errors.** Two corrections to what this entry used to
      say. It is not *broken*: hlint v3.10 accepts `.hlint.yaml` (the offending
      `asRequired`/`qualifiedStyle`/`importStyle` keys are gone), it just exits non-zero on
      any hint. And the "errors" seen once were transient parse failures from a concurrent
      session mid-edit in `src/`, not findings. Ten of the hints were unfixable: a custom rule
      mapped `Control.Exception.displayException` → `displayException`, but Relude re-exports
      that symbol, so it matched the already-correct bare usage and suggested replacing it
      with itself. That rule is deleted. The remaining 22 are ordinary
      (`viaNonEmpty`, `Hoist not`, `Consider count`, …), spread over 10 files — 7 of them sit
      in files another session has uncommitted, so clear those when the tree is quiet.
- [x] ~~**A dead `Data.Effectful.Hasql` import sits in `src/Pages/Telemetry.hs`.**~~ Removed in
      `22ed691ca`. GHC did flag it redundant and the module carries no orphan instances, so
      nothing depended on it being in scope.


Companion to [README.md](README.md) (the overnight batch) and
`timefusion/docs/plans/2026-08-11-certification-survival.md`.

## TimeFusion dedup certification — closed, see the coverage plan

The P0 watch items and the two partial counter reads that used to sit here are done or
superseded. Both deploys they asked about landed: monoscope `5f23b3946` is on master and
migration 0124 applied (the `code_mappings.github_sync_id` column is gone; the runner has since
reached 0137). The measurement itself was answered from the code rather than the 24h window —
the sweep can only certify today and yesterday (`timefusion_dedup_lookback_days`, default 1)
while dashboards query 7-30 days, so `never_certified_pct = 100` is structural. Survival plan
closed; the open work is
`timefusion/docs/plans/2026-08-12-dedup-certification-coverage.md`.

The kill switches are still worth knowing, in order:
`timefusion_dedup_certification_persist=false` (cold cache per process) →
`timefusion_read_dedup_skip_swept=false` (removes the skip entirely).

## P2 — known-incomplete work from the overnight batch

- [ ] **Issue view still renders a raw `<pre>` stack trace.** `Pages.Anomalies` was left alone
      because `RuntimeException` issues carry a *synthesised* stack (see
      `haskell_backtraces_unavailable`), a different shape from a span's
      `exception.stacktrace`. Pointing `stackTrace_` at it without checking those strings would
      be guessing. Needs someone to look at real payloads first.
- [x] ~~**`fetchSnippet` has no cache.**~~ Done — `AuthContext.codeBlobCache`, 15-min TTL,
      successes only. Remaining nuance if it ever matters: the cache cannot tell a branch ref
      from a sha, so an immutable sha gets the same short TTL as a moving branch.
- [ ] **Source-map / debug-file symbolication is still out of scope.** Minified JS and stripped
      native frames resolve to nothing. `fetchSnippet` is the seam it plugs into. Only worth
      doing if customers actually hit it.
- [x] ~~**`plans/06-tf-dedup-skip-incident.md` is stale.**~~ Marked resolved in place; the
      reproduction steps are worth keeping since a narrowed projection dropping a column an
      exec node needs can recur with any new pushdown.

## P3 — hygiene and follow-ups

- [ ] **Three integration tests fail locally, pre-existing** (confirmed against `4aa402626`):
      the `preload="false"` assertion vs the shell's `hx-preload="false"`, and two that need a
      local TimeFusion. The first is a real (small) bug in either the test or the shell; the
      other two are environmental.
- [ ] **`object_store_cache::tests::test_basic_operations` is flaky in TF.** Failed once in a
      full parallel run, passed in isolation and on a clean rerun of the whole suite. Shared
      cache dir across parallel tests is the likely cause. Not caused by recent work.
- [ ] **Environment switcher is shipped but unexercised.** The TF column
      (`resource___deployment___environment___name`) is committed and the selector defaults to
      nothing selected, so no query filters on it yet. Worth sending real staging traffic
      through and confirming the picker actually partitions it.
- [ ] **Latency-column rethink was one pass, not a conclusion.** `plans/03` explored directions;
      the by-service/by-kind breakdown shipped, but the child-row UX at depth is still the
      weak part. Watch how it reads on a genuinely deep trace before iterating further.

## Standing risk, not a task

**Two Claude sessions share these working trees.** Earlier today a `git reset --hard` in the
TimeFusion tree discarded another session's uncommitted edits (`src/database.rs`,
`src/metrics.rs`, `tests/e2e/hot_tail_sorted_footer.rs`); committed work was safe, and
surviving stash commits `6650dd74` / `10a95a8e` / `58b3c53c` / `54bec045` still hold most of
it. It has not been confirmed whether anything was permanently lost.

Whatever the outcome there, the practice that follows is: **work in a `git worktree`, not in
the shared checkout** — which is how the certification work above was done, and why it could
be rebased onto the other session's rollup commits without either side losing anything.
