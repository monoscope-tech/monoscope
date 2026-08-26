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

- [ ] **Attach the metered prices to live subscriptions, then flip `ENABLED_USAGE_METERS`.**
      All 9 Stripe subscriptions today carry exactly two items: the base price and the
      `events_usage` overage. Nothing bills metrics or replays until an item for those prices
      is added to each. **Attach before flipping** — Stripe aggregates a meter over the whole
      billing period, so enabling the meter first and attaching later can bill a customer for
      usage recorded before the item existed. This raises live customers' bills, which is why
      it is not done unprompted. `ENABLED_USAGE_METERS` is a deployed env var, so flipping it
      is a CapRover update — and `appDefinitions/update` is a FULL REPLACE, so send the whole
      definition back.
- [ ] **Decide what LemonSqueezy customers are charged.** LS is alive — 10 active
      subscriptions — but **a LS subscription carries exactly one subscription item and the
      API offers no way to add another**, so the three dimensions cannot be billed separately
      there the way they can on Stripe. The code already handles this safely: with no
      `billing_meter_items` row those meters stay dormant, so LS customers are billed for
      events exactly as before and nothing is silently wrong. The options are to leave LS on
      events-only, or migrate those customers to Stripe. Creating "LS metered variants" — the
      earlier instruction here — is not possible as written.
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

- [ ] **`make lint` is broken locally** — the installed hlint rejects `.hlint.yaml`
      (`Not allowed keys: asRequired qualifiedStyle importStyle`). Reproduces on untouched files,
      so it is a version mismatch, not a code problem. CI runs hlint in its own container and is
      unaffected, which is why it went unnoticed.
- [ ] **A dead `Data.Effectful.Hasql` import sits in `src/Pages/Telemetry.hs`** — zero uses, and
      `-Werror` carries no `-Wno-error=unused-imports`. CI builds pass, so it may be an
      instance-only import GHC false-positives on; worth one look rather than a blind delete.


Companion to [README.md](README.md) (the overnight batch) and
`timefusion/docs/plans/2026-08-11-certification-survival.md`.

## P0 — watch, because something just changed under production

- [ ] **Verify the two deploys landed.** monoscope `5f23b3946` (code-context / github
      credentials, includes migration **0124**) and TimeFusion `0308015`+ (dedup certification
      persistence). ~28 min each. Migration 0124 is destructive-ish — it drops
      `code_mappings.github_sync_id` after carrying rows across — so confirm the runner
      actually reached it rather than halting on an earlier checksum mismatch.
- [ ] **Watch `count(*)` correctness for a day.** TF certification persistence is now ON, which
      makes the read-side dedup skip fire at a real rate for the first time. The skip removes
      `DedupExec`; if the sweep's certification rule has a latent flaw, this is what surfaces
      it. Compare a few dashboard counts against a known reference.
      Kill switches, in order: `timefusion_dedup_certification_persist=false` (cold cache per
      process) → `timefusion_read_dedup_skip_swept=false` (removes the skip entirely). Reach
      for the second if a count is doubted.
- [ ] **Confirm the parity guard is meaningful now.** `count_is_identical_with_and_without_the_dedup_skip`
      only began genuinely exercising the skip today — before that it ran without a time bound
      and never engaged it. The rule is now covered by a test that is hours old, not by a
      production track record. That is the actual risk in the item above.

## P1 — the measurement everything else waits on

### First read (2026-08-11, ~20 min after deploy — NOT the real one)

| counter | value |
|---|---|
| `dedup_eligible` | 412 |
| `dedup_skipped` | 2 (0.5%) |
| `dedup_denied_never_certified` | 396 (**100%** of certification denials) |
| `dedup_denied_fp_moved` | **0** |
| `dedup_denied_no_window` / `_unresolved` / `_disabled` | 0 / 10 / 4 |
| `cert_granted_total` | **0** |

Only 1360 scans total, so this is a young process and **not** the ≥24h reading. Two things
are worth noting anyway:

1. **It contradicts the prediction this plan wrote down.** The doc expected `fp_moved` to
   dominate — partitions being written to continuously, nothing to recover, *stop*. So far
   `fp_moved` is flat zero and `never_certified` is 100%. If that holds, the prize is real and
   the exit criteria say proceed rather than stop.
2. **`cert_granted_total = 0` is the thing to explain.** The sweep has certified nothing at all
   in this lifetime — so partitions are not being certified-then-invalidated, they are simply
   never certified. That is either "the process is 20 minutes old and the confirming pass
   hasn't come round yet", or something still blocking certification that the sweep fix did not
   cover. Distinguishing those is the next real question.
   (`dedup_skipped = 2` with zero grants is consistent with the two skips coming from
   certifications *loaded off disk* — i.e. persistence working — but at n=2 that is a guess.)

### Second read, ~15 min later (a DIFFERENT process — TF restarted in between)

`dedup_skipped` fell 2 → 1 while `total` rose 1360 → 2196; counters are monotonic within a
lifetime, so that is a restart, not a decline. Which is itself the finding that Phase 0 keeps
running into: **TF redeploys often enough that a clean 24h window has to be arranged, not
waited for.**

| counter | value |
|---|---|
| `dedup_eligible` | 652 |
| `dedup_skipped` | 1 (0.2%) |
| `dedup_denied_never_certified` | 625 |
| `dedup_denied_fp_moved` | **0** |
| `cert_granted_total` | **4** |
| `cert_dwell_total` | **0** |

**This reframes the problem, and away from what Phase 1 built.** Certification *does* happen —
4 grants in a young process — and `cert_dwell_total = 0` says not one of them has been
invalidated yet. Nothing is dying, so there is nothing for *survival* to save. What is missing
is **coverage**: the sweep certifies a handful of partitions while queries touch hundreds, and
625 denials are partitions it simply never reached.

If that holds over a full day, the lever is **which partitions get swept** —
`timefusion_dedup_lookback_days`, the sweep's per-tick budget, and whether the partitions
queries actually hit are inside its scope at all — not persisting verdicts that are not
expiring. Persistence and the confirming-pass fix are still correct and still cheap; they just
may not be where the 99.5% is.

- [ ] **Re-read after ≥24h with no TF deploy** and confirm or kill the above. The two numbers
      that decide it: `cert_granted_total` (is the sweep covering a meaningful share?) and
      `cert_dwell_total` vs `dedup_denied_fp_moved` (is anything expiring at all?). If dwell
      stays near zero while `never_certified` stays huge, close out the survival plan and open
      a coverage one.
- [ ] **Read Phase 0.** After **≥24h with no TF deploy**:
      ```sql
      SELECT key, value FROM timefusion_stats WHERE component='scan' AND key LIKE 'dedup_denied%';
      SELECT key, value FROM timefusion_stats WHERE component='scan' AND key LIKE 'cert_%';
      ```
      Counters reset on restart, so diffing two scrapes is only valid inside one lifetime — and
      TF deploys several times a day, so this needs arranging deliberately.
      Decide from `dedup_denied_never_certified_pct` and `cert_dwell_p50_secs`:
      - `fp_moved` dominates → persistence earns little. Set the flag false and close it out.
      - `never_certified` still dominates → find out what is still blocking certification.
      - `dedup_skipped_pct` is now the headline. It was 0.2–0.5%; if it has not moved
        materially, persistence did not help and should be turned back off.

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
