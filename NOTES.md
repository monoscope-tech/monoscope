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

**Verified in production after the deploy (finished 2026-08-28 00:55 UTC):**
`QueryMonitorsCheck` failures were 11 in the 00:00 hour and **zero** since. All **13 of 13**
active monitors have a `last_evaluated` inside the last 45 minutes. Alerting is live again
after three weeks dark.

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

**Timing note:** the 00:00 UTC DailyJob on 08-28 ran *before* the deploy finished (00:55),
so it still emitted the old log lines — DSI-APP logged "Reporting usage" at 00:00:23 and
again did not advance, consistent with the empty sub-item. The new `Counting usage` /
`Usage reporting skipped` lines will first appear at the **next** 00:00 UTC run, which is
what will finally discriminate Talstack Prod. Nothing to do but read the log then.

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

**Branch state:** full integration suite **757 examples, 0 failures, 28 pending**;
library compiles clean (125 modules, 2 pre-existing warnings); fourmolu + hlint clean.
Not merged — review and merge when the other session's work settles.

### Reviewed

- `ProcessMessage.hs` — **no findings**. Dense but genuinely well-built: doctests on every
  pure helper, rationale comments, idiomatic combinators. Forcing changes would be churn.
- `BackgroundJobs.hs` — `processBackgroundJob` went from **657 lines to 88**, 45 arms.
  Most already delegated to named functions; nine did not. Now extracted, each with a
  signature: `reportUsageForProject`, `monoscopeAdminDaily` (172 lines inline!),
  `usageAuditReport` (75), `runDailyJobScheduling` + `withAdvisoryLock` (124 via a
  case-alternative `where`), `trialEndingReminder`, `cleanupDemoProject`,
  `errorAssignedNotification`, `sendDiscordDataJob`.
  **Residual, stated honestly:** the rule is now "an arm delegates or is ≤7 lines" —
  seven arms are still 3–7 lines inline (`InviteUserToProject` is the longest at 7).
  Those read fine at the call site; extracting them would be ceremony.
  Payoff beyond tidiness: `reportUsageForProject` and `runDailyJobScheduling` — the two
  functions tonight's billing investigation centred on — are now top-level and testable.
- `Data/Effectful/Notify.hs` — a **file-wide** `-Wno-redundant-constraints` was hiding
  exactly one thing: `sendSlack` declared `Reader AuthContext` and never read it. An
  effect row is a capability claim, so that is a false claim. Constraint and pragma both
  gone; the file is clean under `-Werror=redundant-constraints`.
- `Pages/CodeContext.hs` — `nonBlank` was `Utils.nonEmptyT` re-implemented with `guarded`
  instead of `mfilter`. Deleted; 8 call sites use the shared one.
- **No findings** (reviewed, nothing worth changing): `Web/ApiHandlers.hs` (103 small
  functions, already has `withRefetchNoContent`/`notFoundOr` helpers), `Pkg/LiveTail.hs`,
  `Pages/Replay.hs`, `Models/Apis/Issues.hs`, `Pkg/EmailTemplates.hs`. Their `_ ->`
  catch-alls are all on `Text` or on byte/char parsing — open domains, which the
  guidelines explicitly allow — and their `String`s are `Either String` from aeson/Minio
  at the boundary.

### Honest assessment of "we shipped with no regard for code quality"

The evidence does not support that. Across the whole tree: no `fromJust`/`head` outside
`Devel.hs`, every remaining manual `ToJSON`/`FromJSON`/`FromField` instance is an orphan
for a foreign type that *cannot* be derived (`Either`, `CI Text`, `ByteString`, `Map`),
only eight warning suppressions exist and seven were justified, and the long functions
are mostly long *type definitions*, not long bodies. The real finds tonight were three
narrow ones, all now fixed. What the codebase is short of is not tidiness but **tests on
the revenue path** — before tonight the billing gate had coverage that tested `NULL`
where production failed on `""`.

### Identified, deliberately NOT done

**1. `queryEditorInitializationCode` (LogQueryBox.hs) — ~180 lines of JS in a Haskell
string.** It defines nine `window.*` globals and interpolates only three Haskell values
(`$vizType`, `$schemaUrl`, `$popularQueriesJson`), so it looks like a clean move to the
bundle. It is not: the block re-executes per render on HTMX morph swaps and relies on
`customElements.whenDefined` plus proximity to `#filterElement`. A bundle module
initialises once, so moving it means rebuilding the Explorer's re-init lifecycle — a
rewrite, not an extraction. Needs a browser to verify; chrome-devtools was disconnected
and jsdom cannot exercise morph swaps. **Do this one with the app open.**

**2. Three byte formatters, three different outputs.**
`Telemetry.humanBytes` → `3KB` (integer div, 1024 divisor but decimal label),
`Settings.humanBytes` → `1.5 KB` (same mislabel), `Containers.formatBytes` → `1.5 KiB`
(correct binary labels, scales to PiB). `formatBytes` is the right one to keep. Merging
changes user-visible text in three places, and two of the three files are being edited
by the other session, so it wants a deliberate decision rather than a 5am sweep.

**3. Same-name-different-module pairs that are NOT duplicates** (checked, leave alone):
`linkButton` in Bots/Utils vs Bots/Discord are Slack and Discord payload shapes;
`parseStackTrace` in StackTrace vs ErrorFingerprint return different types.

**4. `ratio` and `plainCell` are true duplicates** between `Models/Telemetry/Containers.hs`
and `Pages/Infrastructure.hs` (identical signatures; Infrastructure already imports
Containers). Left alone only because both files are open in the other session.

### Two things I owe you plainly

- **The ReportUsageSpec extension pins behaviour; it is not proof of a fix.** It passes
  against both the old and the new code, because the code always skipped an empty sub
  item — what was missing was the *log line*. Its value is stopping someone "fixing" the
  gate by treating `""` as a valid subscription item.
- **I may have killed your other session's `live-reload`.** My `pkill` pattern for the
  worktree watcher (`cabal repl monoscope --no-semaphore`) matches the main checkout's
  command verbatim. If that pane is dead, `make tmux-live-reload` in the main tree.

### Notes for whoever continues

- The worktree watcher is **compile-only on purpose**. `make live-reload` there runs
  `--test :run Start.startApp`, which would boot a second server against the **prod**
  `DATABASE_URL` in `.env` and join the prod Kafka consumer group.
- Running `cabal test doctests` desynchronises `monoscope-shared`'s `.hi` vs `.dyn_hi`
  and breaks the next `live-test-dev` with "Dynamic hash doesn't match". Recover with
  `cabal build monoscope-shared monoscope-cli --ghc-options="-O0"`.
