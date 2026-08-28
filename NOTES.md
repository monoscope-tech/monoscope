# Overnight session notes — 2026-08-27/28

## TL;DR

1. **Alerting was dark for three weeks and is now fixed and verified live** — 13/13 monitors
   evaluating, zero `QueryMonitorsCheck` failures since the 00:55 UTC deploy.
2. **New bug found, not fixed:** the daily-scheduling advisory lock leaks on every run and
   is held in prod right now. When it doesn't clear, a whole day of job scheduling
   (including all usage metering) is silently skipped. Written up in full below — the fix
   is a billing-adjacent decision, so it waits for you.
3. **Two billing decisions waiting on you:** DSI-APP and TestCorp have
   `first_sub_item_id = ''` and have gone unbilled since 2026-07-17. Populating either
   starts real invoicing, so I left both alone.
4. **Deadline: ~08-31.** After that the 7-day retention clamp makes the 08-23/08-24 gap
   permanently unbillable.
5. **Talstack Prod is still unexplained**; the diagnostic log I shipped lands at the next
   00:00 UTC run.
6. **`refactor-sweep` branch pushed, green** (757 examples, 0 failures) — not merged.

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

## Refactor sweep — branch `refactor-sweep` (pushed to origin, NOT merged)

Working in a worktree because the other session is editing `Dashboards`, `Anomalies`,
`Monitors`, `Settings`, `Infrastructure`, `Containers`, `BodyWrapper`, `ServiceMap`,
`Widget`, `ApiSpec`, `LogSpec` in the main checkout.

**Two integration failures in the main tree are theirs, not mine** — `ApiSpec:46` and
`LogSpec:784` assert `aria-label`s whose source changes have not landed yet.
Full suite otherwise: 758 examples, 2 failures, 28 pending.

**Branch state:** full integration suite **757 examples, 0 failures, 28 pending**.
(The main checkout reports 758 with 2 failures — the extra example and both failures are
the other session's uncommitted spec edits, not something lost here: `ReportUsageSpec` has
16 `it` blocks before and after my change.)
Library compiles clean (125 modules, 2 pre-existing warnings); fourmolu + hlint clean.
Not merged — review and merge when the other session's work settles.

A later run in this worktree reported **766 examples, 10 failures** — all ten are
`Pages.GitSync / GitHub Sync E2E (Real API)`, which are gated on `GH_TEST_PAT` /
`GH_TEST_OWNER` / `GH_TEST_REPO`. `make worktree` copied `.env` in, so they *activate*
here and then 404 against a repo this machine cannot reach. They are pending (skipped) in
the main checkout, which is why its count is lower. Nothing I touched goes near GitSync.

### Reviewed

- `ProcessMessage.hs` — **no distill findings**; dense but genuinely well-built (doctests on
  every pure helper, rationale comments, idiomatic combinators) and forcing changes there
  would be churn. The *silent-drop* lens did find something in it later — see below. Worth
  remembering: "clean" is per-lens, not per-file.
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
- `Pkg/Queue.hs` + `ProcessMessage.hs` — the **silent-drop lens** found two places ingest
  loses messages without saying so; both now log. Pub/Sub's pull loop dropped any message
  with no ackId or no decodable payload (and never acked it either, so it redelivers
  forever). `processMessages` dropped messages via `runMaybeT`/`catMaybes` while acking
  them anyway, collapsing two different causes — free-tier quota (policy) and a missing
  project cache (a bug that loses data) — into one invisible number; it now uses `Either`
  with a `SpanDrop` reason. Note this is the *same file* the distill lens called clean:
  different lens, different find.
  Two caveats on that change, so the morning read is accurate: `NoProjectCache` is
  **defensive and unreachable today** — `projectCaches` is built over `ordNub` of every pid
  in the batch with a `defaultProjectCache` fallback, so the map is total; it exists to make
  the attention log meaningful if that construction ever changes. And severity follows the
  reason, not the count: quota drops log at `info` (a free-tier project over its cap sends
  all day, and paging on every batch is the same failure as saying nothing), only a missing
  cache is `logAttention`.
- **Tests added** (two, both on paths where being wrong costs money or data):
  `ReportUsageSpec` now covers `first_sub_item_id = ""` as well as NULL and asserts the
  watermark does not move; `ProcessMessageSpec` pins that a free-tier project over its daily
  cap has its batch **acked in full with nothing stored** — deliberate policy that is
  otherwise indistinguishable from data loss.
- `Opentelemetry/OtlpServer.hs` — **looked at, deliberately left.** `convertMetricToMetricRecords`
  drops histogram datapoints via `mapMaybe` when `len(bucket_counts) /= len(explicit_bounds)+1`.
  That rejection is *correct* — the datapoint violates the OTLP spec and cannot be stored — but
  it is silent, and the function is pure and on the hot path, so signalling it means threading
  an effect or returning a reject count through the conversion. Worth doing; not worth doing
  blind at 04:30.
- **No findings**: `Pkg/Parser.hs`, `Models/Projects/Projects.hs`, `Pages/Telemetry.hs`,
  `Pages/Onboarding.hs`, `Pkg/SchemaLearning/Worker.hs`, `Models/Apis/LogPatterns.hs`,
  `Pkg/Drain.hs`, `Models/Apis/ErrorPatterns.hs`. Every `mapMaybe` on these paths was
  checked against the silent-drop lens and each is correct — the one that matters most,
  `splitUsageIntoChunks` (billing), drops only zero-quantity chunks via a smart constructor
  and is precisely doctested for it.
- **No findings** (reviewed, nothing worth changing): `Web/ApiHandlers.hs` (103 small
  functions, already has `withRefetchNoContent`/`notFoundOr` helpers), `Pkg/LiveTail.hs`,
  `Pages/Replay.hs`, `Models/Apis/Issues.hs`, `Pkg/EmailTemplates.hs`. Their `_ ->`
  catch-alls are all on `Text` or on byte/char parsing — open domains, which the
  guidelines explicitly allow — and their `String`s are `Either String` from aeson/Minio
  at the boundary.

### One type-safety change I checked and rejected

`LemonSub.projectId :: Text` looks like primitive obsession next to the `ProjectId`
newtype — but `apis.subscriptions.project_id` is a **`text`** column, so typing the field
as `ProjectId` (a `UUIDId` over `UUID`) would fail to decode at runtime. That is precisely
tonight's `last_evaluated` bug pointed the other way: the Haskell type has to match what
the column actually holds. A real fix is a migration on a billing table for the Lemon
Squeezy path we are migrating off, which is not worth it. Left as `Text`, deliberately.

### Honest assessment of "we shipped with no regard for code quality"

The evidence does not support that. Across the whole tree: no `fromJust`/`head` outside
`Devel.hs`, every remaining manual `ToJSON`/`FromJSON`/`FromField` instance is an orphan
for a foreign type that *cannot* be derived (`Either`, `CI Text`, `ByteString`, `Map`),
only eight warning suppressions exist and seven were justified, and the long functions
are mostly long *type definitions*, not long bodies.

What it *is* short of is **observability on the paths where being wrong costs money or
data**, and that is where every real find of the night came from: alerting silently dead
for three weeks, two ingestion paths dropping messages without a word, a billing gate that
skipped without logging, and an advisory lock that leaks on every daily run. None of those
are tidiness problems, and none would have been found by a distill pass. Related: before
tonight the billing gate's only spec tested `NULL` where production failed on `""`.

## ⚠ NEW BUG FOUND (confirmed in prod) — the daily advisory lock leaks every run

Found by hoisting `withAdvisoryLock` out of `DailyJob`'s `where` and asking whether it was
safe to reuse. It is not, and the existing use is already broken.

`Hasql.statement` goes through `OHasql.useStatement pool` — **a connection is checked out
of the pool per statement**. `pg_try_advisory_lock` is *session*-level, so the lock is taken
on connection A and the unlock runs on connection B, which does not hold it. Prod logs show
the pair on every single daily run:

```
2026-08-28 00:00:02  "Running daily job"
2026-08-28 00:00:39  "Advisory unlock returned FALSE (lock was not held by this session)"
2026-08-27 14:43:08  "Running daily job"
2026-08-27 14:43:22  "Advisory unlock returned FALSE (lock was not held by this session)"
```

The unlock never succeeds. The lock stays held on connection A until the pool recycles it.
**It is held right now** — `pg_locks` has advisory objid `2654377941`, which is
`hashtext('daily_job_scheduling')` = `-1640589355` read unsigned, on pid 1573144 (backend
started 2026-08-27 23:35).

**Consequence:** if the acquiring connection is still in the pool at the next 00:00 UTC,
`pg_try_advisory_lock` returns false and the run logs *"Daily job already running in another
pod, skipping"* — and the **entire day's scheduling silently does not happen**: no
`ReportUsage`, no per-project job seeding. That is an intermittent, self-clearing failure
that depends on pool recycling, which fits both the erratic metering coverage investigated
tonight and the older "per-project jobs were never seeded" incident.

**Deliberately not fixed unattended.** Getting a distributed lock wrong in the *other*
direction lets two pods schedule the same day concurrently, which means duplicate
`ReportUsage` jobs — double metering. That is a worse failure than a skipped day and it is
billing, so it wants review.

**Fix sketch:** acquire, run and release on one explicitly checked-out connection — the
`liftIO $ withResource authCtx.jobsPool \conn -> …` pattern this file already uses
elsewhere — which needs an unlift (`withEffToIO`) to run the `Eff` action inside. Or drop
advisory locks for a row in a table with an owner and a TTL. Either way it deserves a test
that asserts the lock is *released* after `withAdvisoryLock` returns (query `pg_locks`),
because the current bug is precisely a missing release.

**Operational note for this morning — still held as of 02:37 UTC**, ~3 hours after the
backend started, so the pool has not recycled it. If it survives until 00:00 UTC the next
daily run skips, which means no metering that day — and that directly worsens the 08-31
clamp deadline above. I did not clear it: `pg_terminate_backend` on a live pooled
connection aborts whatever it is running, and there were ~21 hours of margin for the pool
to recycle it on its own. Check and decide:

```sql
SELECT pid, backend_start, state FROM pg_locks l JOIN pg_stat_activity USING (pid)
WHERE l.locktype='advisory' AND l.objid = 2654377941;   -- hashtext('daily_job_scheduling')
```

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
