# Quality sweep — worklist and findings

Branch: `quality-sweep-2026-09-02`, from `master@44c38b1c`.

## Verification rules (learned the hard way)

- **Never run `cabal build`/`cabal test`.** A ghcid watcher owns the build; a second
  cabal process races it and produces phantom errors.
  Start the watcher type-check-only (NOT `make live-reload`, which kills port 8080 and
  disrupts the other checkouts on this machine):
  ```
  ghcid --command 'cabal repl monoscope --no-semaphore \
    --ghc-options="-j3 -O0 -Wno-error=unused-imports -Wno-error=unused-top-binds"' --warnings > build.log 2>&1
  ```
- Read the verdict with `GHCID_WAIT_TIMEOUT=1800 GHCID_MIN_MODULES=120 ./scripts/local/ghcid-wait.sh`.
  It handles ghcid's three false-green modes (stale title, in-flight reload, and
  `All good (N modules)` printed right after `Failed, unloaded all modules` on an import cycle).
- **Do not run parallel editors against one watcher.** With several agents editing
  different files, a red build cannot be attributed to any one of them. Batch edits,
  then verify once.

## Why the integration suite fails under concurrent checkouts

Several checkouts of this repo share one PostgreSQL server. `Pkg.TestUtils` clones a
server-wide template database, `monoscope_test_template`, and other checkouts' suites
drop and recreate it. A run that starts while another is recreating it dies with
`SqlError 3D000: template database "monoscope_test_template" does not exist`, and the
failure count (9, 72, …) is a function of timing, not of the code under test.

Treat 3D000 as environmental. Get one clean full pass before merging, and prefer
`TEST_MATCH`-scoped runs while other checkouts are active. Note that `make test-integration`
passes `--jobs=$(NCPUS)`, which a hand-rolled `cabal test` omits — another reason not to
hand-roll one.

## Bugs found by survey (fix test-first)

1. **`isIssueWorthy` / `isAlertableLogLevel` had drifted** — `BackgroundJobs.hs:4966` vs `:4835`.
   The new-pattern path was case-*sensitive* and accepted only `ERROR`/`WARN`; the
   rate-change path was case-insensitive and also accepted `WARNING`/`FATAL`/`CRITICAL`.
   A pattern with `logLevel = "fatal"` fired rate-change alerts but never opened an issue.
   The comment at `:4869` claimed the two mirrored each other. **Fix in progress**; regression
   guard `isIssueWorthy_levelGate_mirrorsIsAlertableLogLevel` asserts the two agree across
   the whole level set.
2. **Double slash in query-monitor alert URLs** — `BackgroundJobs.hs:2885,2897` use
   `hostUrl <> "/p/"`, but `hostUrl` already carries a trailing slash; every other site in
   the repo uses `hostUrl <> "p/"` (`Pages/Reports.hs:195`, `Settings.hs:916,1066,1486`,
   `Projects.hs:1157,1405`, `Bots/Slack.hs:331`). Fix by routing all 7 sites through one
   `issueUrl` builder.
3. **Undocumented digest reason** — `BackgroundJobs.hs:2029` passes `"runtime_exception"`,
   absent from the enumeration documented at `:1914`. `digestReason` is a `Text` with its
   values in a comment; it wants a 4-constructor sum type.

## Hazards (zero lines, real risk)

- **`instance Eq ZonedTime where (==) _ _ = True`** — `Pkg/TestUtils.hs:496-499`. An orphan
  that makes every `ZonedTime` field compare equal. `Issues.Issue` has three of them, so any
  test comparing whole `Issue` records passes vacuously on those fields. Replace with a
  `newtype ZT` deriving `Eq` via `zonedTimeToUTC`.
- **`selectIssueById` is unscoped** — `Models/Apis/Issues.hs:390` vs `selectIssueByIdScoped:752`.
  Differ only by `AND project_id = #{pid}`. A latent cross-tenant read; keep the scoped one.
  (Same class of bug as the monitors bulk-mutation authz hole fixed on the previous branch.)
- **`linksJson` emits Haskell `Show` syntax, not JSON** — `OtlpServer.hs:1195` uses
  `show $! AE.toJSON …` while the adjacent `eventsJson` does not. Flagged, not fixed —
  changing it alters ingest output and needs its own test.
- **Quarantine window duplicated in two SQL literals** — `Endpoints.hs:587-594` and `:598-610`
  both inline `applied_at > NOW() - INTERVAL '24 hours'`. Change one and the merge-delete
  path disagrees with the challenge UI.

## Consolidation worklist, ranked

### Pages (~250 lines)
1. `emptyState_` already exists (`Components.hs:48`) and covers every variant found —
   ~16 sites hand-roll it. `Infrastructure.hs:925` is a verbatim duplicate of `:263`.
2. Nav tab bar — 6 sites, no canonical component; propose `navTabs_`.
3. Card + uppercase section header — 10 sites inside `Anomalies.hs` alone, plus `Projects.hs:950`.
4. Severity banner — 8 sites; `infoBanner_` exists but is brand-only, needs a severity param.
5. Underline radio tab label — 5 sites duplicate `detailTab_`'s class string verbatim.

Verified clean, do not touch: modals, pagination, form fields, TimePicker, LogQueryBox,
page header chrome (all already centralized).

### BackgroundJobs.hs (~240 lines)
Runtime-error alert pipeline ×3 → `createAndNotifyErrorIssue` (~55); channel fan-out ×3 (~40);
`WeeklyReportData` build duplicating `Pages/Reports.renderWeeklyEmail` field-for-field (~38);
`withResource`+`createJob` ×22 → `enqueueJob` (~35); health-check `([Text],[Text])` ×5 →
`[HealthFinding]` (~20); relative-time helpers ×3 → `Utils` (~10); `sendMessageToDiscord`
bypasses the `Notify` effect (~8).

Separately: `processBackgroundJob` is 658 lines because five arms carry inline bodies
(`MonoscopeAdminDaily` 172, `ReportUsage` 133, `DailyJob` 128, `UsageAuditReport` 74).
Extracting them deletes nothing but drops the dispatch table to ~230 lines.

### Handlers / models (~250 lines)
DTO shovel converters ×11 (~60-90); generic patch merge ×5 (~50); `selectIssues` vs
`selectIssuesByFilters` fork (~25); untyped `V.Vector (V.Vector AE.Value)` row matrix and the
five `lookupVec*` helpers that exist only to index it (~25); parallel badge/colour tables in
`Utils.hs` vs `Telemetry.hs` (~20); `archiveHosts`/`unarchiveHosts` boolean fork (~10).

Reinventions of library functions: `TestUtils.hs:1510` `lookupParam` is `Prelude.lookup`;
`:491` `fromRightShow` is `either (error . show) id`; `:1518` `pBool` is `FromHttpApiData Bool`;
epoch conversion has 4 spellings; hour truncation has 2+.

### Typing
`IssueStatus` is destroyed into `Maybe Bool -> Maybe Bool` at `ApiHandlers.hs:903`;
`Plan` exists but 5 functions take bare `Text`; bare `UUID` where `UserId` exists (4 handlers);
no `LogPatternId` newtype; 14 anonymous wide tuples where the file already shows the fix
(`GroupReview`, `MetricCatalogPage`, `UserBilling` are records with `HI.DecodeRow`).

## Scale, honestly

Identified so far is roughly 750-900 deletable lines against 75,133 in `src/` — about 1%.
The previous branch's breadth-first pass netted -598. A mature codebase does not hold
30% dead weight; "drastic" here realistically means low thousands of lines plus the
uniformity and typing wins, which matter more than the line count.
