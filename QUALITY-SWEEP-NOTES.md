# Quality sweep — findings and open decisions

Branch `quality-sweep-2026-09-02`, from `master@44c38b1c`.
All three skills (`/hs-distill`, `/hs-lob-review`, `/hs-evasion-review`) applied across
feature groups: log explorer, service map, replay, bots, monitors, issues, endpoints,
billing, auth/server/MCP, the pattern/AI pipeline, and the shared UI components.

## Read this first — session summary (2026-09-04, overnight)

**Verified state:** integration suite `808 examples, 0 failures`; doctests
`1470 Tried, 0 Failures`; both compile targets green. Everything below is pushed.

**The premise, measured.** The sweep was asked to fix "so much verbosity and no code
reuse". Neither survives measurement:

- **Zero** duplicated blocks of 6+ lines anywhere in `src/` + `web-components/src`, with
  identifiers and literals normalised so renamed copies still match. The scan is sound
  because it decreases monotonically: 7 blocks at 4 lines, 3 at 5, **0 at 6**.
- The sweep's growth is **+411 lines of code**, not the +1811 first reported — that figure
  counted comments. The other +1480 is Haddock and doctests.
- `src/` is 69% code, 18% comment, 12% blank. The long functions are long because markup
  and SQL are verbose: `dashboardPage_` is 325 lines containing *one* repeated 4-line block.

**Where the real defects were: types, not size.** Each fix is a value carried as `Text`
where a sum type existed or belonged.

| commit | what it fixed |
|---|---|
| `a0be8e67` | `SessionSort` — sort silently fell through to `last_seen` |
| `1652d04b` | `IssueTab` — four independent matches, four separate fall-throughs |
| `7810598b` | `MonitorTab` — three sources of the same two literals |
| `6b60a3b6` | tab options derived from the enum (a merge had re-added the literals) |
| `7058d5c7` | `MonitorBulkAction` — **unknown action returned 200 and did nothing** |
| `5134a124` | `IssueSeverity` round-tripped through `Text` at five call sites |
| `3fc7c94c` | `isByosPlan` — two ad-hoc case-sensitive checks beside a case-folding sibling |
| `4d1cf27f` | deleted the dead `OtlpServerSpec`; verdict compared to a literal |
| `79182b4c` | group-review reply parsed twice behind a no-op `seq` |
| `4940a9f7`, `bb2f23fc` | `reportDayLabels` extracted and its doctests made to actually run |

`7058d5c7` is the one to read. Typing a single route parameter surfaced a third consumer
the compiler found (`action == "deactivate"`), which in turn revealed that
`bulkAction_doesNotTouchAnotherProjectsMonitors` iterated a hand-written list of four
actions — **`reactivate` and `unmute` had never been checked for cross-tenant access.**

**Consolidations deliberately NOT done**, each argued in its own section: the two
group-review pipelines (same shape, different control flow), the four remaining
bulk-action handlers (they fail *visibly*; only monitors was silent), `Endpoints.hs:240`
(a boolean projection, not a fall-through), `getErrorPatternById` (scoping it would delete
a cross-tenant signal), `writeTargetFor` (already doctested, uniform call sites),
`PlanName` (its spelling is a provider contract), and typing `GroupVerdict`'s field (would
turn a legacy row into a decode failure inside a background job). **Shape similarity
justifies consolidation only when the substance matches.**

**Needs a human:** the ack-vs-spike-detector question (paging behaviour, both readings
defensible); two remaining guard gaps with the test that would close each; four
client-tier demotions that need a browser to verify; and the ghcid fd leak in the sibling
`monoscope/` checkout (~250 MB per reload, ~1.3 GB held when last checked — restart that
watcher).

**Method warnings that each cost real time and produced a confident wrong answer:**
`git diff` emits no `+` lines under this repo's external diff driver (use `--no-ext-diff`);
doctests live in `src/`, so "no `test/` change" does not mean "no guard";
`cabal test doctests` builds the 197-module test-dev target and corrupts a running watcher;
a break-test must assert on the failure *reason*, never the count; a doctest count that
does not *rise* means the examples were never extracted; and four successive attempts to
measure function size by regex each looked plausible and were each wrong.

## Verification rules (learned the hard way)

- **Never run `cabal build`/`cabal test`.** A ghcid watcher owns the build; a second
  cabal process races it and produces phantom errors. Read the verdict with
  `GHCID_WAIT_TIMEOUT=1800 GHCID_MIN_MODULES=120 ./scripts/local/ghcid-wait.sh`, which
  handles ghcid's three false-green modes.
- **`0 examples, 0 failures` is not a pass.** `TEST_MATCH` silently matches nothing if
  the pattern is wrong. It is a substring match on the full spec path, and the Makefile's
  `:main --match $(TEST_MATCH)` form **cannot contain a space**. Bad patterns cost time
  three separate times tonight.
- **A filtered run of a `sequential`/`aroundAll` spec can fail for reasons unrelated to
  the code** — the fixture is seeded by an earlier example the filter excluded.
- **The library build cannot catch test-only breakage.** The test target compiles
  separately; a removed field can leave a live reference in a spec that `build.log`
  never sees.
- **`weeder` is currently unusable** — `~/.cabal/bin/weeder` is a dangling symlink into a
  pruned store entry. CLAUDE.md tells you to run it regularly; it has been silently
  unavailable. `cabal install weeder` will fix it, but not while watchers are running.

## Bugs fixed on this branch

Each has a regression guard unless noted.

1. **Cross-tenant read via `selectIssueById`** — it and `selectIssueByIdScoped` differed
   only by `AND project_id = #{pid}`. Same shape as the monitors bulk-action hole.
2. **Cross-tenant writes via the monitors bulk actions** (earlier branch, same class).
3. **`EnvConfig` derived `Show`** — ~15 secrets, embedded in `BWConfig` which also
   derives `Show`. One `show bwconf` in a log or exception printed the whole credential
   set. Now redacting. *No test — asserting on the absence of a leak is awkward; the
   instance is one line and self-evident.*
4. **`active` was DB-inert on monitors** — `queryMonitorUpsert` omitted `deactivated_at`
   from both the INSERT and the `ON CONFLICT SET`, so creating a monitor with
   `active: false` answered "disabled" while the row stayed live and kept firing.
5. **Load-more paging skipped rows** — the cursor epsilon was 1ms against an inclusive
   `timestamp <= cursor` bound, discarding up to 999µs of rows per page.
6. **Alerting whitelists had drifted** — `isIssueWorthy` was case-sensitive and missing
   `WARNING`/`FATAL`/`CRITICAL`, so `logLevel = "fatal"` fired rate-change alerts but
   never opened an issue.
7. **Double-slash alert URLs** — `HOST_URL` carries a trailing slash; three sites added
   another. Now one doctested `Utils.hostPath`.
8. **Dashboard "Create Dashboard" CTA was dead** — `ZeroState` collapsed an
   `Either Text Text` with `either id id`, so a modal id rendered as
   `href="newDashboardMdl"`.
9. **Syntax highlighting never re-ran after a swap** — the listener read
   `e.detail.elt`, which htmx 4 does not provide (the same file documents this 250
   lines earlier and uses `event.target`).
10. **`updateUrlState` defined twice with incompatible signatures**; the surviving one
    dropped the URL fragment.
11. **`navigateSpans` off-by-one** — advanced past the last span and fired
    `htmx.trigger('#trigger-span-undefined')`.
12. **`.waterfall-active` accumulated** on every row ever clicked.
13. **Timepicker labels disagreed on all eleven entries** between `sinceWindows` and
    `timePickerItems`, so the same control read differently per page.
14. **`chart.updateRollup` is implemented nowhere** and was guarded by `if chart exists`
    (the element, not the method), so it threw on every change.
15. **`Eq ZonedTime` was `_ == _ = True`** in TestUtils — every `ZonedTime` field of
    every record compared equal, so any test asserting on a whole `Issue` was vacuous on
    three fields. Now compares the instant; the full suite confirmed nothing was hiding.

## Open decisions — deliberately NOT actioned

These need a human call or a failing test first.

1. ~~**Basic auth mints a `persistent_sessions` row per request.**~~ — **FIXED; entry was
   stale, verified round thirteen.** `Web/Auth.hs:149-152` now reuses the cookie's session
   when it is live *and* belongs to the same basic-auth identity, falling back to minting
   only otherwise. The guard on identity is load-bearing: a cookie for any other user must
   not be adopted just because it exists.

2. **Two independent project-membership checks.** `Auth.hs:374` (CLI path) vs
   `Projects.sessionAndProject`. The API path is *stricter* — no sudo bypass, no stale
   refetch — so not a vulnerability, but they can drift.
3. **`flushDrainTask` writes orphaned `pat:<hash>` span tags.** Tags come from
   pre-Jaccard templates; rows are keyed on the merge survivor, so absorbed patterns'
   tags dead-end with no `log_patterns` row. `errByHash` likewise keys on pre-merge
   hashes and silently drops an absorbed pattern's `isError`. Every fix moves stored
   hashes.
4. **Two definitions of "placeholder"** — `PatternMerge` treats any `{…}` token as one,
   `Drain` only a closed set of ~19. A JSON blob is a placeholder for Jaccard merging
   but content for embedding normalisation, so the two stages rank the same pair
   differently.
5. **"Alphabetical" sort is a silent no-op** on the API-catalog and endpoints pages —
   the pages send `sort=name`, the model matches `first_seen`/`last_seen` and falls
   through to traffic order.
6. **`SortConfig`/`Features.sort` and `SortableConfig`/`Features.sortableColumns` are
   dead** — no caller sets either, so Dashboards' `withSort` columns render no sort
   affordance. Keep-and-wire or delete: a product call.
7. ~~**`apiMonitorPatch` never recomputes `logQueryAsSql`**~~ — **FIXED; entry was stale,
   verified round thirteen.** Both the create path (`ApiHandlers.hs:226`) and the patch
   path (`:348`) now go through the shared `compileAlertSql`, so a patched `query` can no
   longer leave stale compiled SQL behind.

8. **`AuthContext` carries `EnvConfig` twice** (`env` and `config`), set from the same
   value, used interchangeably, with nothing enforcing they stay equal.
9. ~~**`ErrorPatterns.getErrorPatternById` is unscoped**~~ — **RESOLVED as
   deliberately-unscoped; entry was also miscounted.** There are *four* callers, not three:
   the fourth is `BackgroundJobs.ErrorAssigned`, and it is the reason the query must stay
   unscoped. See "audited, deliberately left unscoped" below.

## Pattern hashes are NOT project-scoped (verified)

`patternHash = toXXHash dp.templateStr` (`BackgroundJobs.hs:2765`) hashes the template
string alone — no project id. Two projects logging the same message template therefore
produce the **same** `pattern_hash`, and that value becomes the issue's `target_hash`.

Endpoint hashes are different: `toXXHash $ projectId.toText <> host <> method <> urlPath`
(`ProcessMessage.hs:273`) embeds the project, so they cannot collide across tenants.

This asymmetry is why the unscoped `archiveAnomaliesAndIssues` cascade
(`WHERE target_hash IN (…)`) was a genuine cross-tenant write rather than a
theoretical one, and it is the reason any future query keyed on `target_hash` or
`pattern_hash` **must** carry a `project_id` predicate.

Audited after that fix: every `UPDATE`/`DELETE` in `src/Models` and `src/Pages`
carrying `ANY(#{…})` or keyed on those hashes. The remaining two unscoped writes are
not exploitable — `clearPendingAnomalies` takes ids from an already project-scoped
query in `BackgroundJobs`, and `frameworkCanonicalHashes` keys on endpoint hashes,
which embed the project. Both would still benefit from a `pid` parameter as
defence-in-depth. The two hash-rewrite updates in `Endpoints.hs` are scoped through
their subquery join.

## Remaining stringly-typed enums, surveyed

Ranked by how many sites compare against the literal. Each is a `WrappedEnumSC`
candidate; remember it does **not** supply `FromHttpApiData` automatically — the type
must list it in its deriving clause.

- **`reportType`** — `== "daily"` / `== "weekly"` at `BackgroundJobs.hs:3125,3138`,
  `Projects.hs:672`, `Reports.hs:429`, `Mail.hs:423`. A `ReportType` sum **already
  exists** (`Pages/Bots/Utils.hs`) and is converted to `Text` and discarded at the top
  of `sendReportForProject`. The type is there; it just doesn't flow.
- **`status == "sent"`** — `Settings.hs:949,967`. Two-state delivery outcome.
- **`channel == "all"`** — `Settings.hs:967` and six other `"all"` comparisons; a
  sentinel standing in for "no filter", which is what `Maybe` is for.
- **`sourceField`** — `"body"/"summary"/"url_path"/"exception"`, used as a DB key in
  ~12 queries and in web routes.
- **`runtime`** — `"go"/"nodejs"/"webjs"/"python"/"java"/"php"/"dotnet"` in
  `ErrorFingerprint.parseStackFrame`, with an `otherwise` catch-all, so a typo at a
  caller silently produces a different fingerprint.

## Recommended follow-up PRs

- **`PatternHash` newtype.** `patternHash`, `hash`, `pattern_hash`, `keyHash`,
  `targetHash`, `templateHash` are all bare `Text` and freely interchangeable across
  `LogPattern`, `UpsertPattern`, hourly stats, `Endpoints` and `SchemaLearning`.
- **Stringly-typed `sourceField`** (`"body"/"summary"/"url_path"/"exception"`) used as a
  DB key in ~12 queries and in web routes.
- ~~**Stringly-typed `runtime`** in `ErrorFingerprint.parseStackFrame`~~ — **this entry was
  wrong; verified in round thirteen.** `Runtime` is a proper sum
  (`RGo | RNodejs | RWebjs | RPython | RJava | RPhp | RDotnet | RGeneric`) derived via
  `WrappedEnumSC`, and `parseStackFrame` matches all eight constructors exhaustively with
  no catch-all. A caller cannot pass a typo'd string — the type forbids it. The only
  residual is that `parseRuntime` maps an unrecognised SDK language attribute to
  `RGeneric` silently, which is a documented total fallback at the boundary
  ("Unknown values are 'RGeneric'"), i.e. parse-don't-validate rather than evasion. If
  anything is wanted here it is a metric on the unknown-language case, not a type change.
- **Unify `Utils.parseTime` with `TimePicker.parseSince`.** The labels are now shared;
  the parsers are not. `parseTime` returns an unbounded range for absent params and
  `Telemetry.hs:266` depends on exactly that, so each of four callers needs review.
- **64 dependabot vulnerabilities** on the default branch (9 critical), unrelated to
  this work.

## Scale, honestly

Roughly −200 net lines across ~75 files. Several agents independently reported that the
code is already better factored than assumed: `EmailTemplates` already had its
parameterised skeleton, the handler layer was "unusually well-factored", and modals,
pagination, form fields, TimePicker and LogQueryBox had zero reinventions.

Where lines *were* added, it was deliberate type strengthening — `IssueFilters` +
`NullFilter` (+34), five billing newtypes (+33), `EndpointQuery`/`HostQuery` (+126),
frontend helpers (+25). The largest structural win doesn't show in the line count at
all: `processBackgroundJob` went from 655 lines to 106 by extraction.

---

# Round six — 2026-09-03 (branch `quality-sweep-2026-09-03`)

## The headline: literal duplication is genuinely exhausted

Rounds 1–5 removed the copy-paste. Two independent detectors confirm it:

- Exact-clone detector (6+ identical lines, cross-file): only import blocks plus the two
  real finds below.
- **Identifier-normalized** detector (every identifier/string/number rewritten to a
  placeholder, so a clone survives renaming — the shape that *would* have caught
  `selectIssueById` vs `selectIssueByIdScoped`), N=6, run over `src`, `test/integration`
  **and** `web-components/src`: **exactly one hit** in the whole tree.

That one hit was the two API routers in `TestUtils`, now fixed. This is a real result, not
an absence of effort: the remaining bloat in this codebase is verbosity and weak types,
not repetition, and future rounds should stop looking for clones.

## Fixed

1. **Unsanitized timezone reached the LLM system prompt.** `Pages/LogExplorer/Log.hs`
   read `timezone` from the request body and passed it straight into
   `AgenticConfig.timezone`, which `Pkg/AI.hs:530` interpolates into the **system**
   message. `Web/MCP.hs` validated the same field; the log-explorer path never did —
   the identical drift class as rounds 1–5, found by consolidating the two.
   Both now call one `AI.runNlSearch`, which sanitizes internally, so no transport can
   forget. *Severity is modest and worth stating plainly: the same request's `input` is
   already attacker-controlled prompt text, so this grants no new capability — but it
   lands in the system message rather than the user message, and it was unbounded in
   length (now ≤64 chars, restricted alphabet).*
   Guard: `nlSearchConfig` doctests pin both the accept and the reject.

2. **Two API routers, 37 arms, one repeated incantation.** Every arm of
   `routeApiV1Get`/`routeApiV1Write` spelled `mockResponse . AE.encode <$> runAsBase tr`.
   Now one `jsonRoute` helper; both routers read as the dispatch tables they are.

3. **Host and container charts built the same Widget by hand.** Byte-identical 9-field
   records in `Containers.hs` and `Infrastructure.hs`. Now one
   `Widget.infraTimeseries`; callers layer on only what differs. This is the
   uniformity goal literally — the two pages can no longer drift apart visually.

## Verified, not changed — do not relitigate

- **Modals are already uniform.** `teamModal`, `confirmModal_` and friends all go
  through `Components.modalWith_`. No reinvention.
- **Tailwind classes toggled from TypeScript are safe.** `web-components/**/*.ts` is in
  the `@source` list, so `bg-fillSuccess-strong` et al. are generated.
- **`Utils.replaceAllFormats` (355 lines) stays.** It is a deliberately backtrack-free
  single-pass scanner on the ingestion hot path, with doctests. Dense but earned.
- **`Models/Telemetry/Telemetry.hs` (2952 lines) is well factored** — `getTraceRowsWith`,
  `selectSpansWhere`, `mkTrace` are already the shared generalizations.
- **The other seven standalone-widget sites genuinely differ.** One forced constructor
  would be over-abstraction; `def` already carries the defaults.

## New open item — needs a product call, not a refactor

**The "Add teams" dashboard bulk action cannot work.** `Table.BulkAction` renders a bare
`hxPost_` button, so it submits only the table form's `itemId` checkboxes. Nothing in that
flow supplies a team, so `DashboardBulkActionForm.teamHandles` is always `[]`:
`getTeamsById` returns `[]`, the length check `0 /= 0` passes, and
`addTeamsToDashboards` is called with an empty vector — the user always sees "No
dashboards were updated". This is worse than the round-five note (which read it as a
naming problem): the action has **no team picker at all**. Fixing it means designing UI
(a picker modal before the POST), so it is deliberately left for a human.

## Weeder — now installed, and the earlier note was incomplete

436 `.hie` files exist under `dist-newstyle`, so weeder can run. Two blockers, not one:

1. `~/.cabal/bin/weeder` pointed into a pruned store entry — fixed by `cabal install weeder`.
2. **A plain `cabal install weeder` is not enough.** It resolves to a build against
   whatever GHC cabal picks (here 9.12.4) and then refuses every `.hie` file with
   *"weeder must be built with the same GHC version as the project it is used on"*.
   It has to be pinned to the project's compiler:

   ```
   cabal install weeder --with-compiler=$(which ghc-9.12.2) --overwrite-policy=always
   ```

   That is almost certainly why weeder silently fell out of the workflow despite
   CLAUDE.md mandating it: the obvious install command produces a binary that cannot
   read this project's hie files, and the error only surfaces at run time.

## Second batch: every team reference is now `TeamId`

`ProjectMembers.TeamId` (a `UUIDId "team"`) existed and was used in *some* places, while a
parallel set of signatures passed the same value as a bare `UUID`, with `coerce` bridging
them at four call sites. That is the primitive-obsession CLAUDE.md warns about, and it had
already produced two naming lies:

- `Monitors.getAlertsByTeamHandle` took a team **id**, not a handle → renamed `getAlertsByTeam`.
- `DashboardBulkActionForm.teamHandles` held ids → `teamIds` (the spec even carried a
  comment saying "not handles - the field name is misleading").

Now typed end-to-end: the `teams` column on `QueryMonitor` and `DashboardVM`, the
`teams` field on six API request/response types, `insertDashboard`, `addTeamsToDashboards`,
`selectDashboardsByTeam`, `monitorRemoveTeam`, `buildTeamMap`, `toUnifiedMonitorItem`,
`notificationSettingsSection_`, the git-sync `teamMap`, and three Servant captures /
query params. Every `coerce` and `.unwrap` on the team path is gone.

**Wire format is unchanged** — `UUIDId` derives its JSON and `FromHttpApiData` instances
newtype-wise, so the API and form encodings are byte-identical. This is a
compile-time-only strengthening.

## Weeder: what it actually found (and what it got wrong)

With weeder finally usable, the library-target sweep produced 12 candidates. **Three were
false positives from Template Haskell**, which weeder cannot see through — worth knowing
before anyone trusts its output blindly:

- `viteAssetFile` — spliced at `BodyWrapper.hs:307` as `$(viteAssetFile "index.html")`.
- `assetManifestFingerprint` — forced inside that same splice.
- `sparklineBlocks` — used two lines below its own definition, inside `sparklineBar`.

Three more are doctested (`sparklineBar`, `ensureUrlParams`, `dynSegmentLabel`); weeder
does not count a doctest as a use, and deleting them would silently drop coverage of the
underlying logic. Left in place deliberately.

**Deleted (genuinely dead, no doctest, no splice):** `Pages.BodyWrapper.withPageWrapper`,
`Pages.Components.jsonTab_`, `DeriveUtils.idToText` (a one-line shim over `.toText` —
exactly the single-use wrapper CLAUDE.md says to inline), `DeriveUtils.textArrayEnc`.

### Two uncalled functions that are *bugs*, not dead code

Weeder's real value this round was not the deletions:

1. **`LiveTail.reapExpiredSubscriptions` was never called.** Its sibling `relayReap` is
   wired at `System/Server.hs:237`; this one was exported and forgotten, so
   `projects.live_tail_subscriptions` grew for the life of the install. Its own comment
   ("running it late, or not at all, changes nothing a user can observe") is true about
   *correctness* and is probably why it was never noticed — nothing breaks, the table just
   grows. **Now wired onto the same timer as `relayReap`.**

2. ~~**`TestClock.syncConnectionTime` is never called**~~ — **this entry overstated the
   risk; verified and resolved in round thirteen.** The clock/SQL sync *does* work: the
   Hasql twin `runHasqlPoolSynced` is wired at three places in `TestUtils` and pushes the
   clock into the `app.current_time` GUC before every Session, which is why
   `AnomaliesSpec:551` can rely on a trigger reading `app_now()`. Test SQL was never
   reading wall-clock. `syncConnectionTime` was only the unused *postgresql-simple*
   sibling — genuinely dead (weeder had flagged it), now removed along with its two
   now-unused imports. Both comments that pointed at it (the `TestClock` module header
   and `TestUtils.runTestBg`) now name `runHasqlPoolSynced` instead, so the docs describe
   the mechanism that actually exists.

---

# Round eight — settings / projects / git-sync (branch `quality-sweep-round7`)

All three skills invoked as skills (not applied from memory — that was the round-seven
correction) over `Settings.hs`, `Projects.hs`, `GitSync.hs`, `CodeContext.hs`,
`Models/Projects/GitSync.hs`.

## The find: `createEmptyUser` could 500 the invite-members handler

```sql
insert into users.users (email, active) values (#{email}, TRUE)
  on conflict do nothing returning id     -- DO NOTHING suppresses RETURNING
```

`ON CONFLICT DO NOTHING` returns **no row** when the row already exists, so an
existing email came back as `Nothing`. The only caller did:

```haskell
Projects.userIdByEmail email
  >>= maybe (Projects.createEmptyUser email >>= maybe (error "duplicate email …") pure) pure
```

Lookup-then-insert with nothing between them, and `error` on the losing branch. Two
people inviting the same new address — or one request racing itself — crashes the
handler. This is the constraint-evasion shape exactly: a reachable failure mode
absorbed by `error` rather than modelled.

**Fixed** by making it a real get-or-create: `on conflict (email) do update set email
= excluded.email returning id`. `DO UPDATE` returns the row. The `SET` is a
deliberate no-op on the conflict target and **must not touch `active`** — assigning
`excluded.active` would silently reactivate a deactivated user on every invite. The
caller's `error` is gone; the residual impossible case is a 500 with a message rather
than a crash.

Guard: `ManageMembersSpec.createEmptyUser_isIdempotent_soAConcurrentInviteCannotCrashTheHandler`.
Deterministic — two sequential calls, no concurrency contrivance. Confirmed to fail
before the fix with `expected: Just (UUIDId …) but got: Nothing` (798 examples, 1 failure).

## Everything else in this group came back clean

- **No clones** (identifier-normalized detector, N=5, across all five files).
- **No hoisted Tailwind classes.** Every class string keeps both `bool`/`if` branches
  literal, so the scanner sees them all.
- **No suppressions**, no other partiality.
- `Settings.hs` (1687 lines) is already consolidated: `promSave` is shared by the
  create and update handlers, `apiKeySetActive` by delete and activate,
  `prometheusMut` by all five Prometheus mutations. Nothing to extract.
- `safeScrapeUrl`'s `case … of Nothing -> False` could be `maybe False`, saving one
  line at a cost in readability for a doctested SSRF guard. Left alone.
- `Default GitHubSync` is hand-written and positional (17 args) because `UTCTime` has
  no `Default`. Cannot be derived. Worth knowing it is order-sensitive.

---

# Round nine — ingestion pipeline

`runSharedKafkaProducer` interpreted 3 of kafka-effectful's **11** `KafkaProducer`
constructors and caught the rest with `_ -> error`. `cabal.project` carried a note
asking whoever bumps the tag to remember to audit it — a comment doing a type's job.
The other 8 are now spelled out, so a bump that adds a 12th op is a compile error at
both the real interpreter and the `KafkaConsumerSpec` mock (which had the same
wildcard, and which the rewritten cabal.project note would otherwise have described
falsely). +13 lines, deliberately, to retire a production-panic path.

Verified clean and **not to be re-reviewed**: no clones across OtlpServer /
ProcessMessage / Queue / LiveTail (5,418 lines); LiveTail's three hand-written JSON
instances are all justified; every swallow-site is counted or logged — the publish
drop feeds `publishFailed`, the gRPC catch logs at `logAttention`, and the two
`handleAny (const pass)` sites are best-effort control-plane work covered by lease
expiry. The `ce-type` fallthrough logs **and** DLQs as poison rather than empty-acking
(which would stall the partition).

# Round ten — anomalies / dashboards / issues

## `withIssueDataH` rendered nothing for a payload that did not parse

```haskell
withIssueDataH d = whenJust (parseMaybe AE.parseJSON $ getAeson d)   -- silent
```

`issue_type` and `issue_data` are separate columns written by paths spread across the
pattern, alert and API-change code. When they disagree, this rendered **nothing** — no
log, no metric, no visible gap — at eight call sites on the anomaly detail page and
the issue list. A blank panel is indistinguishable from "this issue has no details",
so the on-call reader concludes there is nothing to see. Same failure class the
LiveTail author flagged as the worst that feature has.

Now renders a "details unavailable" notice. Exported (it was unexported, which is part
of why nothing could test it) and doctested on **both** paths — well-formed payload
renders content, malformed renders the notice.

## The larger type change, deliberately not attempted

`issueData :: Aeson AE.Value` + a separate `issueType` tag is untyped-blob-plus-tag.
One sum type — `LogPattern LogPatternData | RuntimeException RuntimeExceptionData | …`
— makes a mismatched pairing unrepresentable and deletes the entire class of bug the
above only makes *visible*. It touches the DB codec and every writer, so it needs its
own PR. **This is the highest-value type change left in the codebase.**

## Clean

No clones, no enum ladders, no hoisted Tailwind classes, no suppressions, no
partiality across 6,397 lines.

---

# Round eleven — components layer

- `instance Default (Features a)`: 21 hand-written lines setting eighteen fields to
  Nothing/[]/False — exactly the generic instance. Now derived (−19). Guarded by
  `defaultFeaturesAreInert`, break-tested (`expected: False but got: True` at
  Table.hs:143). **Guard's limit:** asserts five representative fields, one per field
  shape; several fields hold functions with no Show/Eq so a total assertion is not
  expressible. A future field with a non-generic default would slip past.
- `pricingBtnCls` (a top-level class function referenced by `class_` — the LoB rule's
  "Wrong" example) became `pricingButton_`, a markup component holding the literal
  inline. Also deduped the "Current plan" label logic repeated at three sites.
- **Kept deliberately:** `Default Config` / `Default FieldCfg` have real non-generic
  defaults; `emptyState_`'s class tuple has three uses and inlining means three copies
  of the same case.
- **Flagged, not changed:** `Widget.cellClass` / `Table.hs:516,579` interpolate
  `col.align` — data-driven config the Tailwind scanner cannot see, same class as the
  Onboarding bug. `align` is set nowhere (no Haskell caller, no dashboard YAML), so it
  is working-but-unused surface; removing it drops a feature. The first value not
  appearing literally elsewhere will silently no-op.

# Round twelve — background jobs / notifications

## Fixed

1. **`CleanupDemoProject` reinvented `Projects.demoProjectId`** as a bare `UUID.nil`.
   The constant already existed and is used in three other places. Exactly the
   "reinvented a pattern we had an equivalent of" case.
2. **`ErrorAssigned`'s `_ -> pass` hid a tenant-boundary event.** The catch-all
   conflated deleted rows (routine) with `err.projectId /= pid` — an assignment aimed
   at an error belonging to a *different project*. Now two arms: cross-tenant logs at
   `logAttention` with all three ids; missing-row logs at `logInfo` with which lookup
   failed. Different levels on purpose — `logAttention` is meant to be page-worthy.

## Noted, not changed

- **`Notify.hs:1` carries a file-wide `-Wno-redundant-constraints`.** Suppression is a
  finding by default; narrowing it means touching effect rows across the notification
  interpreters, which needs its own verification pass.
- **`ErrorPatternId` is a bespoke newtype**, not a `UUIDId`, so it has none of the
  typed-id machinery (no `.toText`; needs `.unErrorPatternId`). Converting it is a
  separate change with its own blast radius.

## Verified clean, do not re-review

No clones across 7,116 lines. No client surface at all in these three files — zero
hyperscript/htmx; EmailTemplates' 77 inline `style_` uses are correct, since Tailwind
classes do not work in email clients. `backfillSessionSql`'s apparent 265 lines are a
raw SQL quasiquote, not distillable Haskell.

**`ThreadRefs`' hand-written Semigroup/Monoid must stay hand-written.** `<>` is
first-wins (`<|>` per field), but `Maybe`'s own Semigroup *concatenates* — so
`deriving via Generically` would silently change alert threading from "keep the parent
message id" to "append ids". This is a case where deriving is not merely unnecessary
but wrong; a future distill pass should not "fix" it.

---

# Rounds fourteen & fifteen — tenant-scoping audit

## Method: three mechanical scans, not reading

Reading had already missed these. An earlier round fixed the *bulk* monitor actions for
exactly this bug class, and the single-entity siblings survived that pass. What found
them was enumerating every query and checking for a tenant predicate:

| Scan | Candidates | Real |
|---|---|---|
| `UPDATE`/`DELETE`, no `ProjectId` param, no `project_id` predicate | 46 → 15 handler-reachable | **2** |
| `SELECT` keyed on an entity id, unscoped | 12 | **1** |
| Takes a `ProjectId` but never references `project_id` in its SQL | 24 | **0** |

The third scan is all false positives for one reason worth remembering: `projects.projects`
keys on `id`, which *is* the project id, so the string `project_id` never appears in a
correctly-scoped query against it.

## The three vulnerabilities (all fixed, all on prod)

1. **`alertSingleToggleActiveH` → `monitorToggleActiveById`** — `WHERE id=#{mid}`. A user
   authorised on one project could deactivate another project's monitor, silencing its
   alerting. The API path was already safe (`withRefetch`/`apiMonitorGet`/`ownedOr`);
   only the web route was not.
2. **`errorUnmergePostH` → `unmergeErrorPattern`** — `WHERE id = #{pid}` (and that
   parameter was *named* `pid` while holding a **pattern** id, which is what a project id
   is called everywhere else — plausibly how the bug was written; renamed to `epid`).
3. **`errorGroupMembersGetH` → `getErrorPatternGroupMembers`** — `WHERE canonical_id =
   #{eid}`. Disclosed another project's error types and messages, *and* rendered unmerge
   links for each disclosed row under the caller's own project path — i.e. the read handed
   out the ids for vulnerability 2.

All three are fixed **in the query**, not at the caller, so every present and future
caller is covered. `getLogPatternGroupMembers` was scoped too: it has no production
caller today, so scoping it now means the first one is safe by construction.

## Verified safe, do not re-audit

API keys guard via `ownedKey` from `projectApiKeysByProjectId`; the three sibling
error-pattern handlers each guard with `err.projectId /= pid`; dashboards go through
`getDashAndVM pid`; the member-permission list is derived from `projMembers`, not user
input; `Settings` calls `getConfigByProject pid cid` (the `getConfig` hit was a substring
false positive).

## The verification lesson — the important part of these rounds

**A failing test is not evidence the test works.** I twice declared these guards "proven"
on the strength of `[✘]` markers and a `2 failures` count. Both times the tests were
dying in *setup* and never reached an assertion. Three separate fixture bugs did this,
each producing a summary identical to a working guard:

- `stacktrace` is `NOT NULL` (migration 0033) and the INSERT omitted it → `SqlError 23502`
- `canonical_id` is a self-referencing FK → `SqlError 23503` pointing at a random UUID
- `error_data` is decoded as `ATError` on read; `'{}'` lacks its required `when` key

Only reading the **failure text** distinguishes `expected: [False] but got: [True]` (guard
works) from `uncaught exception` (guard never ran). This is the mirror image of the
false-green discipline used everywhere else in this sweep, and I had no habit for it.

**Any future break-test must assert on the failure reason, never the count.**

## Break-test outcome — both guards proven

With the fixtures fixed and each predicate neutralised as `AND (project_id = #{pid} OR
TRUE)` (a form that keeps `pid` used, so it still compiles under `-Weverything -Werror`),
both tests failed at their **assertion**:

- `errorUnmerge_…` — `AnomaliesSpec.hs:674` `expected: [False] but got: [True]`
- `errorGroupMembers_…` — `shouldSatisfy` failed on HTML containing
  `SecretTypeError: victim-only-message`, `Hash: group-read-authz-hash`, and a live
  `data-hx-post` **Unmerge** button — the disclosure and the ids for the write bug, in
  one response body.

Restoring the predicates makes both pass. The guards measure what they claim to.

A side effect worth recording: because the fixture bugs were in the *tests*, master was
red for these two specs from the moment they landed — they errored in setup on every run.
A test that has never been observed green is not a regression guard.

Another session found the same two schema bugs concurrently (`6320d244`) and fixed them
first; that work is upstream and this branch rebased onto it. It did **not** include the
`error_data` fix, for a good reason: with the guard intact the query returns no rows, so
nothing is ever decoded and the missing `when` key cannot surface. A filtered run is
genuinely green either way. The difference only appears when the guard regresses — the
foreign row comes back, and an undecodable payload turns the disclosure assertion into a
`HasqlException`. Same red, wrong reason, wrong message.

That generalises: **a fixture defect that only manifests when the guard fails is invisible
to every run in which the guard works.** Break-testing is the only thing that exercises
that path, which makes it the only way to find this class of bug.

## `getErrorPatternById` — audited, deliberately left unscoped

Flagged during the scans as "keyed on an entity id, unscoped". It is not a
vulnerability: all four callers hold a `pid` and all four guard. The three handlers in
`Pages/Anomalies.hs` (`assignErrorPostH`, `resolveErrorPostH`, `errorSubscriptionPostH`)
each match `err.projectId /= pid`, and `BackgroundJobs.ErrorAssigned` guards
`err.projectId == pid`.

The tempting consolidation — scope the query, as the other three fixes did — would be a
regression. `ErrorAssigned` deliberately separates "no such row" from "row in another
tenant" and `logAttention`s the second, because a cross-tenant assignment is a signal, not
a miss. Scoping the query collapses both into `Nothing` and deletes that branch. Encoding
the distinction instead (`data Scoped a = Missing | WrongTenant a | Found a`) costs more
lines than the three guard arms it would replace.

Scope-in-the-query is the right default *when the caller only needs presence*. It is the
wrong move when the caller needs to distinguish absence from a tenant mismatch. Leave it.

## Open decision: `detectErrorSpikes` ignores acknowledgements

`Issues.isSilenced` has exactly one caller — `detectLogPatternSpikes`. Its own Haddock
says "Detectors consult this before firing a fresh issue: an ack means 'don't tell me
about this again', not merely 'hide the old row'." `detectErrorSpikes` never calls it,
so the two detectors disagree about what an acknowledgement means.

The mechanism, which is worth understanding before anyone changes it:

- `insertIssue`'s `ON CONFLICT (project_id, target_hash, issue_type) WHERE
  acknowledged_at IS NULL AND archived_at IS NULL` — that `WHERE` is the **partial-index
  predicate**, not a `DO UPDATE` condition. An acked row is not in the index, so no
  conflict is detected and the INSERT creates a *second* row.
- `mkErrorIssue` sets `issueType = RuntimeException` and `targetHash = p.hash`, which is
  exactly the key `isSilenced` queries. The data for the check is already present.
- Note the two fields are not the same notion: the partial index keys on
  `acknowledged_at IS NULL` ("seen"), `isSilenced` keys on `acknowledged_until > now`
  ("silenced until"). A plain ack and a timed ack behave differently.

**Do not "fix" this without a product call.** Four tests in `ErrorPatternsSpec` (326,
392, 433, 467) ack every open issue *in order to* get a fresh spike issue created —
`-- Acknowledge existing issues so ON CONFLICT doesn't deduplicate the new spike issue`.
Gating on `isSilenced` fails all four.

Both readings are defensible: an ack should mean silence (what the word means, what the
log-pattern path does), or a spike is new information an ack shouldn't mask ("I know about
this error" ≠ "I know it just went 10x"). The fixture comment suggests the current
behaviour was discovered as a test convenience rather than chosen, but that is inference,
not evidence. It is user-visible paging behaviour; wrong either way is expensive.

## Measured: the sweep is net +1811 lines in `src/`, and that is not verbosity

`git diff --numstat 44c38b1c..HEAD -- src web-components/src` over 123 commits:
**6292 insertions, 4481 deletions — net +1811.** The stated goal was to drastically
reduce code size. It has grown. The breakdown explains why, and the conclusion is not
"distill harder":

Distill *worked* where it was applied — Bots (−61 Discord, −57 Slack, −49 Whatsapp),
Onboarding −66, Dashboards −51, `Data/Effectful/Notify` −41, `log-list.ts` −38.

Growth is feature and bug-fix surface, not bloat — `BackgroundJobs.hs` +400,
`Models/Apis/PatternMerge.hs` +307, `Pages/Telemetry.hs` +160, `Pages/Bots/Utils.hs` +160,
`Pkg/ErrorFingerprint.hs` +156, `Pages/Anomalies.hs` +147, `Models/Apis/Issues.hs` +144.

Three distill passes on the largest files found **zero** mechanical wins:

- `BackgroundJobs.hs` (5469 lines) — no cross-file clones exist anywhere in `src` at a
  10-line normalised window. Its largest binding, `backfillSessionSql` at 265 lines, is
  62 lines of SQL literal with 8 lines of Haskell around it.
- `Pages/Dashboards.hs` / `Pages/Anomalies.hs` — modals are already shared (`modal_`,
  `modalWith_`, `confirmModal_`, used across 8 files; the only hand-rolled `dialog_` is a
  Slack API payload, not HTML).
- `PatternMerge` — the error/log families are already unified behind one `MergeConfig`
  record and a single `embedAndMerge` runner. The four remaining SQL pairs differ by
  table and id column type (`uuid[]` vs `bigint[]`); unifying them means `HI.Sql`
  fragment concatenation around each table name, which trades ~35 lines for materially
  worse readability on hot ingestion queries.

55 hand-written instances of derivable classes exist, and the concentrations are all
justified: `Pkg/DeriveUtils.hs` *is* the deriving machinery, several are orphans for
library types that cannot be derived, and `GitHost`'s six carry an explicit rationale —
`WrappedEnumSC` would snake-case `GitHub` to `git_hub`, which migration 0125's CHECK
constraint rejects.

**The reduction target has been met on the code that existed; the remaining mass is
irreducible SQL, genuinely distinct job logic, and new features.** Continuing to squeeze
already-swept modules would trade correctness for line count. The honest next lever is
scope — deciding which *features* to drop — and that is a product call, not a refactor.

## Two corrections to the documented workflow

**1. `cabal test doctests` is NOT safe to run while the watchers are up.** CLAUDE.md lists
running doctests as one of the few sanctioned direct compiler invocations. In practice it
builds the *197-module test-dev target* and writes into
`build/test-dev/test-dev-tmp` — the directory the `make live-test-dev` ghcid owns. Within
seconds it was emitting `[Mismatched dynamic interface file]`, i.e. corrupting a running
suite. Killed it. Treat doctests like every other cabal invocation: one cabal process at a
time. Verify them on a natural watcher run, or stop the test-dev watcher first.

**2. The doctest GHCi session sees the module's full top-level scope, not just its
exports.** CLAUDE.md and the skill both say exports-only, which would make any example
using an imported-but-not-re-exported symbol dead. Evidence it is wrong — this failure
from an earlier run in this session:

    src/Models/Apis/Issues.hs:1203: failure in expression
      `isJust $ parsePayload QueryAlert (AE.object ["query_expression" AE..= ("x" :: Text)])'
    expected: True
     but got: False

`AE` is a qualified import of `Issues.hs` and is not re-exported, yet the example
*evaluated* (returned `False`) rather than failing on scope. So `parseUrlPiece` resolves
in `Endpoints.EndpointSort`'s and `LogQueries.SessionSort`'s doctests without a `$setup`.

The exports-only rule still holds for the thing it was actually learned from: projecting
`.field` off a *type defined elsewhere*, where `HasField` will not solve unless that
module is imported by `$setup`. Worth keeping the distinction — read narrowly it is a real
constraint, read broadly it sends you adding `$setup` chunks that nothing needs.

## The systemic finding: query-param enums living as `Text`

The evasion review's one real result, and it is a *pattern*, not a bug list. A query
parameter that is conceptually an enum is carried as `Text`, matched against string
literals in a `case`, and unmatched values fall through to a default. Nothing ever fails;
the feature just silently does something else.

| Site | Status |
|---|---|
| `Endpoints.EndpointSort` | fixed earlier in the sweep — "Alphabetical" silently did nothing |
| `LogQueries.SessionSort` | fixed (`a0be8e67`) |
| `Pages.Anomalies.IssueTab` | fixed (`1652d04b`) |
| `Pages.Monitors.MonitorTab` | fixed (`7810598b`) |
| `Endpoints.hs:240` — `filterTM == Just "Archived"` | **deliberately left, see below** |

`IssueTab` is the one worth studying, because it shows the failure mode is worse than a
single silent default. The tab was matched in **four independent places** — the
filter/label pair, `tabBlurb`, `issueBulkActions`, `issueZeroState` — each with its own
fall-through. A rename or typo degrades *one* surface while the other three keep working:
the Archived tab renders "Nothing to triage" with Inbox's bulk actions, and no code path
errors. Four separate defaults are four separate opportunities to disagree.

**Wire spelling is the trap when fixing these.** `WrappedEnumSC` snake-cases constructors,
so `TabAcknowledged` would serialise as `acknowledged` — and every live
`?filter=Acknowledged` bookmark would fail to parse and land on Inbox, reintroducing the
exact silent degradation being removed. Where the existing wire form isn't snake_case,
keep it and make one function the source of it (`tabSlug`, `hostSlug`) rather than
reaching for the deriving-via shortcut. `GitHost` already documents this for the same
reason: `WrappedEnumSC` would turn `GitHub` into `git_hub`, which migration 0125's CHECK
constraint rejects.

`Endpoints.hs:240` is left as `filterTM == Just "Archived"` on purpose. It is a boolean
projection, not a `case` with a fall-through: an unknown filter degrades to `False`, i.e.
the Endpoints tab, and there is no second site that could disagree with it. The pathology
in the other four was *multiple independent defaults drifting apart*; a single `==`
against one literal cannot have that failure mode. Converting it would add a type and two
functions to delete one comparison — the trade running backwards.

Verified green after all four: `808 examples, 0 failures, 90 pending` (full run — a
truncated run was rejected twice before accepting this one).

Cost, honestly: `SessionSort` +37 lines, `IssueTab` +40, `MonitorTab` +23. All three trade
size for a compile-time guarantee, against the size-reduction goal, knowingly. The trend
down is deliberate — the rationale belongs in this file once, not re-argued in Haddock at
every site, and `MonitorTab` also *deleted* literal duplication (three sources of
"Active"/"Inactive" collapsed to one) rather than only adding a type.

## `BackgroundJobs` catch-alls — all three audited, none changed

- `TrialEndingReminder` (`:701`) — `_` logs the skip explicitly with the sub id and
  status. A documented no-op, not a silent drop.
- `ReportUsage` (`:794`) — `case provider of NoBillingProvider -> audit; _ -> drain`.
  This *is* a catch-all on an in-house sum (`StripeProvider | LemonSqueezyProvider |
  NoBillingProvider`), which the convention forbids. Left deliberately: the outer test is
  binary ("is there a usable provider"), and the branch that actually depends on the
  provider — `case target of Right StripeMeter / Right LemonSqueezyMeter / Right
  LemonSqueezyEventsItem / Left reason` — is already exhaustive, so a genuinely new
  provider fails to compile *there*. Making the outer case exhaustive means restructuring
  a ~40-line block that carries explicit `DOUBLE-SUBMIT RISK` handling, for a guarantee
  that is largely already held. Not worth it without a reason to touch that code.
- The third (`:689` region) is the same `TrialEndingReminder` case.

Also noted, not fixed: `Pages/Bots/Discord.hs` uses the magic number `type_ = 11` twice
for Discord's public-thread channel type. Foreign enum, so the catch-all beside it is
fine, but the literal deserves a name.

## `/hs-lob-review` on the client tier — no violations worth acting on

26 `classList.toggle` sites in real source (the other 29 grep hits were `dist/` build
output and a minified sourcemap — always exclude `dist/` when counting these). Assessed
each against the escalation ladder. Verdict per cluster:

**Keep — tier 5 earned.**
- `main.ts:207-226` `syncTimeTransports`. Looks like four demotable class toggles, but the
  same function also sets a *three-way* `aria-label`, `aria-pressed`, `disabled`, and
  `textContent`, none of which CSS expresses. Demoting only the class toggles splits one
  cohesive state-sync across two tiers and two files. The skill's own rule applies: a
  demotion that is materially uglier stays.
- `service-map.ts:670`. The anchor's `href` is computed from graph data in the same loop,
  so the element is JS-constructed regardless; a CSS rule would not remove the loop.

**Demotable, small, deliberately not applied tonight.**
- `service-map.ts:684-685` — the `hidden`/`flex` pair on the scope chip is the classic
  CSS-state pattern: `container.dataset.scoped` + `hidden group-data-[scoped=true]:flex`.
  Net −1 line, and the chip's visibility becomes readable on the chip.
- `widgets.ts:237-238` — `setChartLoading` writes two classes to two elements; one
  `dataset.loading` write on the container would drive both from inline Tailwind. The
  `requestAnimationFrame` deferral must stay (it exists to keep a fetch from forcing
  layout mid-scroll).
- `session-replay.ts:831` — `container?.classList.toggle('expanded')` on a chevron button
  is a pure expand/collapse with no state read elsewhere. This is a genuine tier-2 case:
  `<details>/<summary>` removes the handler entirely.

Not applied because none of them can be *verified* tonight: confirming a client change in
the running app needs a Haskell content-change rebuild to re-fingerprint the Vite assets,
and these are interactive surfaces (a replay viewer, a service map, chart loading states)
where "it compiles" is not evidence. Blind-editing them to save four lines is the wrong
trade. They are worth doing in a session with a browser open.

Also confirmed healthy: 3 `htmx.ajax` calls total, and **zero** `dataset.wired` /
`dataset.bound` manual-wiring guards — the pattern htmx attributes exist to remove is
absent from this codebase.

## Category 5 — "code that should have changed but didn't"

The evasion lens the mechanical scans cannot reach. Three axes checked.

**New fields vs their codecs — clean.** 124 record fields were added across the sweep.
Cross-referenced against every type carrying a *hand-written* `ToJSON`/`FromJSON` (where a
new field silently never serialises): no overlap. The only near-miss, `Replay.FetchSplit`,
has no JSON instance at all — it is an internal classification record introduced to
consolidate four repeated comprehensions.

**New fields vs explicit SQL column lists — clean.** `Issues.hs` gained 13 fields and
`Endpoints.hs` 27, but none landed on a persisted record: they are all on query-config and
view types (`IssueFilters`, `MkIssueOpts`, listing filters). The persisted `Issue` record
is unchanged, so `insertIssue`'s hand-written column list cannot have fallen behind.

**Bug fixes without a regression guard — and a correction to how I checked.**

My first pass flagged 19 `fix` commits as untested because they touched no `test/` file.
**That heuristic is wrong in this repo: doctests live in `src/`.** The clearest example was
`fix(errors): stop the fingerprint shattering on any hex run of an odd length`, which I
called unguarded — its guard is a doctest on `Utils.replaceAllFormats` pinning both halves
of the invariant (the two order ids reaching one group, *and* the below-floor case staying
`{integer}`). Counting `+.*>>>` in `src/` as a guard drops the list from 19 to 14, and most
of the remainder are CSS/JS/nav changes with no Haskell surface to test.

`fix(timepicker): one label vocabulary` also needs no guard: it was fixed *structurally* —
`timePickerItems = map (second snd) sinceWindows` — so the two lists can no longer
disagree. Deriving beats asserting.

Genuine remaining guard gaps, all Haskell-side:

- `fix(dashboards): "% of Total" truncating to whole numbers` — the fix is a SQL cast
  (`::numeric` → `::float::text`), so no doctest is possible. Closing it needs an
  integration test seeding a known distribution and asserting a *fractional* `pct` comes
  back, on both PG and TF.
- `fix(reports): weekly emails dated in the server's timezone` — customer-visible, and
  date logic is exactly what `TestClock` exists for.
- `fix(errors): backfill shapes`, `fix(metrics): stale pagination cursor` — both
  integration-testable.

## The two group-review pipelines: a fork by shape, not by substance

`BackgroundJobs.hs` carries two families ~1300 lines apart whose names invite consolidation:

| Endpoints | Errors |
|---|---|
| `endpointGroupReviewBatch` | `errorGroupReviewBatch` |
| `reviewResidualEndpointGroups` | `reviewErrorGroups` |
| `applyConfirmedGroups` | `applyConfirmedErrorGroups` |
| `recheckQuarantinedMerges` | `recheckQuarantinedErrorMerges` |
| — | `refuteErrorGroups` |

The skeleton does match: gate on a config flag + API key, key each group by a members-hash,
filter to fresh, one LLM call, `PatternMerge.parseGroupReview`, record, auto-apply.

**Do not unify them.** The control flow differs where it counts. Errors paginate by a
cursor that restarts at the end of the corpus (deliberately — "a cursor that only ever
moves forward is how a sweep quietly declares itself finished with work left"), require a
minimum member count, and run a *second* refute pass over only the proposed merges.
Endpoints take a biggest-first batch (also deliberate — one customer's group is 1,262
endpoints against a median of three), honour `dueForReconfirm`, and promote id rules after
applying. A shared config record would need ~12 fields, several of them domain-shaped
functions plus a type parameter, to save ~20 lines — while coupling two LLM-calling,
cost-incurring, DB-mutating pipelines whose invariants are documented separately and
differ.

Contrast `MergeConfig`/`embedAndMerge` in the same file, which *is* the right call: there
the two families differ only by table and id column type, and the steps are identical.
Shape similarity justifies consolidation only when the substance matches too.

Reading them side by side did pay, though: `reviewErrorGroups` was calling
`parseGroupReview` **twice** on the same response (once for verdicts, once for shapes)
behind a `sh `seq` True` guard that always yields True and silences nothing —
`-Wno-unused-matches` is already set. The endpoint sibling had it right in one pass.
Fixed in `79182b4c`. That is the value a consolidation review actually produces here: not
"merge these two", but "one of them proves the other is doing unnecessary work".

## `7c225496` is mislabeled — git log is load-bearing here

Its subject reads *"fix(reports): weekly emails were dated in the server's timezone, not
the customer's"*. Its entire diff is `T.unwords` -> `unwords` in `src/Pkg/Mail.hs`. The
real timezone change is in `364d728a`; this message is orphaned, presumably left by a
squash or rebase.

Consequence: my first guard-gap pass listed this as an unfixed bug on the strength of the
message. It is fixed — `renderWeeklyEmail` uses `project.timeZone` through
`utcToLocalTimeTZ` with a UTC fallback. **A commit message describing work that is not in
its diff will mislead exactly as it misled me.** In a repo where the log carries this much
reasoning, that is a real defect, not a cosmetic one.

The behaviour *was* genuinely untested, and the reason was structural: `renderWeeklyEmail`
takes 14 parameters, so no one was going to build that fixture. Fixed by extracting the
pure core (`reportDayLabels`) so a two-line doctest holds it. That is the general move for
the remaining guard gaps — **when something is untested because it is buried in a wide
effectful function, extract the pure core rather than building a heroic fixture.**

Verification note: the offsets were checked against the IANA database independently
(Auckland is UTC+13 on both dates; NZDT runs to 2026-04-05) rather than asserted from
memory. The doctest itself has not executed yet — `cabal test doctests` cannot run while
the test-dev watcher holds the build directory, so it lands on the next doctests pass.

## CORRECTION: the +1811 figure conflated code with documentation

Earlier in this file I reported the sweep as **net +1811 lines in `src/`** and concluded the
size-reduction goal had not been met. That number counted comment lines as code. Split
properly (`git diff 44c38b1c..HEAD -- src web-components/src`):

```
added    6525 =  4362 code +  1739 comment +  424 blank
removed  4385 =  3951 code +   259 comment +  175 blank
NET     +2140 =  +411 code + +1480 comment + +249 blank
```

**Net code growth is +411 lines** across ~130 commits that shipped the error-grouping
pipeline, LLM group review, ingest-time masks, and the issues redesign. The other +1480 is
Haddock, doctests, and explanatory comments — which this repo's conventions actively
require (doctests *are* the unit tests here; comments are mandated where the reasoning is
non-obvious).

For scale, `src/` overall is **77,107 lines = 53,536 code (69%) + 14,166 comment (18%) +
9,405 blank**, with ~1,871 doctest lines inside that comment total. Roughly a fifth of the
tree is documentation by design.

The same correction applies to my own work tonight. I reported the four type/test commits
as "+140 lines against the size goal". Actually:

| commit | +total | code | comment | removed |
|---|---|---|---|---|
| `SessionSort` | 46 | 18 | 24 | −10 |
| `IssueTab` | 63 | 35 | 20 | −23 |
| `MonitorTab` | 33 | 19 | 8 | −10 |
| `reportDayLabels` | 25 | 6 | 17 | −4 |
| **total** | **167** | **78** | **69** | **−47** |

**Net +31 lines of code** for four compile-time guarantees and one extracted, doctested
pure function — not +140. I overstated the cost against myself by ~4.5x.

Methodological note: my function-size script measured `::` to `::`, which charges each
function for the *next* one's Haddock. It reported `getAlertStatusColor` as 102 lines; it
is 4, followed by 98 lines of `replaceAllFormats` doctests. Any "largest function" figure
in this file taken from that script is inflated the same way unless separately verified
(`backfillSessionSql` was checked directly and is genuinely mostly SQL).

## Largest functions, measured correctly

My earlier size scripts were wrong twice. The first charged each function for the *next*
one's Haddock (`getAlertStatusColor` reported as 102 lines; it is 4). The second ended a
function's extent only at the next `^ident ::`, so an intervening `data` block was
attributed to the preceding function (`otlpHttpH` reported as 440 lines; it is 3 — the
rest was the `ApiV1Routes` record). Correct measure: extent ends at the next top-level
declaration of *any* kind, counting non-comment lines only.

| code lines | function |
|---|---|
| 326 | `dashboardPage_` |
| 292 | `anomalyDetailPage` |
| 291 | `tracePage` |
| 288 | `replaceAllFormats` — single-pass scanner, algorithmic, justified |
| 275 | `bodyWrapper` |
| 229 | `apiLogsPage` |
| 216 | `backfillSessionSql` — SQL literal, justified |

**Corrected again (fourth attempt).** The measure above still under-detected: a signature
whose name sits on its own line with `::` on the next (`containersInWindow\n  :: (DB es,
...)`) was invisible, so its body counted against the *previous* function —
`freshnessWindow` reported as 156 lines when it is 2, and `backfillSessionSql` as 216 when
it absorbed a neighbour. Correct rule: a top-level declaration begins at **any non-comment,
non-blank line at column 0**. Final figures:

| code lines | function |
|---|---|
| 325 | `dashboardPage_` |
| 291 | `anomalyDetailPage` |
| 290 | `tracePage` |
| 287 | `replaceAllFormats` — scanner, justified |
| 274 | `bodyWrapper` |
| 228 | `apiLogsPage` |
| 211 | `logQueryBox_` |
| 183 | `queryEditorInitializationCode`, `processEagerBatch` |
| 151 | `containersInWindow` |

The conclusion is unchanged — the outliers are Lucid page renderers, long because markup is
verbose, with no duplication in them.

**The meta-lesson is the real finding here.** Four separate attempts to measure function
size by regex, each fixing a case the previous missed: (1) charging a function for the next
one's Haddock, (2) not ending at a `data` block, (3) not matching multi-line signatures,
(4) a `ps | sort -t= -k5` key that landed on the wrong field and hid a 2.4 GB process.
**Ad-hoc structural parsing of Haskell with regex is unreliable, and every intermediate
result looked plausible enough to quote.** Any figure in this file derived that way was
checked against the source before being trusted; treat new ones the same way.
| 212 | `logQueryBox_` |

The page renderers are long but **not duplicated**: a 4-line normalised clone scan inside
`dashboardPage_` finds exactly one repeated block in 326 lines. Lucid markup is verbose
because markup is verbose; that is not the same as copy-paste.

### The one real duplicate: two gridstack handlers that have drifted

`Dashboards.hs:411` and `:482` are the same change-handler written twice — once for the
top-level grid, once for nested grids — inside embedded JS. They have already diverged
in three ways, and at least two look accidental:

| | top-level (`:411`) | nested (`:482`) |
|---|---|---|
| mobile guard | `grid.getColumn() === 1` | `window.innerWidth < 768` |
| collapse lookup | `gridEl.querySelector(...)` | `nestedEl.closest(...)` |
| interaction events | `dragstart resizestart` | `dragstart resizestart removed` |

Two different tests for "are we on mobile", and `querySelector` (descendant) versus
`closest` (ancestor) is a semantic difference, not a rename. One shared
`wireGridChange(inst, el, isMobile)` would remove ~8 lines and, more importantly, stop the
next divergence.

Not applied: this is drag/resize behaviour on the dashboard grid, unverifiable without a
browser (a client change needs a Haskell content-change rebuild to re-fingerprint the Vite
assets). Worth doing in a session with the app open — and worth doing *deliberately*,
since deciding which of the two mobile guards is correct is a real question, not a merge.

## The duplication question, answered empirically

The premise this sweep started from was "no code reuse or code consolidation". Measured
across all 77k lines of `src/` and `web-components/src`, with identifiers, string literals
and numbers all normalised away (so renamed copies still match):

| window | duplicated blocks | redundant copies |
|---|---|---|
| 3 lines | 7 | 12 |
| 4 lines | 7 | 8 |
| 5 lines | 3 | 3 |
| **6 lines** | **0** | **0** |

**There is no copy-paste duplication of six lines or more anywhere in this codebase** —
not across files, not within them. The monotonic decrease is the sanity check that the
scanner works; a broken scan returns zero at every window, and this one does not. (Two of
my scans tonight *did* return false zeros — `git diff` emitting no `+` lines under the
repo's external diff driver, and a doctest-guard grep that could not see `src/`. A zero
result gets a sanity check before it gets believed.)

The surviving 4-line hits are Servant route-type declarations (`Routes.hs:279/315` —
repeated `QPT` chains) and one TypeScript block. Route boilerplate is inherent to the
Servant API type; it is not consolidatable logic.

The one duplicate worth acting on is smaller than the scan's floor: the two gridstack
handlers in `Dashboards.hs` (see above), which matter because they have *drifted*, not
because they are long.

**Conclusion.** The long functions in this codebase are long because markup and SQL are
verbose, not because anything is repeated. `dashboardPage_` is 326 lines with exactly one
repeated 4-line block in it. Consolidating distinct markup does not reduce anything — it
relocates it. Combined with the corrected size figures (+411 net code across the sweep,
69% of `src/` is code, 18% documentation), the "verbosity and no reuse" premise does not
survive measurement.

## The five bulk-action handlers — one silent outlier, now fixed

Five routes share the `Capture "action" Text` + `case action of` shape. Surveyed all of
them for what happens on an unrecognised action:

| handler | unknown action | verdict |
|---|---|---|
| `alertBulkActionH` (monitors) | `_ -> pass` — **200, fires `monitorsListChanged`, does nothing** | **defect, fixed in `7058d5c7`** |
| `anomalyBulkActionsPostH` | `throwError err400` naming the action | correct |
| `dashboardBulkActionPostH` | `addErrorToast "Invalid action"` | correct |
| `apiCatalogBulkActionH` | `throwError err400` naming the action | correct |
| `manageTeamBulkActionH` | `toastError "Invalid action"` | correct |

Four of five fail visibly; one reported success while doing nothing. That is the
uniformity problem in its clearest form — not five different designs, but one silent
divergence from a norm the other four already hold.

**The other four are deliberately left stringly-typed.** They are still producer/consumer
literal pairs that could drift, but a drift is *visible* (400 or an error toast), so the
marginal value of a sum type is much lower than it was for monitors, where drift was
silent. Typing each would cost ~20 lines for a guarantee already enforced at runtime with
a clear error. Revisit if one of them grows a third consumer, which is what made the
monitors case worth it.

Note for whoever does revisit: **they need four separate types, not one shared enum.** The
vocabularies are disjoint (`deactivate|reactivate|mute|unmute|resolve|delete` vs
`acknowledge|unacknowledge|archive|unarchive` vs `delete|add_teams` vs
`archive|unarchive`). A shared `BulkAction` type would make `mute` representable on the
dashboards route. A blind `sed` over `Capture "action" Text` hits all five — I did exactly
that and caught it in the output before it landed.

## Silent no-op fall-throughs, codebase-wide: 13, one worth fixing

Scanned every `_ -> pass|pure ()|mempty|pure Nothing` arm in `src/`. Most are correct:
`Config.hs:492` covers migration success (the failure arm `error`s loudly);
`Settings.hs:1612` covers a normal non-trialing subscription, with the anomalous
"trialing but no trial_end" case logged at attention on the line above; the `Discord.hs`
arms are on Discord's own foreign enums.

**`Anomalies.severityBadge_` is the one to fix.** All three callers hold a typed severity
and throw the type away to call it:

```haskell
severityBadge_ (display issue.severity)   -- x3
severityBadge_ :: Text -> Html ()
severityBadge_ = \case
  "critical" -> ...
  "warning"  -> ...
  _ -> pass
```

`data IssueSeverity = Critical | Warning | Info | Low`, so `Info` and `Low` render no
badge. That is probably intended — but it is expressed as a fall-through on a
round-trip through `Text`, which means a renamed constructor silently removes *both*
badges rather than failing to compile. Taking `IssueSeverity` directly and spelling out
`Info -> pass` / `Low -> pass` makes the omission a stated decision, drops three `display`
calls, and turns a rename into a compile error.

`issueStateBadge_` (`:2013`) is the same shape on `Maybe IssueEvent` — six events badged,
the rest silently unbadged. Lower value (the events genuinely are a long tail and most
should not badge), but it is the same trade if anyone touches it.

## Boolean blindness survey — 14 signatures, none urgent

Scanned `src/` for two adjacent `Bool` parameters (the classic silently-swappable
argument pair). Several hits are false positives (`isDemoAndNotSudo`, `isTrue`,
`monitorStatus`, `errorGroupEvidenceMet` — argument plus `Bool` *return*).

The genuine ones cluster in two places:

- **Pricing UI** — `paymentPlanPicker` and `popularPricing` take *three* adjacent `Bool`s;
  `pricingCta_`, `systemsPricing`, `pricingPage`, `notifChannelsWithUrls` take two. A swap
  renders the wrong pricing state silently. Worth a record type if anyone touches these.
- **`Telemetry.writeTargetFor :: Bool -> Bool -> Maybe Text -> WriteTarget`** — routes
  writes between Postgres and TimeFusion, so a swap would send telemetry to the wrong
  store.

`writeTargetFor` is **already defended and needs no change**: four doctests pin every flag
combination and both DLQ failure markers, and all seven call sites read
`writeTargetFor appCtx.env.enablePostgresTelemetryWrites appCtx.env.enableTimefusionWrites`
— the field names make the order checkable at a glance. Newtypes would upgrade that to a
compile error, but the marginal safety is small against touching the ingestion path.

Recording the pattern rather than fixing it: this is a real class, the instances are all
currently correct, and none of them is worth an unverifiable change tonight.

## Next up: `GroupVerdict` is round-tripped through `Text` (not yet done)

Same shape as the `IssueSeverity` fix in `5134a124`, with a DB boundary in the middle:

- `data GroupVerdict = Param | Routes | Mixed` derives `Display` via `WrappedEnumSC` but
  **not** `FromField`/`ToField`.
- It is written with `display v`, stored as text, read back as `verdict :: Text`
  (`Models/Apis/PatternMerge.hs:230`, `Models/Apis/Endpoints.hs:662`), then compared
  `r.verdict == "param"` at `BackgroundJobs.hs:3900` and `:5358`.

A typo or rename in either comparison silently means "no groups confirmed" — the merge
pipeline quietly does nothing, which is the same failure class as the monitors bulk
action. The fix is the one CLAUDE.md prescribes and `IssueSeverity` already uses: add
`FromField, ToField, HI.DecodeValue, HI.EncodeValue` to the `deriving ... via
WrappedEnumSC` line, type both record fields as `GroupVerdict`, and compare against
`Param`.

One risk to check first: a stored row whose verdict is not one of the three would now fail
to *decode* rather than silently compare false. Both writers go through `display v`, so
only the three values can be stored — but confirm before changing, because a decode
failure in a background pipeline is louder than a silent false.

Deliberately not done tonight: I had seven unexecuted doctests and a repeatedly-truncated
suite run outstanding. Adding more unverified changes on top of unverified changes is how
the false-green problems earlier in this file happened. Verification first.

## Enum-ish record fields still typed `Text`: 35

Scanned for record fields named `status|kind|state|mode|severity|direction|action|level|
verdict|source|paymentPlan|…` typed `Text`. Most are legitimately at a boundary — form
inputs (`AlertUpsertForm`) take raw posted values, `Web/ApiTypes` are wire types. The ones
worth typing are the *domain* records:

- `Monitors.QueryMonitor.severity` / `.visualizationType` — DB-persisted domain rows.
- `Projects.paymentPlan` — appears as `Text` in five records while a `PlanName` newtype
  already exists.
- `GroupReview.verdict` in two modules — see above.

None are defects today; all are seams where the next silent divergence will come from.

## ~90 pending examples every run — mostly legitimate, one dead file

Every suite run reports 84–90 *pending*. Pending reads as green, so it is worth knowing
what is actually being skipped. Across the log:

| skips | reason |
|---|---|
| 865 | `monoscope exe not built — set MONOSCOPE_BIN or run cabal build all first` |
| 308 | `Set MONOSCOPE_API_KEY …` — hits the live API, correctly skipped locally |
| 42 | CLI binary not built |
| 27 | **no reason given** |
| 24 | documented (`no fixture reaches the backend: unknown fields are rejected at parse time`) |

The exe/CLI skips are structural: the dev loop runs `cabal repl`, so `exe:monoscope` is
never built and those tests never execute locally. They are not broken — but nobody should
read a local green run as covering them.

**`test/integration/Opentelemetry/OtlpServerSpec.hs` is dead and should be deleted.** All
42 lines of it: one `it "should process a request" $ const pending` that asserts nothing,
plus 30 lines of commented-out tests written against `runM . evalState mockDB . runReader
…` — a `freer-simple`-style stack this codebase abandoned for `effectful`. They cannot be
uncommented as written.

No coverage is lost. OTLP ingestion is exercised by `GrpcIngestionSpec` (865 lines, 41
examples), `TimefusionWriteFailureSpec` (414 lines, 15), and `StandardOtelSpansSpec` (145
lines, 7). `OtlpServerSpec`'s only `processList` mentions are inside the dead comments.

Deleting it needs `hpack` afterwards — `monoscope.cabal` lists the module in
`other-modules` even though `hspec-discover` finds specs automatically.

## Doctests verified — and three defects behind one three-line example

`Examples: 1470  Tried: 1470  Errors: 0  Failures: 0`. Tonight's eleven new doctests now
provably execute. **The count matters as much as the pass**: the suite reported 1434
before, so a green run still showing 1434 would have meant the new examples were never
extracted — the exact failure that could not be ruled out when neither `LogQueries` nor
`Endpoints` had a `$setup` chunk. 1434 → 1467 → 1470 is what made "they run" a fact.

Getting `reportDayLabels` green took three iterations, each defect hidden by the previous:

1. **`read` is not in scope** — Relude hides partial `read`, so
   `read "2026-03-01 20:00:00 UTC"` could never compile. The timezone arithmetic had been
   verified independently against the IANA database and was correct; that verified the
   wrong thing entirely.
2. **Prose after a `>>>` is parsed as expected output.** The `$setup` explanation sat
   below `>>> let marchUTC ...`, so doctest compared the prose against the `let`'s result.
   Prose must come *before* the examples in a chunk.
3. **An orphaned Haddock block**, surfaced only because fixing (2) meant reading the
   region. `reportDayLabels` (`4940a9f7`) had been inserted *between* `renderWeeklyEmail`'s
   doc comment and `renderWeeklyEmail` itself. It compiled cleanly and no test could ever
   have caught it.

Operational notes for the next person:
- `cabal test doctests --test-options="src/Foo.hs"` **does not narrow the run** — the
  runner already discovers every module, so passing a path yields
  `module 'Pages.Foo' is defined in multiple files`. Verification is all-or-nothing, ~14
  minutes.
- Stop the `live-test-dev` watcher first (`kill` the `make` pid). `cabal test doctests`
  builds the 197-module test-dev target into the directory that watcher owns.

## Doctest gaps in pure branching code — a prioritised backlog

Found by inverting the search: instead of hunting suspicious code, look for **pure
functions with real branching logic and no doctest**. That inversion is what surfaced the
two bugs in `42374bec` (`truncateMiddle` breaking its own "at most n chars" contract, and
`deleteParam`'s unanchored regex) — both look fine on inspection; the defects only appear
when you compare what each promises against what it does at the edges.

190 such functions. The ones worth doing, largest and most edge-case-dense first:

| lines | function | why |
|---|---|---|
| 58 | `ErrorFingerprint.parseJsFrame` | pure parser, three documented formats, none pinned; feeds error grouping. The module has 47 doctests but not this one |
| 46 | `OtlpServer.migrateHttpSemanticConventions` | old→new OTel attribute mapping, pure, silent if wrong |
| 40 | `Telemetry.buildSpanTree` | orphans, missing parents, cycles — the waterfall depends on it |
| 39 | `ProcessMessage.httpKeyOf` | ingestion hot path |
| 143 | `OtlpServer.convertSpanToOtelLog` | large, but mostly field mapping |

Note the ones to *skip*: `buildTitlePrompt`/`buildDescriptionPrompt` (LLM prompt text —
asserting on prose is churn), `apiKeyColumns` (table config), `widgetToECharts` (a large
JSON literal; a golden test suits it better than a doctest).

**Method warning — a fifth measurement bug.** The scan that produced this list was wrong
on its first run: a function's *definition* line sits at column 0 just like its signature,
so "top-level declaration" splits the two and every candidate measured as 1 line, with the
"has real logic" regex examining only the signature. Merging each signature with its
following same-named definition fixed it. (The earlier largest-functions table was
unaffected — it picked up the definition entries, which do span the body.)

## Two stack-trace parsers — the one real duplicate subsystem

The largest genuine duplication in the codebase, and the closest thing found to the
"reinvent patterns we had equivalent versions of" complaint:

| | `Pkg/StackTrace.hs` (272 lines) | `Pkg/ErrorFingerprint.hs` (parsers, ~250 lines) |
|---|---|---|
| type | `Frame` | `StackFrame` |
| entry | `parseStackTrace :: Text -> [Frame]` | `parseStackTrace :: Runtime -> Text -> [StackFrame]` |
| strategy | format-sniffing (`asum [pyFile, atFrame, php, ruby, bareLocation]`) | runtime-dispatched (7 per-language parsers) |
| languages | JS, Python, Java, Ruby, Go, PHP | JS, Python, Java, Go, PHP, .NET, generic |
| consumers | `Pages/LogItem`, `Pages/Components` — display | `BackgroundJobs`, `PatternMerge`, `Telemetry` — fingerprinting |

**Two exported functions share the name `parseStackTrace` with different signatures.** Both
parse the same frame formats; neither shares a line with the other. Both are doctested.

**Do not merge them without treating it as a data migration.** The fingerprint path runs
`parseStackTrace -> normalizeStackTrace -> modulePart|funcPart|contextPart -> hash`, so any
change in parsing *moves error-grouping hashes* and existing issues regroup. The sweep
already has a precedent for how much that matters: the hex-shatter fix moved 1486 distinct
hashes to 783. A parser unification is worth doing — roughly 200 lines — but it needs the
same measured before/after that fix had, not a refactor commit.

The display side is the safe half: `Pages` could move onto the fingerprint parser (or vice
versa) without touching a hash, if one of the two types absorbed the other's fields.

### A harmless wart found while reading it (reasoned, not executed)

`parseJsFrame`'s parenthesised branch does `T.breakOn " (" txt`, giving
`rest = " (/app/src/x.js:12:5)"`, then `T.dropAround (`elem` "()")`. The **leading character
is a space**, so `dropAround` stops immediately and only the trailing `)` is removed —
leaving `filePath = " (/app/src/x.js"`.

It is invisible to every consumer, which is why it has survived: `moduleName` goes through
`moduleFromPath`, which takes the *basename* (`splitOn "/"` then `last`), so the `" ("`
lands in a discarded segment; `isInApp` uses `isInfixOf`; and `StackFrame.filePath` is
never read outside the module. The existing doctest
`normalizeStackTrace RNodejs … → "server|handleRequest"` passes for exactly this reason.

Worth fixing only if anyone starts *using* `filePath` — e.g. rendering a file link, which
would show `" (/app/..."`. Confirm first with a doctest projecting `.filePath` on a
parenthesised frame; the existing ones project only `.functionName`, `.isInApp` and
`length`, which is why it has never been caught. **This is a reading of the code, not an
executed result.**

## Finding reimplementation, not copy-paste

The clone scan found **zero** duplicated 6-line blocks — but it found the two stack-trace
parsers not at all, because they share no text. **Clone detection finds copy-paste; it
cannot find reimplementation.** A better signal for this codebase: the *same function name
defined in more than one module*. That is how `parseStackTrace` surfaced.

14 such collisions exist. Triaged:

**Genuine reimplementation (recorded above):** `parseStackTrace` — `Pkg/StackTrace.hs` vs
`Pkg/ErrorFingerprint.hs`.

**Coincidental, correctly separate:** `runServer` (gRPC vs HTTP), `count` (metric counter
vs a field), `issueTypeBadge` (HTML vs email rendering — different output media),
`nonBlank`, `plainCell`, `renderNameCol` (module-local table helpers).

**Worth a look if anyone is in the area:** `truncateText` (`EmailTemplates` vs `Web/MCP`),
`toolError` (`Pkg/AI` vs `Web/MCP`), `servicePicker_` (`RealUserMonitoring` vs
`Pages/Telemetry`), `parseInstallState` (`Bots/Utils` vs `GitSync`), `renderTable`
(`Components/Table` vs `Components/Widget`).

### `PrometheusScrapeConfigs.selectFrom` — hand-rolls what `GenericEntity` derives

`selectFrom :: HI.Sql` and a hand-written `selectCols` list, where every sibling model uses
`deriving (Entity) via (GenericEntity '[Schema …, TableName …, PrimaryKey "id",
FieldModifiers '[CamelToSnake]] T)` and `DeriveUtils.selectFrom @T`. The hand-written list
is character-for-character what `CamelToSnake` over the record fields would produce.

**Not changed, and the reason matters:** the apparent risk is drift — add a field to the
record, forget the list, get a `DecodeRow` mismatch at runtime. But `PrometheusSpec`
exercises both paths (`configsByProjectId` via `selectFrom`, `claimDueConfigs` via
`RETURNING selectCols`), so that drift fails a test rather than reaching production. It is
a consistency nit, not a latent bug, and not worth touching query generation for.

If someone does adopt it: `selectCols` must stay for the `RETURNING` clause, which needs
bare column names — so the *complete* fix also wants a `columnsOf @e` helper in
`DeriveUtils` (it currently imports only `_select` from pg-entity). Half-doing it leaves
the same hand-maintained list behind.

### A note on my own work tonight

`Pages/RealUserMonitoring` already had `parseTab`/`tabParam` built on
`decodeEnumSC`/`encodeEnumSC`. My `IssueTab` work added a second `parseTab` with a
hand-written `tabSlug`. The hand-written slug is *justified* — the wire spellings are
capitalised (`?filter=Acknowledged`) and `encodeEnumSC` snake-cases — but I named it
`tabSlug` where the established convention is `tabParam`. Same pattern, gratuitously
different name. Worth aligning if either is touched again.

## `servicePicker_` — the clearest instance of the uniformity complaint

Two pages render a control for the same user action ("filter this page to one service"),
and they do not look or behave alike:

| | `Pages/RealUserMonitoring.hs:696` | `Pages/Telemetry.hs:666` |
|---|---|---|
| signature | `RumData -> Html ()` | `ProjectId -> Text -> Html ()` |
| control | native `<form>` + `<select>`, `onchange="this.form.requestSubmit()"` | `<button>` + popover `<div class="dropdown">` |
| search | none | yes, an `<input>` inside the panel |
| empty state | renders anyway — *"that is exactly when the user needs the control that got them there in order to get back out"* | n/a |
| label | "All services" | "All Services" |

Even the default option's capitalisation differs. This is what "the platform is not uniform
anymore" looks like concretely: not duplicated code — they share almost no text — but two
answers to one interaction question.

Note which is *better* by the escalation ladder: RUM's is tier 2 (native select + form
submit, no JavaScript), Telemetry's is a custom popover widget. The ladder prefers RUM's —
but Telemetry's has search, which matters once a project reports many services. So the
consolidation is a real design decision ("native select below N services, searchable
popover above"), not a mechanical merge, and it needs a browser to verify. Recorded, not
attempted.

## Name-collision triage — complete

14 function names are defined in more than one module. Full triage so nobody re-derives it:

- **Genuine reimplementation (1):** `parseStackTrace` — see the stack-trace section.
- **Genuine uniformity defect (1):** `servicePicker_` — above.
- **Coincidental, correctly separate (12):** `runServer` (gRPC vs HTTP), `count` (metric
  instrument vs a field), `issueTypeBadge` (HTML vs email — different output media),
  `truncateText` (two deliberate truncation *policies*; MCP's marker is documented as
  intentionally not-JSON), `toolError` (`Text` message vs `AE.Value` tool result),
  `parseInstallState` (two different OAuth state encodings: `pid__x` vs `pid:dest`),
  `renderTable` (`Widget`'s is a thin delegate to `renderTableShell`), `selectFrom`,
  `parseTab`, `nonBlank`, `plainCell`, `renderNameCol`.

**The lesson about the method:** the clone scan reported zero duplication at six lines and
missed both real findings, because neither is copy-paste. Name collision found both. When
looking for "we reinvented something we already had", grep for *names*, not *text*.

## `statusBadge_` — five dead cases, and the one live value falls through

`Pages/Monitors.hs:670`. Found by the near-name scan (`statusBadge_` in Monitors vs
`statusBadge` in Infrastructure — those two are legitimately separate, but looking at them
turned this up).

`statusBadge_ :: Bool -> Text -> Html ()` matches eight status strings. Checked which are
ever *produced* anywhere in `src`:

| case | produced? |
|---|---|
| `Passing`, `Failing`, `Healthy`, `Pending`, `NoData` | **never — 0 occurrences** |
| `Active` | only as a *tab* name (`monitorTabParam`), never as a status |
| `Warning`, `Inactive` | yes, handled correctly |
| `Alerting` | yes — not in the case list, but caught by the `isAlerting` guard |
| **`Normal`** | **yes — and falls through to the default** |

`statusInfo` is the only producer of these labels and emits `Alerting | Warning | Normal`.
So five of eight explicit cases are dead, and the live value the list *omits* lands on
`_ -> ("badge-ghost", "circle")`.

**The visible consequence:** the same state is coloured twice, differently, in one module.

```haskell
statusInfo MSNormal = StatusInfo "bg-fillSuccess-strong" "Normal" …   -- dot: green
statusBadge_ "Normal" → _ -> ("badge-ghost", "circle")                -- badge: grey
```

A healthy monitor shows a green dot and a grey badge on the detail page (`:781`, `:837`,
`:848` pass `displayName`, which is `"Normal"` for a live, non-deactivated monitor). The
list even contains three unused synonyms — `Passing`, `Healthy`, `Active` — that would
each have rendered it green.

**Recommended fix:** add `"Normal" -> ("badge-success", "circle-check")`, matching the
`bg-fillSuccess-strong` that `statusInfo` already assigns to `MSNormal`. That is a visual
change, so it wants a browser before it lands; the dead cases should be *mentioned* rather
than deleted per the house rule on unrelated dead code.

**The deeper cause is the round-trip**, the same one fixed for `IssueSeverity` in
`5134a124`: a typed `MonitorStatus` is rendered to `Text` by `statusInfo`, then re-matched
against string literals by `statusBadge_`. Taking `MonitorStatus` directly would have made
the missing `Normal` case a compile error and the five dead cases unrepresentable.

## My tab consolidation was module-local — the nav still hardcodes the same strings

`Pages/BodyWrapper.hs:882 navFlyoutItems` hardcodes the wire spellings that `IssueTab` and
`MonitorTab` now own:

```haskell
"Issues"   -> [("Inbox", p "/issues?filter=Inbox"), ("Acknowledged", …), ("Archived", …)]
"Monitors" -> [("Active", p "/monitors?filter=Active"), ("Inactive", …)]
```

`1652d04b` collapsed three sources of `"Inbox"/"Acknowledged"/"Archived"` inside
`Anomalies.hs`, and `7810598b` three sources of `"Active"/"Inactive"` inside
`Monitors.hs`. Neither touched the nav, which holds a fourth and a third copy. They agree
today; nothing enforces it. Rename a tab and the flyout links silently point at
`?filter=OldName`, which `parseTab` folds back to Inbox — a dead link that looks like a
working one.

**Why it was not fixed here:** `Pages/Anomalies` and `Pages/Monitors` both *import*
`Pages/BodyWrapper` (for `BWConfig`, `PageCtx`, `mkPageCtx`, `navTabAttrs`), so
`BodyWrapper` cannot import them back. Closing this properly means moving `IssueTab` and
`MonitorTab` into a module all three can see — a small `Pages/Tabs.hs` is the obvious
home — and re-exporting from the pages. That is a multi-file move, not a one-liner.

**The general lesson, and it applies to every type fix in this sweep:** consolidating the
sources of a string *inside* one module does not find the copies in modules that cannot
import it. The grep that finds them is for the *literal*, run across the whole tree, after
the type exists — not for the type's name. I checked call sites of the functions I
changed; I did not re-grep the literals afterwards, which is why this survived.

## Two operational rules learned the hard way tonight

**1. Stop a watcher by killing its `make` pid, never `pkill -f 'live-test-dev'`.**
`pkill` matches the `make` wrapper but not the `ghcid` it spawned, so the ghcid survives
with `ppid=1` — still watching files, still able to re-run the suite into a build directory
another process is using, and holding ~3.5 GB. I did this to myself and then found it in a
straggler sweep. `kill <make-pid>` takes the whole chain down; `ps -eo pid,ppid,command |
grep test:test-dev` shows the chain to confirm.

**2. Never anchor a code insertion on a function's *signature* line.** Doing so places the
new code between the target's Haddock block and the function it documents. It compiles
cleanly — Haskell does not care — and the only symptom is a doctest failure elsewhere,
because expected output runs until a blank line and now continues into the inserted
Haddock. This happened twice tonight (`reportDayLabels`, then `sessionSortParam`). Anchor
on the *start of the Haddock block*, or insert after the target's definition.

Corollary for reading such a failure: the diff is useless. It printed

```
expected: ["last_seen","duration_ns","error_count","event_count"]
 but got: ["last_seen","duration_ns","error_count","event_count"]
```

— identical, because the difference was appended prose beyond the visible line. What
located it was `sed` refusing the file with `RE error: illegal byte sequence` (an em-dash
nearby). **When a diff shows two identical strings, stop reading the diff and dump bytes.**

## Checking every `WrappedEnumSC` enum for hardcoded wire strings

Generalised the literal-grep: for each of the 32 `WrappedEnumSC` enums, compute its wire
slugs and look for them hardcoded outside the owning module. High false-positive rate —
short slugs like `error`, `info`, `service`, `events` collide with route paths, CSS classes
and field names — but two hits were the same concept:

**Fixed (`91bf6807`): `allChannels`.** `Pages/Projects.hs` listed
`["email","slack","discord","phone","pagerduty"]` while `NotificationChannel` derives
exactly those. The code documented its own bug — *"If a new channel type is ever added, it
must also be added to `allChannels` or it will silently be treated as enabled for every
project"* — which is a comment doing a type's job. Now `map display [minBound .. maxBound]`,
pinned by a doctest because those spellings are what the form posts and what
`disabled_channels` stores.

**Recorded, not fixed: `"above"`/`"below"` in `Pages/Monitors.hs`.** Three literals — the
dropdown values (`:305`), its selected-value `bool` (`:305`), and
`triggerLessThan = alertForm.direction == "below"` (`:156`). The failure mode is nasty: if
the dropdown value ever changed case, the comparison silently yields `False` and **alerts
fire in the wrong direction**.

Left alone because the obvious fix is disproportionate and slightly wrong. `Monitors.hs`
does not import `Models.Apis.Issues`, and `ThresholdDirection` belongs to a different
domain — it describes a *fired alert's* direction (`QueryAlertData.thresholdType`), while
a monitor stores `triggerLessThan :: Bool`. Typing the form field would need a
cross-module dependency plus a `FromHttpApiData` instance, to remove three literals that
sit in one file 150 lines apart. Worth doing only if the form gains a third direction or
the monitor starts storing the direction rather than a Bool.

## `monoscope-shared` is built by three consumers with three flag sets

I broke the build tonight and the root cause is worth knowing, because it is a property of
the dev loop rather than a one-off mistake.

`monoscope-shared` is compiled by:

| consumer | flags |
|---|---|
| `make live-reload` (`cabal repl monoscope`) | `-j10 -O0 -Wno-error=unused-imports -Wno-error=unused-top-binds` |
| `make live-test-dev` (`cabal repl …test-dev`) | `-j10 -fobject-code -osuf dyn_o -hisuf dyn_hi -O0` |
| `cabal test doctests` / `cabal build` | defaults |

Whichever ran last wins, and the losers see mismatched artifacts. The symptom is
`Dynamic hash doesn't match for 'Pkg.CLIFormat'` — the `.hi` and `.dyn_hi` disagree. My
notes already said "stop the test-dev watcher before running doctests"; that is
insufficient, because the **library** watcher uses the same shared package.

**Then two repair attempts each made it worse:**

1. `rm -rf …/monoscope-shared-0.1.0.0/build` removes the artifacts but leaves the package
   **registered**, so cabal reports *"There are files missing in the
   'monoscope-shared-0.1.0.0' package"* instead of rebuilding.
2. Rebuilding with plain `cabal build lib:monoscope-shared` uses *default* flags. `cabal
   repl` then wants `-O0` and must relink, which fails on macOS aarch64 with
   `ld: invalid use of ADRP/imm12 in '' to '_stg_upd_frame_info'` — mixed objects.

**The repair that works**, narrow and without `cabal clean` (which CLAUDE.md forbids and
which would cost an hour of dependency rebuilds):

```sh
kill <make-pid>   # both watchers
rm -rf dist-newstyle/build/aarch64-osx/ghc-9.12.2/monoscope-shared-0.1.0.0 \
       dist-newstyle/packagedb/ghc-9.12.2/monoscope-shared-0.1.0.0-inplace.conf
cabal build lib:monoscope-shared \
  --ghc-options="-j10 -O0 -Wno-error=unused-imports -Wno-error=unused-top-binds"
# then restart live-reload, wait for "All good", then live-test-dev
```

Directory **and** registration, and rebuild with the *watcher's* flags, not the defaults.

## `Containers` and `Infrastructure` — three small divergences

`Infrastructure.hs` (1006 lines) already imports `Pages.Containers` (374), so these are not
independent modules — Infrastructure builds on Containers. They share exactly one
top-level name, but three things have drifted:

**1. `plainCell` is defined in both, and they differ.**

```haskell
-- Containers:      maybe emDash_ (span_ [class_ "whitespace-nowrap text-textStrong tabular-nums"] . toHtml)
-- Infrastructure:  maybe (span_ [class_ "text-textWeak"] "—")
--                    (\v -> span_ [class_ "block truncate whitespace-nowrap …", data-tippy-content=v] …)
```

So "a plain table cell" truncates with a tooltip on one page and not on the other. Not
mergeable without choosing which behaviour is right — a design call, like `servicePicker_`.

**2. `emDash_` is private to `Containers` (6 uses); `Infrastructure` inlines its body**
(`span_ [class_ "text-textWeak"] "—"`) byte-identically. Exporting it is a zero-risk
one-liner, since the markup is the same.

**3. Kubernetes readiness renders two ways.**

| | Infrastructure | Containers |
|---|---|---|
| model | `KubeStatus = KubeReady \| KubeNotReady \| KubeUnknown` | `ready :: Maybe Int` |
| ready / not ready | `badge-success` "Ready" / `badge-error` "Not ready" | identical |
| unknown | `badge-ghost` **"Unknown"** | **em-dash** |

Same labels and badge classes, different unknown handling — one shows a badge, the other
shows nothing. Merging needs the `Maybe Int` mapped onto `KubeStatus`, which is a modelling
decision, not a rename.

None is a defect; all three are the "platform is not uniform" complaint at small scale,
in two files that already depend on each other.

## Archiving an issue has three implementations that disagree

Found by grouping SQL by `(operation, table)`: `UPDATE apis.issues` appears 18 times across
5 modules. Three of them archive an issue:

| path | `updated_at` | cascades to `apis.anomalies` |
|---|---|---|
| API — `ApiHandlers` → `Issues.setArchiveState` | **bumped** (`COALESCE(#{mTs}, updated_at)`) | no |
| UI bulk — `Anomalies.archiveAnomaliesAndIssues` | not touched | yes |
| UI single — open-coded at `Pages/Anomalies.hs:143` | not touched | yes |

**The `updated_at` split is user-visible.** `selectIssues` can order by `updated_at`, so
archiving through the API moves an issue to the top of that sort and archiving through the
UI does not. Same operation, different result depending on which client you used.

**The cascade split is benign — and reveals dead work.** `apis.anomalies.archived_at` is
written by both UI paths and never by the API. It is also **never read**: the only two
readers of `apis.anomalies` filter on `created_at` (`Projects.hs:556`, an activity check)
and on `project_id`/`target_hash` (`Endpoints.hs:961`, merge logic). So the column is
written by two paths, skipped by a third, and consulted by none.

Not fixed, because unifying requires deciding two things a refactor cannot: whether
archiving should bump `updated_at` (it changes list ordering either way), and whether the
`apis.anomalies` write should continue at all now that nothing reads it. The single-issue
handler at `Pages/Anomalies.hs:143` open-coding what `setArchiveState` already does is the
part that is unambiguously worth folding in once those are settled.

Same shape as the onboarding-step find in `b30d874f`: a canonical helper exists in the
Models layer, and the Pages layer open-codes a variant of it that has quietly diverged.

## Four lenses for "we reinvented something we already had" — and what each misses

Tonight used four different scans to look for duplication. They found disjoint sets, which
is the point: **each is blind to what the others catch.** If you repeat this sweep, run all
four, and do not read a zero from one as an answer from all.

| lens | how | found | blind to |
|---|---|---|---|
| **Clone detection** — normalised N-line windows, identifiers and literals replaced | `dup3.py`-style hashing | **nothing** (0 duplicated 6-line blocks in 77k lines) | anything reimplemented rather than copied |
| **Name collision** — same function name in >1 module | grep `^name ::` | the two stack-trace parsers; `servicePicker_` | reimplementations that were renamed |
| **Literal grep** — take a type's wire strings, grep the tree | per enum, after the type exists | `allChannels`; the nav's hardcoded tab strings; the sort dropdown | operations that share no literal |
| **SQL by (operation, table)** — group every query by what it touches | regex over `[sql\| …\|]` | the onboarding lost-update race; three disagreeing archive paths | anything not expressed as SQL |

The SQL lens was the most productive and the least obvious. `UPDATE projects.projects`
appearing 20 times across 4 modules — three of them `Pages` — is what pointed at both the
layering problem and the race. Neither clone detection nor exact-SQL comparison saw it:
each site is a single long line, and the literals differ.

**The recurring shape, seen three times:** a canonical helper exists in the Models layer,
and the Pages layer open-codes a variant that has quietly diverged — onboarding steps
(read-modify-write vs atomic), archiving (`updated_at` bumped or not, cascade or not), and
the inline `UPDATE projects.projects` in `Log.hs`/`Monitors.hs`. Grepping for *the table a
handler writes to* finds these; grepping for *the helper's name* does not, because the
whole problem is that the helper's name is absent.
