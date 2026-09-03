# Quality sweep — findings and open decisions

Branch `quality-sweep-2026-09-02`, from `master@44c38b1c`.
All three skills (`/hs-distill`, `/hs-lob-review`, `/hs-evasion-review`) applied across
feature groups: log explorer, service map, replay, bots, monitors, issues, endpoints,
billing, auth/server/MCP, the pattern/AI pipeline, and the shared UI components.

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

1. **Basic auth mints a `persistent_sessions` row per request.** Browsers resend
   `Authorization` on every request, so a `BASIC_AUTH_ENABLED` deployment gets one
   INSERT per page load. Same failure class as the 2,569,021-row demo-guest incident
   documented ~100 lines away in `Web/Auth.hs`. The fix reorders auth resolution.
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
7. **`apiMonitorPatch` never recomputes `logQueryAsSql`**, so patching `query` leaves
   the compiled SQL that drives alert evaluation stale.
8. **`AuthContext` carries `EnvConfig` twice** (`env` and `config`), set from the same
   value, used interchangeably, with nothing enforcing they stay equal.
9. **`ErrorPatterns.getErrorPatternById` is unscoped** — not currently exploitable, as
   all three handler callers guard `err.projectId /= pid` explicitly, but the guard is
   repeated rather than structural.

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
- **Stringly-typed `runtime`** in `ErrorFingerprint.parseStackFrame` with an `otherwise`
  catch-all — a typo at a caller silently produces a different fingerprint.
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

2. **`TestClock.syncConnectionTime` is never called** — only referenced from two comments,
   one of which (`TestUtils.hs:611`) claims SQL going through `app_now()` "sees the same
   clock too". If nothing ever syncs the connection, that claim may not hold and
   time-sensitive SQL in tests could be reading wall-clock. **Not changed** — the whole
   suite passes today, so touching test-time plumbing needs a deliberate look rather than
   a drive-by fix. Flagged for a human.

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
