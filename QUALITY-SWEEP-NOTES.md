# Quality sweep — overnight run, 2026-09-01

Branch: `quality-sweep-2026-09`, forked from `823431ef` on `master`.

**Goal (owner's words):** the last six months of code went unreviewed — verbose, little
reuse, not strongly typed, and the platform lost its uniformity because features
reinvented patterns that already existed. Drastically reduce code size, strengthen types,
componentalise and reuse. Features must not be lost, but may be tweaked.

**Method:** feature-area groups, not per-commit. 1366 commits × 3 skills does not fit in a
night and would review stale intermediate states. Groups are picked by churn × size.

## Baseline

| | |
|---|---|
| `src/**/*.hs` | 75381 lines |
| HEAD at fork | `823431ef` |

Largest / highest-churn modules at baseline:

```
5095 src/BackgroundJobs.hs            (241 commits)
3050 src/Models/Telemetry/Telemetry.hs (109)
2635 src/Pages/Dashboards.hs           (84)
2059 src/Pages/Anomalies.hs           (105)
1997 src/Pages/LogExplorer/Log.hs     (133)
1976 src/Utils.hs                      (69)
1841 src/Opentelemetry/OtlpServer.hs   (81)
1693 src/Pages/Settings.hs             (78)
1671 src/Pages/Telemetry.hs            (98)
1644 src/Pages/Projects.hs             (55)
```

## Verification loop

`cabal build` is forbidden here (it races the ghcid watcher — see CLAUDE.md and the
"one cabal process at a time" memory). Added `scripts/local/ghcid-wait.sh`: it waits for
the `make live-reload` watcher to settle and exits 0 on "All good", 1 with the compile
errors, 2 if the watcher is stuck. Every automated agent uses that as its build command.

Per group: green build → group edits → `ghcid-wait.sh` green → integration tests →
`make fmt` → one commit. A bad group is revertable without losing the night.

## ⚠️ Port 8080 collides between the two checkouts — do not run `make live-reload` here

`live-reload` depends on `kill-live-reload`, which kills the **process group of whatever
holds `$(PORT)`** (Makefile:56-69). Both `monoscope` and `monoscope-2` default to 8080, so
starting the watcher in one checkout kills the other checkout's watcher *and its app*.
That is exactly what happened tonight: my restart killed the watcher in
`~/Projects/apitoolkit/monoscope` — which has uncommitted work belonging to another Claude
instance — and that instance's restart then killed mine. The `ProcessMessage.hs:221` parse
error I chased was scrollback from *their* tree in a reused tmux pane, not this repo.

**Use instead** a type-check-only ghcid, which never binds the port and so never invokes
`kill-live-reload`:

```
ghcid --command 'cabal repl monoscope --no-semaphore \
  --ghc-options="-j4 -O0 -Wno-error=unused-imports -Wno-error=unused-top-binds" \
  --with-compiler=ghc-9.12.2' --warnings > build.log 2>&1
```

`-j4` rather than `-j10` deliberately: another instance is compiling on the same 10 cores.
If the app itself ever needs running here, set `PORT` to something other than 8080 first.

Second surprise: `monoscope-2/dist-newstyle` contains only `cache/` and `src/` — there is
no `build/` directory, so this checkout has **never been compiled**. The first green build
is a from-scratch dependency build (~56 `source-repository-package`s), not an incremental
one. Budget hours, not minutes.

## Two watchers are alive — watch for phantom errors

`make live-reload` (pid tree from 22:43) and `make live-test-dev` (running since ~10:48)
are both up. The test-dev repl is loaded and idle, so nothing is racing *right now*, and
they use distinct `-osuf`, but once edits start both recompile on the same save. If a
compile error appears that does not match the code, suspect the race before the diff —
see the "one cabal process at a time" memory. `build.log` is authoritative for the
library; `build-test-dev.log` for the suite.

## Survey 1 — UI component duplication (complete)

Important correction to my mental model: the shared component surface is **not** only
`Pages/Components.hs` / `Pkg/Components/*`. A large part of it lives in **`src/Utils.hs`**
(`navTabAttrs`, `explorerNavTabs_`, `infrastructureNavTabs_`, `loadingIndicator_`,
`getDurationNSMS`, `prettyPrintCount`, `summaryStyleClass`, `popoverTrigger_`), which
`BodyWrapper` merely re-exports. Anyone consolidating must treat `Utils.hs` as canonical
or they will create a *fourth* copy.

Findings, ordered by lines deleted:

1. **Tab navigation (~110)**. Three tab bars break the mandated HTMX pattern outright:
   `Dashboards.hs:243-259` uses a `/content` partial endpoint *and* hyperscript
   active-class juggling — both explicitly forbidden by CLAUDE.md — backed by a handler
   (`dashboardTabContentGetH:2427`) that exists only to serve it. `Monitors.hs:814`
   `tabbedSection_` and `Projects.hs:1053` `teamTabsHeader_` each invent a third and
   fourth tab visual language. Separately, the same tab-strip loop is copied six times
   with already-diverged classes; one `navTabStrip_` kills ~45 lines.
2. **Hand-rolled `<table>` bypassing `Pkg/Components/Table.hs` (~90-120)** at twelve sites.
   `Monitors.hs` is notable: it uses `Table.hs` for its main list and a hand-rolled table
   on the same page. A `simpleTable_` beside `renderTable` covers the eight mid-size ones
   and ends four competing `<th>` styles.
3. **Card/panel headers (~55)** — five competing implementations against the existing
   `panel_`/`PanelCfg`. `Projects.hs:962` hand-rolls one and then uses `panel_` correctly
   twenty lines later.
4. **Badges (~50)** — two parallel systems (daisyUI `badge-*` vs custom `cbadge-*`) and
   five separate status→class mappers. **Concrete visible bug:** `Anomalies.hs:562` defines
   a local `severityBadge` in a `where` clause that renders "CRITICAL" differently from
   `severityBadge_` at `Anomalies.hs:1791` — the same file renders the same severity two
   ways depending on whether you are on the detail page or the list.
5. **Duration/byte/number formatting (~45)** — six different renderings of "how long did
   it take". Confirms and widens my own `humanBytes` finding. Also a **correctness**
   divergence, not just duplication: `RealUserMonitoring.hs:1067` formats dates server-side
   with `formatTime` while the rest of the app uses the timezone-correcting `<local-time>`
   element.
6. **Copy-to-clipboard reinvented 8 times (~35)** with four different feedback behaviours
   (toast / inline "Copied!" / nothing) and five different toast strings.
7. **Empty states (~25)** — mostly healthy (27 call sites on the shared one), but
   `Pkg/Components/ServiceMap.hs:285` defines its own `emptyState_` with the same name and
   different markup, which is the collision I had already spotted independently.

Reported healthy, no action: modals, charts/widgets, time pickers.

## Consolidated plan — all five surveys

Rough total identified: **~2,400 lines removable** plus ~935 relocatable, across five areas.
Ordered into commits by (risk ascending × lines descending).

### Commit 1 — zero-risk deletions and exact-duplicate merges (~120 lines, no behaviour change)

- `BackgroundJobs.hs:1022` `parityDrift` has a **literally identical body** to
  `continuityDrop:1089`. Delete it; `continuityDrop` already has two callers.
- `Models/Apis/LogQueries.hs:377` `retryLogExplorerRead` reimplements
  `Telemetry.retryTransientEff:1556` verbatim — same predicate, same
  `100000 * 2^(attempt-1)` backoff, same four log fields. Delete, call the original.
- `Telemetry.objectToMap:206` ≡ `Utils.jsonToMap:1620`. Keep `Utils`'.
- `humanBytes` ×3 (`Telemetry:2685` `Int`, `Settings:1293` `Int64`,
  `Containers:369` `Double` as `formatBytes`) → one `Integral`-polymorphic helper in `Utils`.
- `ratio` (`Containers:89` ≡ `Infrastructure:984`), `nonBlank`, `fmtDate`, `truncateText` —
  see the table above.
- Dead code, zero references anywhere: `ServiceGraph:224` `emptyServiceGraph`,
  `Endpoints:423` `countEndpointInbox` (its last consumer is gone), `Utils:641`
  `checkFreeTierExceeded`.
- `Pkg/Components/ServiceMap.hs:285` `emptyState_` — delete, use the shared one.
- `Pages/Anomalies.hs:562` local `severityBadge` — delete, use `severityBadge_:1791`.
  Fixes a *visible* bug: the same severity renders two ways in the same file.
- Dead client code: `web-components/src/main.ts:342-392` `updateMarkAreas` + helpers
  (~50 lines, zero references), `service-map.ts:719`, `log-list-utils.ts:40`.
- Workbox is dead toolchain: `package.json:10` devDependency, `Dockerfile.deps:30`, and
  `docs/DEVELOPMENT.md:234` documents a `config/workbox-config.js` **that does not exist**.
  `CLAUDE.md:182` is stale for the same reason.

### Commit 2 — two latent correctness bugs found by the type survey (small, high value)

- `Pkg/Components/Widget.hs:53-60` declares `instance {-# OVERLAPPABLE #-} FromJSON a =>
  FromHttpApiData a`. Every module importing `Widget` silently reroutes **all** query-param
  parsing through JSON decoding, overriding the library instances for `Text`, `Int`,
  `UTCTime`, `UUID`. The correct newtype already exists — `Utils.JSONHttpApiData:796`.
- `Pkg/DeriveUtils.hs:291` `WrappedEnumInt`'s `HI.DecodeValue` does
  `fromMaybe minBound . safeToEnum`, so an out-of-range DB int **silently decodes to the
  first constructor**. Its own `FromField` at `:284` correctly `returnError`s. This is in
  the deriving infrastructure, so it affects every enum that uses it.
- `Models/Telemetry/Telemetry.hs:317` makes **every** `ByteString` in the app JSON-encode
  as hex, via a global orphan. Wants a `HexBytes` newtype.
- `Models/Apis/Anomalies.hs` writes `acknowledged_at/by/until` from three places but only
  `Issues.setAckState:647` has the "archive competing recurrences first" guard — the other
  two can hit the partial-unique-index 500 that `Issues.hs:604` documents.

### Commit 3 — client-side tier demotions (~120 lines, high uniformity gain)

- The "filter this list by an input" hyperscript is hand-written at **9 sites**, two of
  them built by string concatenation into `makeAttribute "_"` (so a reader cannot even see
  it is hyperscript). One `install Filterable(...)` behaviour.
- Copy-to-clipboard exists **12 times** across 3 tiers, with 4 different feedback
  behaviours and 5 different toast strings — while `install Copy` already exists at
  `LogExplorer/LogItem.hs:356`. Two sites (`Onboarding.hs:460`, `:589`) are raw JS.
- `Pkg/Components/Table.hs:758` builds a `document.querySelectorAll` **string** to toggle
  column visibility. The control is already a checkbox → `peer`/`group-has-[…]:hidden`.
- Six copies of "Enter/Space makes this div act like a button" — all are `role="button"`
  + `tabindex="0"` on a `div`. Use a real `<button>`/`<label>` and delete the handler.

### Commit 4 — tab navigation uniformity

`Pages/Dashboards.hs:249-256` violates both of CLAUDE.md's explicit "never"s at once: a
separate `/content` partial endpoint, **and** hyperscript managing the active class. Fixing
it also deletes `dashboardTabContentGetH:2427` and collapses the `navigate_to_tab` branch
in `web-components/src/widgets.ts:1093`.

**Doc-vs-code drift to fix first (one line):** CLAUDE.md:400 prescribes `hxGet_` /
`hxSwap_ "morph"` / `#nav-container:morph`, but the canonical helper it points at —
`Utils.navTabAttrs:1504` — actually uses `hxBoost_`, `"outerMorph"` and
`#main-sidenav,#main-navbar`. Three spellings of the preload attribute exist. The doc is
wrong, not the code; ~15 consumers follow the code and are fine.

### Commit 5+ — structural (each its own commit)

- **BackgroundJobs.hs**: `enqueue` helper kills 17 copies of the `withResource
  authCtx.jobsPool` ceremony (~34 lines). `withProject` unifies 14 sites split between a
  silent `whenJustM` and a logging `case` — the comment at `:485` records a **41-day
  revenue incident** caused by the silent variant. Infra health-check triple → one runner
  (~90 of 299 lines). Relocating the endpoint-merge engine (`:3661-4294`) and the infra
  health-check family takes the file from 5095 → ~4160.
- **Types**: adopt enums that *already exist* but are stored as `Text` — `visualizationType`
  → `WidgetType`, `alertStatus` → `MonitorStatus`, `severity` → `IssueSeverity`,
  `disabledChannels` → `NotificationChannel`, `direction` → `ThresholdDirection`. Pure
  substitution, no new types, and each one buys `-Wincomplete-patterns` coverage.
  Then `Issue.issueData :: AE.Value` → a sum type (all five payload records already exist).
- **Notifications**: six Slack/Discord renderer pairs in `Pkg/Mail.hs:237-647` encode the
  same content twice (~200-250 lines). The seam is already visible — `logPatternSeverity`
  returns a 4-tuple carrying *both* a Slack shortcode and a Discord emoji.
- **Tests** (~600 lines): `test/unit/RequestMessagesSpec.hs` is 272 lines of pure
  `shouldBe` against two functions, with IP/port assertions repeated across 10 blocks →
  ~28 doctest lines. `Data/Effectful/NotifySpec.hs` (56 lines) tests the *test interpreter*
  itself and should just go. `Pkg/Parser/ExprSpec.hs`'s only assertion is
  `True \`shouldBe\` True` with the real one commented out.

### Rejected after inspection: gutting `RequestMessagesSpec` (~250 lines)

The test survey's largest single item, and I decided against it. Reasons:

1. **The line saving isn't there.** The bloat is *horizontal* — `RequestMessages.valueToFormatStr`
   repeated 94 times — not vertical. Each assertion is already one line, so a table-driven
   rewrite saves characters, not lines. A per-case doctest (`-- >>> expr` + expected) is
   **two** lines per case, i.e. 358 lines for the 179 cases I extracted — worse than the 309
   it replaces. Only the dense `map f [..]` form is shorter, and that mangles the long
   inputs and drops the per-case comments.
2. **The proposed saving comes from deleting assertions, not relocating them.** "Keep one
   representative case per family" means dropping ~150 of 179. Those cover
   `replaceAllFormats`, which redacts **PII** — SSNs, credit cards, JWTs, emails. Thinning
   a redactor's test matrix to save lines is a bad trade at any line count.
3. **The comments are the documentation.** `"-42" -> Nothing -- Negative integers not
   supported with sign`, `"999" -> {integer} -- Not valid HTTP status`, `"1111111111111111"
   -> {hex_id} -- Not a valid card` record precedence rules that are genuinely surprising.
   That knowledge lives nowhere else.

I extracted all 179 cases mechanically to check this rather than eyeballing it. If someone
still wants it moved, the honest framing is "improve locality", not "delete 250 lines" —
and it should keep every case.

### Explicitly ruled out — do not re-propose

Modals, charts/widgets and time pickers are healthy. `Web/ApiHandlers.hs` is genuinely
clean (`notFoundOr`/`ownedOr`/`bulkExec`/`paged` already absorb the CRUD scaffolding).
BackgroundJobs' scheduling scaffolding is already consolidated (`unlessStale`, `tryStep`,
`seedJobs`) — a generic runner would not pay for itself. The trace-fetch family already
delegates to `getTraceRowsWith`. Partiality is *not* a hotspot: `!!` is absent, `viaNonEmpty
head` is used throughout, and two past `fromJust` crashes are recorded in comments as fixed.
The PG/TimeFusion `Bool` threaded through 63 sites should be **deleted at parity**, not
abstracted — per CLAUDE.md, do not build a `Store` abstraction over it.

## Verification status

| lane | state |
|---|---|
| library (`ghcid`) | **green**, 128 modules, zero warnings |
| `make test-unit` | **green** — 271 examples, 0 failures |
| `make test-doctests` | 1348 examples, **0 errors, 1 failure — pre-existing** (below) |
| `make test-integration` | **not run** (see below) |
| browser / running app | **not run** — port 8080 belongs to the other checkout |

The library watcher compiles `src/` only, so it cannot catch a broken spec. Running the
unit suite immediately found one: moving the retry helpers had broken
`Opentelemetry/TimefusionWriteFailureSpec`. Fixed in `6021f9b6`. **Do not treat a green
watcher as a green tree** — the spec tree needs its own run.

The single doctest failure is at `BackgroundJobs.hs:3246` and is **not** from this work:
the block is byte-identical at the fork point `823431ef` and this branch's diff to that
file never touches it. It fails because the example mixes a package-qualified
`import "monoscope" BackgroundJobs` with a source-tree `import Pkg.EmailTemplates`, so
`System.Types` is loaded twice and its `AuthServerData` family instances collide. Worth
fixing on its own, separately from a refactor branch.

Doctests need `cabal build lib:monoscope` first, or the runner dies with
`cannot satisfy -package monoscope`.

## Landed

| commit | what |
|---|---|
| `a40cdbda` | notes + `ghcid-wait.sh` |
| `9e46edab` | three unreferenced web-component exports |
| `c7c187d6` | `parityDrift`→`continuityDrop`, `ratio` deduped, 3 dead functions |
| `7625d7de` | the blanket `FromHttpApiData` orphan replaced by narrow instances |
| `5a6315f3` | `WrappedEnumInt` no longer decodes a bad int to `minBound` |
| `395641af` | two specs that assert nothing about production code |
| `9885b7d1` | four unused `Pkg.TestUtils` helpers |
| `55d4863d` | one byte formatter instead of three that disagreed |
| `97fa8b79` | severity badge and empty state rendered two ways |
| `776c36cc` | one transient-retry loop; `Concurrent` dropped from ~12 signatures |
| `6021f9b6` | spec fix the library watcher could not catch |
| `530d7137` | duplicate assertion + second copy of `jsonToMap` |
| `73f726f9` | one TimeFusion top-projects query instead of three |
| `099eeb19` | seven module-private helpers un-exported |

**Net −250 lines** of code (`src` + `test` + `web-components`: 217 added, 467 deleted).
The notes file itself is the remaining +386.

That is a real but modest reduction against a goal of "drastically reduce code size", and
it is worth being plain about why. Most of the night went on things that had to happen
first: this checkout had never been compiled, so the first green build was a from-scratch
dependency build; the watcher then reported green on a broken tree three different ways,
each of which had to be found and fixed before any verdict could be trusted; and I rejected
the single largest proposed deletion on merit (see below). The durable output is as much
the survey map and the verification harness as the diff.

Two things improved that don't show up as lines: both correctness bugs are fixed, and the
tree now compiles **zero-warning** where it started with two `-Werror`-class redundant
imports in `Pages/Charts/Charts.hs`.

### The blanket-orphan investigation, since the survey's one-line fix was wrong

The survey said the instance was unneeded and `JSONHttpApiData` should replace it at
route captures. Removing it produced a cascade — `[Query]`, then `[Text]`, then
`WidgetType`, `SummarizeBy`, `Layout`, `AE.Value`, `Charts.MetricsStats`, and finally
`V.Vector Projects.UserId` over in `Pages/Projects.hs`. So it *was* load-bearing, just
not in the shape it was written: `FromForm` derivation needs container instances plus
per-type ones, never a blanket. Final shape: `[a]`, `V.Vector a` and `AE.Value` in
`Utils` beside `JSONHttpApiData`; the four record types derive via `JSONHttpApiData`;
the two enums derive via `WrappedEnumSC`.

That last part is a behaviour **fix**, not just a refactor: the old instance ran the raw
param through `AE.eitherDecodeStrict`, so `widget_type=top_list` — what a form actually
submits — never parsed; only `widget_type="top_list"` did.

### `Models.Apis.LogQueries` → `Models.Telemetry.Telemetry` is a cycle

Worth knowing before anyone tries the obvious consolidation again:
`LogQueries → Telemetry → Models.Apis.ErrorPatterns → LogQueries`. GHCi answers with
`Module graph contains a cycle` / `Failed, unloaded all modules`. Shared helpers between
those two have to go **down** the graph — `Data.Effectful.Hasql` is the natural floor for
anything Hasql-shaped, and both already import it.

### Three ways ghcid reports green on a tree that does not compile

All three were hit tonight, and each one nearly produced a bad commit:

1. **Stale title.** Grepping the log tail for a success word matches the *previous*
   reload's status line. Read only the last one.
2. **In-flight reload.** ghcid prints the changed-file list, then goes quiet while
   compiling. During that silence the previous verdict is still the last thing in the
   log. Only read output produced *after* the final `Reloading...`.
3. **Refused module graph.** On an import cycle ghcid prints `Failed, unloaded all
   modules` and then, cheerfully, `All good (56 modules)` for the partial load — and
   `All good (0 modules)` / `No files loaded` when it loses the session entirely.

`scripts/local/ghcid-wait.sh` now handles all three, and `GHCID_MIN_MODULES=120` guards
the module count. **Any verdict from that script before this was hardened should be
distrusted.** The commits so far were each re-confirmed at the full 128 modules.

Note the log wraps at ~76 columns and splits words mid-token (`[-Wredundant-cons\ntraints`),
so grepping it for a phrase silently fails. Anchor on a short string that fits on one line.

### A build-verification trap worth remembering

`ghcid-wait.sh` first reported green on a broken tree. ghcid rewrites the terminal title
after each reload, and grepping the tail for a success word matched the *previous*
reload's title. It now reads only the last title and treats `N errors, …` as failure.
Any verdict from before that fix should be distrusted — this is exactly the phantom-error
class the "one cabal process" memory warns about, arriving from the opposite direction.

## Progress log

- **22:40** — watcher was dead and `build.log` stale since 12 Aug; restarted via
  `make tmux-live-reload`. It is re-fetching every `source-repository-package`, so the
  first green build is some way off. No edits until it is green.
- **22:47** — deps still building (the watcher is quiet because cabal is compiling the ~56
  `source-repository-package` deps it just re-cloned). Read-only analysis continues.
- Found the first hard evidence of the owner's complaint by name collision alone — helpers
  defined twice in different modules:

  | helper | copies | verdict |
  |---|---|---|
  | `ratio` | `Models/Telemetry/Containers.hs:89`, `Pages/Infrastructure.hs:984` | **identical** bar variable names — merge outright |
  | `humanBytes` | `Models/Telemetry/Telemetry.hs:2685`, `Pages/Settings.hs:1293` | **divergent**: binary-step no-space integer vs decimal one-dp with space. Two byte formatters is the uniformity bug itself; recent commit `c2c2db05` already fixed byte units once |
  | `emptyState_` | `Pages/Components.hs:48`, `Pkg/Components/ServiceMap.hs:285` | ServiceMap shadows the canonical shared component — flagship uniformity violation |
  | `issueTypeBadge` | `Pages/Anomalies.hs:1907`, `Pkg/EmailTemplates.hs:828` | same signature, badge rendered twice |
  | `truncateText` | `Web/MCP.hs:441`, `Pkg/EmailTemplates.hs:586` | same name, different ellipsis — parameterise the suffix |
  | `plainCell` | `Pages/Infrastructure.hs:992`, `Pages/Containers.hs:214` | same signature, different classes |
  | `parseStackTrace`, `nonBlank`, `renderTable`, `renderNameCol`, `servicePicker_`, `fmtDate`, `linkButton`, `toolError` | two each | assess individually |

  First planned commit — fold each into one canonical home. Verdict per pair after
  reading both bodies:

  - `ratio` — identical; keep the `Containers.hs` copy (it carries doctests), delete the
    `Infrastructure.hs` one and import.
  - `nonBlank` — canonical `Text -> Maybe Text` (stripping) in `Utils`; `CodeContext`'s
    `Maybe Text` variant becomes `(>>= nonBlank)`.
  - `fmtDate` — keep `Settings`' `FormatTime t => String -> t -> Text`, move to `Utils`;
    RUM's `Text -> UTCTime -> Text` becomes a call with `toString`.
  - `humanBytes` — one `Integral`-polymorphic formatter in `Utils`. Picks a single spelling
    for byte units across charts, settings and telemetry, which is the actual uniformity
    fix; displayed strings change in one of the two places, which the brief allows.
  - `truncateText` — one helper in `Utils` taking the ellipsis/suffix as an argument.
  - `issueTypeMeta` — the IssueType → (label, colour, icon) mapping is written twice
    (`Anomalies.hs:1938` and inline in `EmailTemplates.hs:828`) with the same labels but
    different colour spellings. Move the mapping next to `IssueType` in
    `Models/Apis/Issues.hs`; keep two *renderers* (email needs inline styles, not Tailwind)
    over one source of truth. This is the drift-prone one.

  Explicitly **not** merged: `linkButton` in `Bots/Utils.hs` vs `Bots/Discord.hs` — same
  name but genuinely different wire formats (Slack blocks vs Discord components).
- Launched five read-only surveys (UI component duplication, type weakness, backend/query
  duplication, client-side tier + locality-of-behavior) to build the cross-file
  consolidation map. The `hs-deep-clean` workflow only fixes within a single file and
  defers everything cross-file, so this map is the complement to it.
