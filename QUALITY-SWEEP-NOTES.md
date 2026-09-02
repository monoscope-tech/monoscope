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
