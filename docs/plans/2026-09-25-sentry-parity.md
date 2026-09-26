# Sentry parity — tracked work (2026-09-25)

Gap list: `docs/issues-page-redesign.md` §25. Every item is derived from OpenTelemetry data: the
semantic-convention attribute each one reads is named, so nothing depends on a vendor SDK field.
Status: `[ ]` todo · `[~]` in progress · `[x]` done (commit).

## A. Capture / derive at ingest (`Telemetry.atErrorFrom` → `ErrorPatterns.ATError`)

- [x] A1 Geography — `client.geo.*` / `geo.*` when the SDK sends them; else GeoIP from `client.address`
      (MaxMind GeoLite2 `.mmdb`, path from env, optional).
- [x] A2 Releases — `service.version` (resource). First/last seen release on the pattern, release
      markers on the chart, "resolve in next release".
- [x] A3 Users affected — distinct `enduser.id` / `user.id` / `client.address` per issue.
- [x] A4 Error details — handled = not `exception.escaped`; mechanism = the signal it came from
      (exception event / log record / span status); level = `severity_text` / span status.
- [x] A5 Browser / OS / device on errors — `user_agent.original` (parsed), `browser.*`, `os.*`,
      `device.*`, `host.arch`.
- [x] A6 Threads + attachments — `thread.id`, `thread.name`; attachments/screenshots via span
      events or `*.attachment` attributes.

## B. Issue page

- [x] B1 Context panels — User (geo, IP), Browser, OS, Device, Runtime, Trace details.
- [x] B2 Tags — event attribute table (All / Custom / Application / Client / Other) + distribution
      drawer backed by a per-issue rollup (a live GROUP BY measured 48s).
- [x] B3 HTTP Request — `http.request.method`, `url.full`/`url.path`, `url.query`,
      `http.request.header.*`, curl view.
- [x] B4 Events — ‹ › stepping, "Recommended" sample, per-issue events table.
- [x] B5 Copy as JSON / Markdown + grouping explanation (which hash, which frames).
- [x] B6 Breadcrumbs — search, sort, copy.
- [x] B7 Triage — editable priority, assignee + resolve on every issue type, archive until
      escalating / for N hours.
- [x] B8 Collaboration — comments in Activity, people viewing, external links (GitHub/Jira/Linear).
- [x] B9 AI in the rail — root cause, plan, code changes rendered on the page.

## C. Issue types (detectors over spans)

- [x] C1 Performance — N+1 query (`db.query.text` repeated under one parent), slow DB query
      (duration over threshold), inefficient query; span evidence (parent, preceding, repeating,
      duration impact).
- [x] C2 Frontend — rage click / dead click from browser SDK click events; selector + replay.
- [x] C3 Uptime — HTTP checks → downtime issues with status code, reason, duration.
- [x] C4 Cron monitors — check-ins (span/log with `monitor.slug`); missed/failed issues.
- [x] C5 User feedback — `user.feedback` spans/logs (`feedback.message`, `feedback.contact_email`, `user.*`,
      `url.full`) become Feedback issues linked to the error sharing their trace; Inbox / Archived (resolved) /
      Spam tabs, AI summary via IssueEnhancement.

## D. Issue list

- [x] D1 Views — category views + saved views.
- [x] D2 Search chips (`is:unresolved`, …) + sort (recommended, events, users, age).
- [x] D3 Columns — Age, Users, inline priority + assignee.
- [x] D4 Bulk actions — resolve, assign, merge, set priority.

## Notes

- `error_data` is written on insert and refreshed only on regression, so every `ATError` field is
  a *first-occurrence* snapshot (first release, first user's geo/client). Anything that must track the
  latest event needs its own column, written under the same 5-minute throttle as `recent_trace_id`.
- OTel has no handled/mechanism attributes: handled = `not exception.escaped` (deprecated but the only
  signal), mechanism = the signal the error came from (exception event / log record / span status).
- A1: `geo.*` attributes first; otherwise `client.address` is looked up in `GEOIP_DB_PATH` (a MaxMind-
  format `.mmdb`; `geoip2` package). Both MaxMind and IPinfo layouts are read, so the IPinfo city
  database drops in for today's free country one. Only the field parsing is tested (doctest).
- A2 done: first/last release columns, "resolve in next release" (`resolved_in_release`; any other
  `service.version` counts as newer, Sentry's non-semver rule). Release markers: the shared Widget takes
  `markers` (label + instant, dotted x-axis lines); the issue chart marks the first release at first seen and
  the last release at `last_release_since` (migration 0207; `last_release_at` keeps ordering late batches).
- A3: `apis.error_pattern_users` + `users_count`, keyed `id:`/`email:`/`ip:` in that order.
- Found while doing A3: span attributes arrive nested, so flat lookups of `client.address`,
  `http.request.method` and `http.route` had silently never matched; `getSpanAttr` now tries both.
- B4: ‹ › stepping (`/issues/:id/step`, `?event=trace@time`), an All events table, and "Recommended"
  (`step?dir=recommended`): the last day's event ranked replay session > user > URL, newest first,
  scored at read time from flattened columns rather than a stored score.
- D2: sort by most events / most users; active filters render as removable chips in the shared Table.
- D3: Priority and Assignee show in the row; they are edited from the detail header or in bulk.
- D4: bulk resolve, priority, assign (`BulkAction` gained `choices`) and merge: the oldest selected error
  is canonical, the rest join its group with `merge_override` (so the model's passes leave it alone) and
  their issues are archived with an `IEMerged` activity. Only runtime exceptions merge.
- A6: `thread.id`/`thread.name`, and attachments as http(s) URLs in `attachment.url` / `screenshot.url`
  (string or string[]; OTel has no convention). The SDK hosts the files — we store and render links, images
  inline. Uploading files to our own storage is a separate decision if SDKs can't host them.

- Latent, pre-existing: `handleRegression` calls `reopenIssue` with no guard against another open issue
  for the same signal, so it can hit `idx_issues_project_target_type_open` (23505) when the newest issue is
  closed and an older one is open. Seen only from contrived test state so far.

- C1: hourly `PerformanceIssueDetection` job over the last hour's db spans (TF when enabled):
  N+1 = same normalised query ≥5× under one parent in one trace, ≥50ms total; slow = one query >1s.
  Queries group by literals replaced with `?`. Only the Postgres path is exercised locally; the
  `regexp_replace(..., 'g')` form is DataFusion-compatible but unverified against TimeFusion.

- C2: rage clicks from click spans (3+ on one element in one session within 2s); dead clicks from
  `dead_click` spans the browser SDK now emits (monoscope-web branch `dead-click-spans`, commit a5e0494:
  a button/link click with no DOM mutation or navigation within 7s), 2+ per element per hour.

- C3: uptime checks are scrape targets of kind `uptime` (migration 0204) sharing the lease/dispatch;
  managed at /monitors/uptime; two failed probes open an `uptime` issue, the next success resolves it.
  No per-probe history yet (Sentry's uptime bar): only the last status and the issue carry it.

- C4: `apis.cron_monitors` (migration 0205), managed at /monitors/cron; check-ins are spans/logs named
  `cron.checkin` with `monitor.slug` + `monitor.status`; evaluated on the per-minute tick under a
  SKIP LOCKED lease. Interval schedules only (no cron expressions yet).

- B2: `apis.error_tag_counts` rollup (migration 0206) over a fixed tag set, written with the users
  rollup; the page shows top values per tag. The per-event attribute table is the Trace section's span
  details plus Highlights/Contexts rather than a separate All/Custom/Application/Client table.

## Decisions (asked 2026-09-26)

- GeoIP: IPinfo Lite free (country) first; the reader also takes city/region fields so the licensed city `.mmdb` drops in later. Env-configured path; absent = no lookup.
- Uptime (C3): HTTP check monitors (URL, interval, expected status) that open downtime issues.
- Cron (C4): in scope — check-ins as OTel spans/logs carrying a monitor slug.
- User feedback (C5): first left out of scope, then built anyway on 2026-09-26 as its own commit so it can be dropped.
- Release ordering: any different `service.version` counts as newer (Sentry's non-semver rule).
- N+1 detection: background pass over completed traces, not ingest batches.

- C5: hourly `FeedbackDetection` over the last 65 min; keyed `feedback:<record id>` and skipped when that
  issue exists in any state, so the overlap never duplicates. Spam is `apis.issues.spam_at` (migration 0208):
  it archives, and every other tab excludes it. Only the Postgres path is exercised locally.
