# Sentry parity — tracked work (2026-09-25)

Gap list: `docs/issues-page-redesign.md` §25. Every item is derived from OpenTelemetry data: the
semantic-convention attribute each one reads is named, so nothing depends on a vendor SDK field.
Status: `[ ]` todo · `[~]` in progress · `[x]` done (commit).

## A. Capture / derive at ingest (`Telemetry.atErrorFrom` → `ErrorPatterns.ATError`)

- [~] A1 Geography — `client.geo.*` / `geo.*` when the SDK sends them; else GeoIP from `client.address`
      (MaxMind GeoLite2 `.mmdb`, path from env, optional).
- [~] A2 Releases — `service.version` (resource). First/last seen release on the pattern, release
      markers on the chart, "resolve in next release".
- [x] A3 Users affected — distinct `enduser.id` / `user.id` / `client.address` per issue.
- [x] A4 Error details — handled = not `exception.escaped`; mechanism = the signal it came from
      (exception event / log record / span status); level = `severity_text` / span status.
- [x] A5 Browser / OS / device on errors — `user_agent.original` (parsed), `browser.*`, `os.*`,
      `device.*`, `host.arch`.
- [~] A6 Threads + attachments — `thread.id`, `thread.name`; attachments/screenshots via span
      events or `*.attachment` attributes.

## B. Issue page

- [x] B1 Context panels — User (geo, IP), Browser, OS, Device, Runtime, Trace details.
- [ ] B2 Tags — event attribute table (All / Custom / Application / Client / Other) + distribution
      drawer backed by a per-issue rollup (a live GROUP BY measured 48s).
- [x] B3 HTTP Request — `http.request.method`, `url.full`/`url.path`, `url.query`,
      `http.request.header.*`, curl view.
- [~] B4 Events — ‹ › stepping, "Recommended" sample, per-issue events table.
- [x] B5 Copy as JSON / Markdown + grouping explanation (which hash, which frames).
- [x] B6 Breadcrumbs — search, sort, copy.
- [x] B7 Triage — editable priority, assignee + resolve on every issue type, archive until
      escalating / for N hours.
- [x] B8 Collaboration — comments in Activity, people viewing, external links (GitHub/Jira/Linear).
- [x] B9 AI in the rail — root cause, plan, code changes rendered on the page.

## C. Issue types (detectors over spans)

- [ ] C1 Performance — N+1 query (`db.query.text` repeated under one parent), slow DB query
      (duration over threshold), inefficient query; span evidence (parent, preceding, repeating,
      duration impact).
- [ ] C2 Frontend — rage click / dead click from browser SDK click events; selector + replay.
- [ ] C3 Uptime — HTTP checks → downtime issues with status code, reason, duration.
- [ ] C4 Cron monitors — check-ins (span/log with `monitor.slug`); missed/failed issues.
- [ ] C5 User feedback — inbox / resolved / spam, AI summary, linked issue.

## D. Issue list

- [ ] D1 Views — category views + saved views.
- [~] D2 Search chips (`is:unresolved`, …) + sort (recommended, events, users, age).
- [x] D3 Columns — Age, Users, inline priority + assignee.
- [~] D4 Bulk actions — resolve, assign, merge, set priority.

## Notes

- `error_data` is written on insert and refreshed only on regression, so every `ATError` field is
  a *first-occurrence* snapshot (first release, first user's geo/client). Anything that must track the
  latest event needs its own column, written under the same 5-minute throttle as `recent_trace_id`.
- OTel has no handled/mechanism attributes: handled = `not exception.escaped` (deprecated but the only
  signal), mechanism = the signal the error came from (exception event / log record / span status).
- A1 so far reads `geo.*` when the SDK or a collector processor sets it; GeoIP from `client.address`
  still needs a provider decision (below).
- A2 done: first/last release columns, "resolve in next release" (`resolved_in_release`; any other
  `service.version` counts as newer, Sentry's non-semver rule). Still open: release markers on the chart.
- A3: `apis.error_pattern_users` + `users_count`, keyed `id:`/`email:`/`ip:` in that order.
- Found while doing A3: span attributes arrive nested, so flat lookups of `client.address`,
  `http.request.method` and `http.route` had silently never matched; `getSpanAttr` now tries both.
- B4: ‹ › stepping (`/issues/:id/step`, `?event=trace@time`) and an All events table done; a distinct
  "Recommended" sample needs a per-event context score we don't store yet.
- D2: sort by most events / most users added; search chips still open.
- D3: Priority and Assignee show in the row; they are edited from the detail header or in bulk.
- D4: bulk resolve, priority and assign done (`BulkAction` gained `choices`); bulk merge still open.
- A6 so far captures `thread.id`/`thread.name`; attachments need a storage decision.

- Latent, pre-existing: `handleRegression` calls `reopenIssue` with no guard against another open issue
  for the same signal, so it can hit `idx_issues_project_target_type_open` (23505) when the newest issue is
  closed and an older one is open. Seen only from contrived test state so far.

## Decisions needed

- GeoIP provider: MaxMind GeoLite2 (license key + attribution) or IPinfo Lite (free, CC-BY-SA).
- Release ordering for "resolve in next release": treat any *different* `service.version` as newer
  (Sentry's default for non-semver), or require semver.
- N+1 detection scope: ingest batches see partial traces; a background pass over completed traces is
  the alternative.
