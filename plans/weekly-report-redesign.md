# Weekly system report: research, design, and delivery

Status: implementation and PR validation complete; merged into production master. Deployment succeeded. Storage metadata has recovered, but the public live-report route still exceeds the gateway timeout; production iteration continues. Earlier entries below are a chronological iteration record.

## Objective and audience

Improve the emails Monoscope sends, with the weekly report as the main deliverable. An engineer or engineering lead should understand their systems, services, infrastructure, and issues from the email itself, then follow a precise link to investigate. Preserve Monoscope's calm, dense, evidence-led identity. The report is a Read surface with Operate-style comparisons and actions.

Success requires real data, readable email components, preserved information on mobile, a saved report that agrees with the email, meaningful verification, production deployment, and a post-deployment review. A prettier two-number digest is not the requested result.

## Research: competitors and prior art

Primary sources inspected on 2026-09-06. Distinguish documented capabilities from design conclusions. Public samples are evidence of composition, not proof of every current customer email.

| Source | Observed approach | Useful lesson for Monoscope | Limitation relative to this brief |
| --- | --- | --- | --- |
| [New Relic performance reports](https://docs.newrelic.com/docs/apm/reports/performance-reports/) | Weekly email features an application, summarizes the 20 highest-call-count apps, and lists recent alerts/deployments/events. Documentation publishes the metric queries and reporting boundaries. | Service breadth and reproducible metric definitions belong in the report. Put workload and performance together. | A featured app can consume attention that should instead go to a smaller, failing service. |
| [Scout notification docs and digest sample](https://scoutapm.com/docs/features/notifications) | Compares web and background-job performance with the previous week; calls out slow outliers. Separate insights email gathers N+1, slow-query, memory-bloat findings and up to five slow traces. The inspected sample opens with a concise change summary, then pairs actual values with changes and linked evidence. | Begin with a factual briefing; put regressions and useful next actions near the relevant service. Absolute values are needed alongside deltas. | Avoid copying its large featured-app treatment, faded low-contrast changes, or implying that a coincident deploy caused a regression. |
| [Scout customer workflow](https://www.scoutapm.com/blog/backerkit-datadog-to-scout) | Customer uses the weekly digest to inform a recurring engineering performance review. | Design for forwarding and team review as well as individual inbox scanning. | One case study supports a use case, not a universal user preference. |
| [Sentry weekly email source](https://github.com/getsentry/sentry/blob/master/src/sentry/templates/sentry/emails/reports/body.html) | Source contains issue/error trend charts, per-project error/new/escalating/regression counts, issue-type distribution, and linked issue detail. Uses email tables for chart structure. | Separate occurrences from issue groups, and new problems from an existing backlog. Small charts can be secondary to labels and counts. | Error tracking alone cannot establish infrastructure health or service availability. Source review does not constitute an inbox-client visual test. |
| [Datadog scheduled reports](https://docs.datadoghq.com/dashboards/sharing/scheduled_reports/) | Scheduled dashboard reports use high-density portrait PDFs, delivered by attachment or link; dashboard-specific schedules and recipients are supported. Some widgets are unsupported. | Dense reporting is established prior art. Coverage and unsupported sections must be explicit. | A PDF export requires an extra opening step; it is not an email-native system briefing. |
| [Grafana reporting](https://grafana.com/docs/grafana/latest/visualizations/dashboards/create-reports/) | Scheduled reports can deliver dashboard PDFs, CSVs, table data, and embedded images. Rendering and attachment-size constraints are documented. | Pair an inbox summary with deeper saved evidence; snapshot identity and rendering reliability matter. | Dashboard composition is not necessarily a good email reading order. |
| [UptimeRobot monthly reports](https://uptimerobot.com/monitoring-for-support/) | Reports include incidents, overall uptime, total downtime, longest incident, and monitor status. | Reliability reporting needs incident/monitor context, not just event volume. | Do not invent uptime from an absence of Monoscope errors: uptime requires the right observation history. |
| [Mailchimp clipping guidance](https://mailchimp.com/help/gmail-is-clipping-my-email/) | Documents Gmail clipping above 102KB of message content. | More information needs compact markup, bounded ranked lists, and clear links to the full saved report. Aim below 80KB for the HTML fixture to leave room for delivery overhead. | A passing browser screenshot does not prove deliverability or immunity to provider-added markup. |
| [Mailchimp email CSS guidance](https://templates.mailchimp.com/development/css/client-specific-styles/) | Documents Outlook table spacing, font/image differences, and text-size adjustment. | Use inline essential styles, presentation tables, system fonts, explicit labels, and a useful image-blocked view. | Avoid web-only interactions or relying on modern layout support in mail clients. |

Research conclusion: the opportunity is **cross-signal completeness in the email**, not an unsubstantiated claim of unmatched density. Compose a system briefing with a service ledger, infrastructure observations, issue priorities, and contextual drill-downs. Use charts to explain changes rather than displace the useful content.

Research artifacts (local, untracked): `/tmp/monoscope-report-research/scout-digest.png`, `sentry-weekly.html`, `current-weekly.html`, `current-desktop.png`, `current-mobile.png`.

## Baseline review and data audit

| Severity | Location | Before | Required after | Why |
| --- | --- | --- | --- | --- |
| HIGH | `src/Pkg/EmailTemplates.hs`, mobile `.report-table` rule | All columns after the first disappear below 600px. Confirmed in the 375px preview: no endpoint latency/change, query latency/count, or log-pattern count. | Preserve every metric in labeled stacked rows or compact readable columns. | Adaptivity must preserve the content needed to decide. |
| HIGH | `src/BackgroundJobs.hs:sendReportForProject`, `src/Pages/Reports.hs:buildLiveReportEmailHtml` | Weekly window is six days; preview uses a matching 12-day comparison span. | Seven complete days and an equal preceding period, with explicit bounds and timezone labels. | Accurate comparison is part of report trust. |
| HIGH | `src/Pkg/EmailTemplates.hs:weeklyReportEmail` slow-query rows | Renderer destructures `(statement,total,latency)`; query and saved JSON use `(statement,duration,count)`. Query durations are in nanoseconds, while UI appends ms. | Typed fields and one explicit nanoseconds-to-milliseconds conversion. Assert distinct count/duration fixture values. | A visually convincing wrong number is worse than a missing metric. |
| MEDIUM | `src/Pages/Reports.hs:renderWeeklyEmail` and background report collection | No service or infrastructure section. Issue counts derive from a limited list, and issue filter selects creation time, not all active problems. | Separate full aggregates from bounded detail lists; expose cross-service status and clearly scoped infrastructure/monitor observations. | A ranked sample must not masquerade as total system status. |
| MEDIUM | `src/Models/Telemetry/Telemetry.hs` report queries | Stats drop unnamed services; adjacent queries use inconsistent inclusive/strict boundaries. Endpoint averages combine both client/server HTTP spans. | Explicit, consistent predicates; label what is counted; preserve unnamed telemetry and distinguish request metrics from all events. | Denominators and double-counting affect every conclusion. |
| MEDIUM | `src/Pages/Reports.hs:ReportData` | Saved report omits live log-pattern extras; saved and live email can differ. | Persist the additional report snapshot; old reports decode without inventing historic data. | The full-report link must explain the email the user received. |
| MEDIUM | Weekly layout | Greeting and two large charts precede issue detail; repeated bordered tables have weak hierarchy; numeric cells wrap awkwardly. | Briefing first, comparisons second, service/infra/issue detail next, trends and long-tail evidence below. | Shared edges and deliberate density shorten scanning. |
| MEDIUM | Shared email styles | Table `!important` foreground overrides can erase change colors; dark theme uses broad overrides. | Semantic classes with paired surfaces and text, with words/signs carrying the meaning too. | Color must remain meaningful in both appearances. |

Baseline sample HTML: 23,993 bytes. Baseline screenshots captured at 1000px and 375px. These are browser previews, not Outlook/Gmail certification.

## Brainstorm, iteration 1

Three viable compositions:

1. **Dashboard in an email:** compact metric tiles and charts, then many tables. Broad coverage, but spends the opening screen on anonymous totals and repeats the dashboard problem.
2. **Incident briefing:** attention list first, grouped evidence beneath. Strong for a bad week, but a quiet week can feel empty and system coverage remains unclear.
3. **System ledger with an editorial opening:** short evidence-derived briefing, compact totals, then consistent sections for services, infrastructure, issues, performance, and telemetry coverage. Supports a busy week, a quiet week, and incomplete instrumentation.

Choose option 3, borrowing option 2's priority ordering. No AI-generated health claim is necessary: derive the opening statements from measured counts and comparisons. Keep normal services visible alongside regressions, and explain missing signals.

## Iteration 2: concrete report structure

| Reading order | Content | Layout / behavior |
| --- | --- | --- |
| 1 | Monoscope, project, report period, timezone, “View full report” | Compact masthead; metadata shares an edge with the content. Useful preheader, no large greeting. |
| 2 | “This week” briefing: observed errors, services needing review, critical/unacknowledged issues; explicit coverage caveat when necessary | Up to three short factual lines. Link each actionable finding to its evidence. Avoid an unsupported global “healthy” verdict. |
| 3 | Activity and reliability summary | Events, error events and ratio, observed services, issue activity; absolute values and previous-period context. Traffic direction stays neutral. Missing baseline says “No previous data”. |
| 4 | Services | Ranked service rows with name/environment, events, error count/rate, server request count and latency where available, previous-period comparison. Unnamed service is explicit. Full totals independent of email row cap. |
| 5 | Infrastructure | Observed hosts/containers/pods; CPU, memory, storage and readiness when available. Call out the observation window. Latest-at-period-end snapshot must not be labeled weekly peak/average; restart counters must not be described as weekly incidents. No metrics means “No infrastructure metrics observed”, with instrumentation link. |
| 6 | Issues and monitors | Full issue counts by relevant state/category, prioritized linked issues with service/severity/impact where available. Monitor status is explicitly “at report generation”; distinguish alerting, warning, normal, paused, and never evaluated. |
| 7 | Performance and dependencies | HTTP endpoint volume, latency and change; slow database operations with correct units/counts. Match rank to label. Show service/host context, not SQL alone. |
| 8 | Trends and telemetry composition | Compact event/error trends, span-type breakdown, top patterns scoped to the period or explicitly labeled otherwise. No critical evidence exists only in an image. |
| 9 | Coverage and next actions | Available/missing signals and any partial query failures. Persistent full-report link and report notification settings. |

Density target: approximately 680–720px maximum report width, 14px body text, 12–13px secondary labels, compact row rhythm, 24–28px section gaps. Actual breakpoint depends on the service row fitting, not a device name. On 320–390px screens, identifiers wrap and metric groups stack; labels and values remain visible. Existing transactional emails retain their individual purpose while benefiting from shared shell/contrast/accessibility fixes.

## Data and implementation contract

- Use a typed report snapshot rather than adding more positional tuples. Introduce explicitly named service, infrastructure, monitor, issue, and coverage records where the report needs them.
- Reuse metric identity/normalization from `Models.Telemetry.Containers`; do not infer host CPU from an arbitrary container. Inspect `freshnessWindow` and snapshot query semantics before reuse.
- Use TimeFusion for telemetry reads in accordance with the configured backend and existing dialect patterns. Do not add a new dependency on the legacy span store alone.
- Use `[start,end)` boundaries for new report queries. Seven-day current/previous windows must not overlap or leave a missing day.
- Preserve actual aggregates over the selected period before selecting top rows. Issue groups, error events, server requests, and all telemetry events are different metrics.
- Report service error ratios only with the matching denominator. Requests and request latency use server spans; avoid averaging per-service averages or percentiles together.
- Infrastructure and monitor snapshots have different temporal meaning from weekly aggregates. Persist their timestamp/window and clearly label it. If historical data cannot be reconstructed, say so.
- Persist additional data in `ReportData` with a backward-compatible optional field. A stored older report does not query today's infrastructure and pretend it was present that week.
- Saved report and outgoing email use the same snapshot and renderer. Gather data once per project, not once per recipient.
- A failed optional source yields an explicit unavailable section and structured logging, not zeros. Core report failure must not send a healthy-looking report.
- Keep customer data escaped. Bound long statements, names, and row counts without hiding omitted totals. Do not add private telemetry to third-party chart URLs.
- Keep existing recipient preferences. Do not manually send sample emails to customers as a test; use preview and captured notification tests.

## Verification and completion checklist

- [x] Inspect primary competitor documentation and at least one visual email sample.
- [x] Inspect current code and desktop/mobile baseline; record confirmed defects.
- [x] Brainstorm alternatives and refine an implementation direction in this Markdown file.
- [x] Complete query/schema audit and implement richer persisted report data.
- [x] Correct seven-day windows, interval boundaries, units, counts, and missing baselines.
- [x] Implement reusable email-safe components and the new report hierarchy.
- [x] Improve shared email layout defects that affect other user emails, then inspect representative alert/invite/report previews.
- [x] Verify populated, quiet, missing-instrumentation, partial-source, legacy-saved, long-text, and many-service cases.
- [x] Integration tests prove isolation, period boundaries, service aggregates, infra/monitor semantics, saved-email agreement, and delivery preferences without sending real email.
- [x] Render a batched review at desktop/mobile and light/dark, with images blocked and essential inline-style fallback; fix confirmed defects in one batch, then confirm once.
- [x] Check semantic reading order, all mobile metrics, contrast, links, no horizontal clipping, no scripts, and HTML byte size.
- [x] Run repository-required compile/test/CI gates; record exact evidence.
- [x] Deploy to production master while preserving concurrent work.
- [ ] Verify deployed report preview and saved report behavior; record post-deployment findings and resolve material defects.

## Implementation / review log

Research pass: discovered that the current mobile stylesheet removes the metrics, weekly windows are six days, slow-query values are positional and reversed in the renderer, and saved/live content differs. These findings change the work from a template-only redesign to a report-data and rendering change. Implementation and deployment remain outstanding.

### First implementation and evidence

- Added `Models.Telemetry.Report`: persisted service comparisons; aggregate issue counts plus priorities; monitor observations; infrastructure snapshot summaries. Service aggregates use the configured telemetry backend and `[start,end)` bounds. Infrastructure reuses the existing normalizer with an uncapped report query; priority rows are bounded after counting.
- Added optional `systemSnapshot` to saved report JSON and passed it through preview, saved rendering, and the scheduled report path. Both weekly windows now use seven days.
- Replaced the old weekly table/chart stack with a 720px system report: briefing, services, infrastructure, issues, monitors, HTTP/database performance, patterns, trends, and coverage. Essential metrics have labels and remain visible on mobile. Removed obsolete weekly-only helpers and fixed the slow-query tuple order and nanosecond conversion.
- Shared shell changes: explicit light/dark backgrounds, higher-contrast footer text/links, Outlook table spacing resets, and scalable text.
- `TEST_MATCH=Pages.Reports make live-test-dev` passed **3 examples, 0 failures**. Evidence: `/tmp/weekly-report-tests.log`. Tests currently cover report notification lifecycle/isolation, a service period boundary fixture with unnamed telemetry and separate server metrics, and basic template content/units. This is not yet sufficient for the full completion checklist.
- Populated preview: **50,509 HTML bytes**, 54 metric cells. At 1000px light, 375px light, and 375px dark: zero hidden metric cells, document width equals viewport width. Artifacts: `/tmp/monoscope-report-research/iteration-1-{1000-light,375-light,375-dark}.png`. Desktop/mobile composition inspected. These checks used the new server at `http://[::1]:8080`; a separate older server listens on IPv4, so `127.0.0.1` is not a valid verification target for this work.
- Live report preview request on the new server timed out after **90 seconds**. The pipeline still runs the old Postgres-only report scans before collecting the new snapshot. This must be fixed before deployment. New backend-aware `endpointStats`, `databaseStats`, and `workloadStats` query helpers are started in `Models.Telemetry.Report` but not yet wired into collection.

### Required next iteration (not optional polish)

1. Consolidate report collection: remove duplicate service scans, use the configured telemetry backend for endpoint/database/workload reads, and stop scanning old chart datasets that are stored but never consumed by the renderer. Do not silently replace query failures with empty data.
2. Check free-tier semantics: existing code compares a seven-day total to a hard-coded 5,000 threshold, while the current daily-limit constant is 10,000. Replace this with a correctly scoped, persisted observation of the configured daily cap; avoid a false “incomplete” claim.
3. Distinguish missing endpoint baselines from a fabricated 100% change. Make persisted totals agree with the new service aggregates, including unnamed services and error logs. Align chart error predicates with the headline metric definition.
4. Make the full-report view actually expose omitted service rows; right now it uses the same 12-row presentation limit as email. Preserve exact report time/environment in drill-downs. Infrastructure links need the snapshot window and host/container destination. Check iframe link navigation.
5. Add storage usage, workload/telemetry composition, and deterministic attention findings (critical issues, alerting monitors, unready resources). Improve semantic priority styling; the first pass is too visually neutral for an unhealthy system. Fix dark masthead logo visibility. Avoid repetitive caveats displacing useful evidence.
6. Exercise actual query paths, complete the populated/quiet/partial/legacy/large fixtures, verify snapshot persistence and no real sends, and test representative non-report emails. Keep the HTML bounded below clipping size with worst-case content.
7. Confirm the final visual batch, compile/tests, CI, production deployment, and live verification. Nothing from this email redesign has been deployed yet.

Concurrent work is present in the shared checkout (parser, monitoring tests, and monitor evaluation changes in `BackgroundJobs.hs`). Preserve it and stage only this task's changes. `hpack` also picked up the pre-existing `0146_rum_panel_cache.sql` manifest entry; do not attribute it to this report work. Local watchers currently contend over build artifacts; `build.log` showed a missing in-place package database after an automatic restart, so the prescribed `make tmux-live-reload` restart was invoked. Revalidate the actual running server and test handles before continuing.

### Consolidation and stress verification (6 September, continued)

- New snapshots now own endpoint, database, and workload data as named records. Removed the legacy scan path and unused chart datasets from new report generation; old saved JSON still has its explicit decoder. A single collection feeds saved reports and captured weekly emails. Daily saved reports now use a daily title.
- Corrected drill-downs: service/environment and server HTTP context, monitor overview routes, host/container destinations and snapshot windows. Long SQL remains escaped and display-bounded; statements too large for an email URL open the scoped slow-operation search. Exception/error-log chart predicates are checked with the actual KQL parser; its unsupported `tolower` form was replaced with a case-insensitive regex.
- Live preview against the configured TimeFusion backend completed in **58.63 seconds**, returning **33 service comparisons, 489 labeled metric cells, and no unavailable sections**. Captured evidence is `/tmp/monoscope-report-research/live-current.html` and extracted `live-email.html`. Full report HTML was 379,608 bytes; this is the uncapped browser view, not the email. Individual direct current-period service/database queries took about 30 seconds; endpoint/workload direct observations exceeded 30 seconds. Generation latency remains a limitation to evaluate before shipping.
- Email stress fixture includes 30 services, 105 infrastructure resources, 12 monitors, 20 issues, 30 endpoints, seven workload kinds, long patterns, and a deliberately huge HTML-like SQL statement. It initially reached 107,665 bytes. Compact metric markup, a shared workload table, and four prioritized infrastructure rows brought it below the **80,000-byte test limit**, without removing the underlying snapshot or full-report rows. Email retains eight service comparisons; omitted detail is linked and counted.
- Six integration examples pass in **18.45 seconds**: captured delivery preferences/daily naming/isolation; period boundaries and missing baselines; HTTP/database/workload query units; snapshot JSON round-trip and stress limits; infrastructure totals beyond 500 resources and monitor states; template content and chart-query parsing. No customer test email was sent.
- Image-blocked confirmation found a genuine fixed-width image/table overflow. Shared shell tables and chart images now constrain their width inline. The final batch passed at **1000px light, 375px light, 320px dark, and 375px with stylesheet removed**: no document overflow, no hidden metric cells, no scripts. Report sample is approximately **45 KB**, with 55 metric cells plus the semantic workload table. Invite and runtime-error previews also fit at 375px with images blocked. Screenshots: `final-*.png` under the research artifact directory. These are browser compatibility checks, not an Outlook/Gmail rendering certification.
- Development server recovery removed stale orphan listeners using the prescribed Makefile workflow. The current verified new server is now **IPv4 `127.0.0.1:8080`** (the earlier IPv6-only advice is stale). Verify served markup before future captures.

Still required: legacy saved-report regression; explicit issue priority/state fixture; final Haskell review and required gates; reconcile latest master in an isolated deployment worktree; production deployment and post-deploy preview/saved-report checks. The email work is not yet committed or deployed.

### Review and release preparation (7 September)

- Draft PR: https://github.com/monoscope-tech/monoscope/pull/509. The deployment worktree is `/tmp/monoscope-weekly-report-deploy`, based on master `90b23980d`. It excludes unrelated local work.
- Seven integration examples now pass (36.72 seconds in the last run), including an old saved JSON report with corrected daily title and SQL duration, and issue lifecycle/critical-priority ordering. Captured email tests and persisted snapshot round-trips are part of that suite.
- Review corrections: use the existing KQL quoting helper for names, paths, hosts, and statements; pass the actual stored/scheduled report type through rendering instead of inferring daily/weekly from duration; preserve missing-environment and endpoint-host context; reuse the shared decimal formatter.
- HLint 3.10, matching CI, reports **No hints** after corrections. The system-installed 3.3.6 cannot parse this repository's `MultilineStrings` extension; the matching binary lives under `/tmp/weekly-report-hlint-tool` and does not replace the system tool.
- Contrast spot checks of the actual report palette against its backgrounds: body 14.65:1, muted text 6.39:1, links 5.07:1, attention 6.57:1; dark links 8.63:1, attention 9.14:1, footer 6.92:1.
- The pre-deploy production `/reports/live` route returned its old report successfully in 0.35 seconds (35,111-byte iframe document). This is not a like-for-like performance benchmark: the new report collects considerably more data through the configured backend. Verify generation timing and availability in production after deployment.
- CI full build/tests and deployment remain pending. Flattening the concurrent collector remains an optional readability refactor. Infrastructure, issues, and slow database details intentionally remain priority samples in the stored snapshot; their omission links open the relevant explorers, while full reports expand all stored services/endpoints/monitors. Endpoint performance intentionally ranks current traffic rather than enumerating disappeared routes.

The renderer now takes a `ReportEvidence` sum: `SystemEvidence ReportSnapshot` or a named `HistoricalReportEvidence` record. Removed legacy filler vectors, duplicate totals, and unused category/change fields from the shared template record. Historical summaries explicitly say “Not recorded” for service/request metrics they did not retain. Sample pattern evidence now lives inside its snapshot too. This refactor removes more lines than it adds; all seven report examples pass again (32.81 seconds), and HLint reports no hints.

The first full CI run passed build, doctests, unit tests, CLI tests, formatting, and HLint. Its 840-example integration run found one report-test assumption: the shared TimeFusion test backend can already contain telemetry, so the lifecycle test cannot assume zero events. Replaced that assumption with a captured-email versus persisted-snapshot comparison, and retained quiet-state coverage using an explicitly empty, internally consistent snapshot. All seven report examples pass locally again (22.55 seconds). The CI shard reported zero unavailable report sources. Rebased the release branch cleanly onto master `644a2a24a`; final CI and production release remain pending.

### Release validation

- PR #509 merged into master as `0189e4cff2990d4abf32d702f68bd790aff9db70`, preserving the intervening CLI change.
- Final PR CI [34065122168](https://github.com/monoscope-tech/monoscope/actions/runs/34065122168) passed build, unused-code check, doctests, unit tests, CLI tests, integration tests, and end-to-end tests. Frontend, HLint, formatting, and security checks passed; the separate UI test job was skipped by the fingerprint gate.
- Production workflow [34066258808](https://github.com/monoscope-tech/monoscope/actions/runs/34066258808) is running. Deployment and production verification are not yet claimed complete.

### Production verification: material issue found

- Deployment workflow `34066258808` completed successfully, including all merged-tree test suites and CapRover deployment.
- The first immediate live request returned the old layout during rollout. Two fresh requests subsequently returned HTTP 504. Do not treat the green deployment as successful live-report verification.
- Historical saved report returned HTTP 200 in 0.258 seconds and contains the new `system-report` markup and historical evidence label. Artifact: `/tmp/monoscope-report-research/deployed-saved-outer.html`.
- Production event `9c90df9a-1589-5cc2-894e-217da1994815` at `2026-09-06T23:39:57.343198Z` records database-performance collection failing because a referenced Parquet object in the demo project’s 2026-09-06 partition returns S3 `NoSuchKey` / HTTP 404. Event `64cdae94-dc7e-51ea-be64-b35464d15f85` confirms the same missing object on the previous request. Workload cancellation cleanup also reports the pre-existing pgwire `Prepared statement all does not exist` error. Backend recovery and live latency verification remain required.

Read-only storage verification confirms this is current metadata damage: Delta version `528425` still includes the missing path among 24 active demo-project files for 2026-09-06, and a direct S3 HEAD returns 404. Bucket versioning is not enabled and listing versions for the exact key finds no recoverable version. Evidence: `/tmp/weekly-report-storage-check.json`. No metadata or telemetry objects were changed.

### Resumed production review (7 September morning)

- Latest Delta snapshot `534284` no longer references the previously missing object; the earlier recovery blocker has changed. No storage mutation was performed in this task.
- A direct production-origin request returned HTTP 200 in **43.982 seconds**, with 547,174 bytes of outer HTML, the new system report, and no unavailable section text. Artifact: `/tmp/weekly-report-direct-origin.html`.
- Public Playwright requests still returned 504. A separate public curl request confirmed HTTP 504 after **60.261 seconds**. The app gateway config has no report-specific timeout override. The current report service queries scan substantial weekly data (observed 6,720 MB and 4,274 MB file selections); this is a fresh-generation latency issue requiring further work, not a reason to restore the old narrow report.
- Origin visual/chart verification is running separately from the public-route check. Do not use origin success as proof that the normal user-facing route is reliable.

### Live-preview timeout correction

The synchronous live endpoint exceeded the public gateway’s 60-second timeout after storage recovered. The follow-up uses an app-lifetime background task and a shared PostgreSQL preview record keyed by project. Requests authorize as before, atomically claim an expired lease, and immediately return a loading view. HTMX polls only the preview section. Generation retains the complete seven-day snapshot and all full-report sections; successful results are reused for five minutes across replicas. Failures display an explicit retry state, and a six-minute lease permits recovery after worker restart. A generation timestamp prevents an older worker from replacing a newer result. No notifications or saved report history are created by preview generation.

Added regression coverage for simultaneous claim deduplication, immediate polling responses, cached full rendering, project isolation, expired leases, stale-worker completion, and failure state. Compile/test/CI and post-deploy public-route verification remain pending for this follow-up.

### Persisted activity charts

- Preview-only CI run `34101390876` passed the complete build and test workflow at `cbcf0d3e9`.
- Rechecked production chart URLs: telemetry returned a populated PNG in 0.66 seconds, but the error chart returned nginx 504 at 60 seconds through both public and direct-origin paths.
- TimeFusion logs show the error KQL expands embedded span exceptions through JSONPath and selects about 6.7 GB of raw telemetry. A scalar-only SQL variant rendered in 8.16 seconds, but omits embedded exceptions; it was not adopted. A JSONB-cast experiment returned an empty PNG due to an unsupported backend type; it was also rejected.
- The complete exception-aware aggregate succeeds for a one-day production slice in 21.27 seconds (5,564,457 events, 6,649 errors). New snapshots now collect hourly trends in disjoint day slices and combine shared boundary buckets. The entire requested period and embedded exception coverage are retained.
- New chart URLs carry signed, compressed snapshot data. The PNG handler verifies the existing signature, then renders the supplied dataset without a telemetry query. Old snapshots without trends retain their existing chart path; failed trend collection is explicitly labeled unavailable.
- Added regressions for embedded exceptions, slice boundaries, old snapshots without trends, saved chart payloads, and unavailable trends. The application compiles locally; refreshed integration tests, full CI, deployment, and production verification remain required.
