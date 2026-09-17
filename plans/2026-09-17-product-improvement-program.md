# Product improvement program — 2026-09-17

## Product outcome

Monoscope's primary job is to take an on-call engineer from an alert to
evidence and a safe next action quickly. This program therefore sequences work
by the customer journey, not by subsystem ownership:

```text
telemetry arrives → useful first view → monitor/alert → issue → logs/traces
       → acknowledgement/recovery → trustworthy follow-up and billing
```

The program preserves the product's incident-first, dense operating UI. New
states must be explicit in text as well as color, and a page must disclose a
query failure rather than look like it has no data.

## Current evidence and workstreams

| # | Outcome | Current evidence | Concrete delivery | Completion evidence |
|---|---|---|---|---|
| 1 | Core reads answer promptly | 24-hour container view timed out; sessions took 18.74s; RUM has 31x read amplification. Common Explorer facets already render from the first-response schema summary. A 2026-09-17 production check found 6,160 `/chart_data` requests in seven days with zero 5xx and zero 504 in thirty days; 37 401s are a separate auth/session class. | Finish session tier/client pairing, retain the endpoint timing decision, and investigate a chart failure class only when current telemetry identifies one. | Recorded before/after timings, no unresolved current 504 query class, route regression coverage, and facet browser/handler tests. |
| 2 | Alert to investigation is one path | Alert delivery and issue lifecycle flows exist. The legacy alert-detail route now gives current value, threshold and query, and hands off to the canonical monitor overview; issue detail, logs, traces, acknowledgement, and recovery still do not share one focused workspace. | Reuse the shared details panel for Issues; make alert/issue links carry service, time, environment and trace context; surface acknowledgement and recovery history beside evidence. | One end-to-end integration/browser flow from monitor evaluation through alert, issue, trace/log drill-down, acknowledgement and recovery. |
| 3 | Dashboards are operationally trustworthy | The six dashboard-convergence decisions are implemented and validated: server-side sorting, shared details, trace-table convergence, typed query parameters, endpoint measurement, and CI repair. Long-range endpoint scans remain an explicitly evidence-gated cache/rollup follow-up. | Retain query errors and exact time scope on every refetch; only optimize endpoint scans after equivalence tests and repeated benchmarks justify a complete-result cache or server-side rollup. | Widget sort, panel, parameter, trace and endpoint regression coverage plus measured endpoint decision. |
| 4 | New projects reach first value | The sidebar checklist derives dashboard, monitor and successfully-sent test-notification milestones from live records; a privacy-safe, deduplicated project milestone ledger records the first verified ingest, dashboard, monitor and sent test notification. The onboarding integration journey now proves all four records through the public handlers. | Run the operator funnel query/report and use its conversion evidence to prioritize the next activation improvement. | Activation event funnel; empty/partial/error states remain actionable. |
| 5 | Environment and service scope are dependable | Sticky environment selection scopes Explorer, monitors and RUM. `/chart_data`, its stream, direct widget fetches, and dashboard server-prefill now bind the authenticated session environment to generated KQL and cache identity. Raw SQL dashboard templates, API v1 and a two-environment/two-service fixture remain open. | Finish one scoped-query contract for raw dashboard SQL and API v1; preserve explicit shared-link scope and prove service filtering. | Two-environment/two-service fixture proves every named surface excludes the other scope. |
| 6 | Sessions and replay support investigation | Session/replay UI and panel cache exist; the hour tier is building but its query has unmatched expression aggregates. Session detail preserves replay, identity, landing page and raw telemetry context; browser-error occurrences now also link into that workspace. | Deploy the paired source-named landing-page/user-agent measures, then prove raw-versus-tier equivalence and the production route. | Rollup route test; raw-versus-tier result fixture; session detail navigation and empty/replay-only coverage. |
| 7 | Failure and recovery are visible and trustworthy | Expired-session fetch failures are fixed; `/chart_data` 504/500 are now visible; alert history and browser recovery proof remain incomplete. | Standardize widget/data failure states with retry and preserved query context; add alert-history visibility and recovery semantics; close high-value customer-flow gaps. | Error, retry, expired-auth, acknowledgement and recovery flows verified at the handler/browser boundary. |
| 8 | Billing matches product promises | Usage events are emitted, but attaching metered prices is an external billing-boundary decision; copy must not promise uncharged usage. | Prepare an operator runbook and boundary checklist; once prices are attached, update product/marketing copy and prove provider/webhook reconciliation. | Operator records attachment at a billing boundary; UI copy, subscription state and invoice fixture agree. |

## Sequencing

### Milestone A — query trust (items 1, 6, 7)

1. Complete the RUM session tier pairing in Monoscope and TimeFusion, with
   source-named measures. Treat the deliberate semantic change as a landing
   page in copy and tests.
2. Capture each distinct `/chart_data` 504 query, rank it by cost and traffic,
   and fix the highest-cost shape only after an explain/timing baseline.
3. Implement initial Common facets using the already-read schema summary; keep
   other groups lazy and independently cacheable.
4. Measure endpoint queries at the declared windows and choose a rewrite from
   evidence. Do not materialize unbounded trace IDs.
5. Add a reusable failure/retry contract so a timeout says what failed, retains
   the exact query scope, and provides one safe retry.

### Chart failure decision — 2026-09-17

Read-only production metrics for project `87576849-4941-49d3-a15d-680fef88a1a8`
found 6,160 `/chart_data` requests in the last seven days, with zero 5xx. A
30-day 504-specific count was also zero. The only non-success class in the
seven-day status breakdown was 37 401 responses. Do not change a chart query
shape or create a new rollup on the basis of historical 504s: first capture a
current failure with its query, time range, source and exact status. The 401s
belong to the existing expired-auth/session-recovery investigation, not the
query-performance workstream.

### Milestone B — investigation workspace (items 2, 3, 5)

1. Land the shared details-panel contract and server-side table sorting.
2. Establish `ScopedQuery` at query construction. Migrate one surface at a
   time: Explorer/issues, dashboards/charts, RUM, then monitor evaluation.
3. Link alerts/issues into pre-scoped logs, traces and dashboards. Put current
   state, acknowledgement and recovery history in the same workspace.

### Milestone C — activation and commercial trust (items 4, 8)

1. Build a data-derived activation checklist and the full lifecycle test.
2. Record activation funnel events with privacy-safe project-level aggregates.
3. Run the billing-boundary checklist with the subscription owner. This is not
   an unattended code deployment. Update copy only after the external action.

### Activation funnel measurement

Migration 0186 records only the first successful occurrence of each milestone
per project. It has no user, device, endpoint, or query data. Operators can
measure weekly conversion and elapsed time with this aggregate query:

```sql
WITH cohort AS (
  SELECT id, created_at, date_trunc('week', created_at) AS week
  FROM projects.projects
  WHERE created_at >= now() - interval '12 weeks'
), milestones AS (
  SELECT project_id, milestone, occurred_at
  FROM projects.activation_milestones
)
SELECT
  cohort.week,
  count(*) AS projects_created,
  count(*) FILTER (WHERE ingest.occurred_at IS NOT NULL) AS ingest_verified,
  count(*) FILTER (WHERE dashboard.occurred_at IS NOT NULL) AS dashboards_created,
  count(*) FILTER (WHERE monitor.occurred_at IS NOT NULL) AS monitors_created,
  count(*) FILTER (WHERE notification.occurred_at IS NOT NULL) AS notifications_tested,
  percentile_cont(0.5) WITHIN GROUP (
    ORDER BY extract(epoch FROM notification.occurred_at - cohort.created_at) / 3600
  ) FILTER (WHERE notification.occurred_at IS NOT NULL) AS median_hours_to_notification_test
FROM cohort
LEFT JOIN milestones ingest ON ingest.project_id = cohort.id AND ingest.milestone = 'ingest_verified'
LEFT JOIN milestones dashboard ON dashboard.project_id = cohort.id AND dashboard.milestone = 'dashboard_created'
LEFT JOIN milestones monitor ON monitor.project_id = cohort.id AND monitor.milestone = 'monitor_created'
LEFT JOIN milestones notification ON notification.project_id = cohort.id AND notification.milestone = 'notification_test_sent'
GROUP BY cohort.week
ORDER BY cohort.week;
```

## Implementation rules

- Preserve customer-visible semantics unless this document explicitly names the
  replacement, as it does for the session landing page.
- Query optimizations require a before/after timing and correctness fixture.
- Shared-link context is a product contract: time range, project, environment,
  service and selected evidence must survive every drill-down.
- Use customer-flow tests for lifecycle promises; keep parser and aggregate
  tests focused on dense correctness boundaries.
- Do not report a zero/empty state when a telemetry query failed or timed out.
- Do not attach prices, change live subscriptions, deploy, or backfill data
  without the responsible operator's explicit action.

## Initial implementation record

The first implementation target is Milestone A. The current worktree contains
the RUM landing-page query change and the matching TimeFusion measure/test work;
verify the full route before considering it complete. Existing concurrent
dashboard changes are outside this program unless they are directly needed by a
milestone above.
