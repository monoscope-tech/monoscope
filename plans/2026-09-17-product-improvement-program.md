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
| 1 | Core reads answer promptly | 24-hour container view timed out; sessions took 18.74s; RUM has 31x read amplification; `/chart_data` has recorded 504s. | Finish session tier/client pairing, identify every 504 query from production telemetry, measure endpoint queries at 24h/3d/7d before changing their shape, and render Common Explorer facets in the first response. | Recorded before/after timings, no unresolved 504 query class, route regression coverage, and facet browser/handler tests. |
| 2 | Alert to investigation is one path | Alert delivery and issue lifecycle flows exist, but issue detail, logs, traces, acknowledgement, and recovery do not share one focused workspace. | Reuse the shared details panel for Issues; make alert/issue links carry service, time, environment and trace context; surface acknowledgement and recovery history beside evidence. | One end-to-end integration/browser flow from monitor evaluation through alert, issue, trace/log drill-down, acknowledgement and recovery. |
| 3 | Dashboards are operationally trustworthy | `dashboard-query-and-panel-convergence.md` specifies sorting, details, trace-table convergence, endpoint measurements, typed log query parameters, and CI repair; implementation is in progress. | Complete the six decisions in that plan without client-side sorting after a server limit; retain query errors and exact time scope on every refetch. | Widget sort, panel, parameter, trace and endpoint regression coverage plus measured endpoint decision. |
| 4 | New projects reach first value | Existing tests cover onboarding, first ingest, dashboard, monitor and integration settings, but do not prove the complete activation chain or measure its drop-offs. | Add an activation checklist/state derived from real project data; link each incomplete step to its action; instrument and test ingest → dashboard → monitor → test notification. | One customer-flow test and activation event funnel; empty/partial/error states remain actionable. |
| 5 | Environment and service scope are dependable | Sticky environment selection scopes Explorer and monitor query construction, but dashboards/charts and API v1 remain unscoped; service filters are not centrally proven. | Define one scoped-query contract used by Explorer, issues, dashboards/charts, RUM and monitor evaluation; preserve explicit shared-link scope. | Two-environment/two-service fixture proves every named surface excludes the other scope. |
| 6 | Sessions and replay support investigation | Session/replay UI and panel cache exist; the hour tier is building but its query has unmatched expression aggregates. | Change the RUM session list to an intentionally named landing-page/user-agent representation, declare matching TimeFusion measures with source-column names, and cross-link session, replay, page, error and identity views. | Rollup route test; raw-versus-tier result fixture; session detail navigation and empty/replay-only coverage. |
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
