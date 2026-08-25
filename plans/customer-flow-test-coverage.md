# Customer Flow Test Coverage

Last updated: 2026-08-25

## Purpose

This document tracks customer-facing behavior and its integration coverage.
It is a living work log, not a list of every function or route.

The main goal is concise protection against regressions that customers can see.
One workflow test can cover handlers, models, database writes, jobs, queries, and rendered responses.

## Test policy

Use the highest practical entry point for each flow.
Prefer real handlers, a real PostgreSQL database, real migrations, and the real test clock.
When the repository can run the dependency locally, avoid mocks.

An effective workflow test has these properties:

- It starts with a customer action or an ingest request.
- It crosses all important layers that production uses.
- It checks the final customer-visible result and important durable state.
- It includes the setting that controls the behavior.
- When customers can observe a failure or recovery path, it checks that path.
- It does not repeat assertions that the same flow already proves.

When live calls are unsafe or unstable, use golden tests at third-party boundaries.
Keep request and response fixtures small, sanitized, and versioned.

If a short workflow test gives stronger protection, do not add a unit test.
Keep focused unit tests only for dense algorithms, parsers, and boundary conditions.

## Coverage labels

- **Flow**: A test crosses the customer journey and checks its final result.
- **Partial**: Tests cover important parts, but no test proves the complete journey.
- **Contract**: A test checks a route, shape, helper, or model in isolation.
- **External**: The flow needs optional local infrastructure or a recorded boundary.
- **Missing**: No useful automated evidence was found.
- **Browser later**: Haskell cannot prove the important browser behavior.

## Current evidence

The repository has a broad Haskell integration suite under `test/integration`.
The harness uses migrated PostgreSQL state and direct Servant handlers.
Some CLI tests also use a live HTTP server and the real CLI parser.

The strongest existing flow tests include these files:

- `Pages/BusinessFlowsSpec.hs`: onboarding, settings, billing, replay, and object-storage settings.
- `CLI/CLILifecycleSpec.hs`: monitor, dashboard, investigation, search, metric, and event lifecycles.
- `Lifecycle/LogPatternIncidentSpec.hs`: ingest, detect, notify, acknowledge, suppress, and resume.
- `BackgroundJobs/NotificationsSpec.hs`: notification selection, limits, grouping, and acknowledgements.
- `BackgroundJobs/DashboardAutoProvisionSpec.hs`: metric ingest and automatic dashboard provisioning.
- `Opentelemetry/GrpcIngestionSpec.hs`: authenticated OTLP ingest and customer query results.
- `Pages/DashboardWidgetsSpec.hs`: dashboard widget changes and query behavior.
- `Web/ApiV1Spec.hs`: API resource lifecycles and real HTTP authentication checks.

Several large specs also contain helper-level or route-level tests.
Their presence does not prove a complete customer flow.

## Customer flow matrix

### Account, project, and onboarding

| Customer flow | Coverage | Evidence | Gap or next action |
|---|---|---|---|
| Create a project and complete onboarding | Flow | `Pages/BusinessFlowsSpec.hs` uses one project for profile, survey, notification settings, empty integration state, API-key creation, first ingest, and the pricing redirect | Add the final plan-selection action when it can run without a billing-provider stub. |
| Detect the first ingested event during onboarding | Flow | The same lifecycle moves from the empty integration response to a filtered log-explorer result for the first trace | Keep the query assertion at the customer data-handler boundary. |
| Change project details and notification preferences | Flow | `Pages/BusinessFlowsSpec.hs` saves and reloads settings. `BackgroundJobs/NotificationsSpec.hs` proves runtime-error delivery stops and resumes through the same settings handler. `Pages/ReportsSpec.hs` proves daily and weekly suppression. | Extend the lifecycle only when another saved preference controls a distinct job. |
| Delete a project | Flow | Billing tests capture provider cancellation. `Lifecycle/LogPatternIncidentSpec.hs` primes ingest and alert delivery, calls the delete handler, then checks cached-key rejection, report suppression, and alert suppression. | Extend this lifecycle when another scheduled customer action appears. |
| List and switch projects | Contract | `Pages/Projects/ProjectsSpec.hs` checks project lists | Add a two-project isolation flow through customer handlers. |
| Add, update, remove, and restore a member | Flow | `Pages/Projects/ManageMembersSpec.hs` covers the lifecycle | If one flow retains the same evidence, consolidate related examples. |
| Create a team and manage members | Flow | `Pages/Projects/IntegrationsSpec.hs` creates a team with a member and Slack/PagerDuty settings, routes both tests, deletes it, and proves later routing is silent. `ManageMembersSpec.hs` keeps one member lifecycle and one validation table. | Add a new case only for a distinct permission or routing rule. |
| Keep the `@everyone` team synchronized | Flow | `Pages/Projects/EveryoneSyncSpec.hs` covers add, remove, and external addresses | Retain this focused database integration flow. |

### Ingest, logs, traces, metrics, and queries

| Customer flow | Coverage | Evidence | Gap or next action |
|---|---|---|---|
| Create, revoke, and reactivate an API key around ingest | Flow | `Pages/ApiSpec.hs` creates a key through the settings handler, primes it with real OTLP ingest, checks immediate rejection after revoke, and checks immediate acceptance after activation | Keep browser coverage for copying the one-time secret and confirmation dialogs. |
| Ingest logs, traces, and metrics with supported authentication | Flow | `Opentelemetry/GrpcIngestionSpec.hs` covers resource and header authentication | Keep one table-driven authentication flow. Remove duplicate smoke checks only after comparison. |
| Reject invalid or missing ingest credentials | Flow | gRPC integration examples cover each signal | If the HTTP OTLP route uses different middleware, check that route. |
| Search events with filters and pagination | Flow | `Pages/LogExplorer/LogSpec.hs` ingests customer events and checks filters, explicit columns, descendant inclusion, trace deep links, and older/newer pagination without gaps | Consolidate only when the same handler evidence remains explicit. |
| Surface query syntax and backend failures | Flow | `Pages/QueryCacheSpec.hs`, `Pages/LogExplorer/LogSpec.hs`, and CLI tests | Keep failure assertions at the handler boundary. |
| Run aggregate queries for charts and stat widgets | Flow | `Pages/DashboardsSpec.hs` reads one ingested trace as a row table, grouped table, time series, and scalar stat. Metric ingestion and chart queries also run in `GrpcIngestionSpec.hs`. | Keep query-shape assertions together because they guard different decoders. |
| Cache a query without changing its result | Flow | `Pages/QueryCacheSpec.hs` covers hit, partial hit, ordering, and failures | Retain as a focused integration flow. |
| Discover schema and facets after ingest | Flow | `SchemaLearningSpec.hs`, `SchemaLearningPerfSpec.hs`, and `FacetsSpec.hs` | Add the page flow from a new field to a usable query suggestion. |
| Use live tail without Kafka | Flow | `LiveTailSpec.hs` covers ingest-to-browser transport and degradation | Keep transport details behind the customer-level examples. |
| Use live tail with Kafka | External | `LiveTailSpec.hs` covers registration, matching, isolation, delivery, expiry, and backpressure through the built-in PostgreSQL relay. Kafka selects the same matching pipeline but needs a broker transport tier | The repository compose stack has no Kafka broker. Add an opt-in broker tier when local Kafka infrastructure is owned here. |
| View trace details and related spans | Flow | `Pages/TelemetrySpec.hs` ingests a server parent and erroring client child, then checks both names and the error count on the trace page | Retain the focused projection regression. |
| Follow a trace link into filtered explorer events | Flow | `Pages/LogExplorer/LogSpec.hs` ingests a target and unrelated trace, then follows the customer-link `trace_id` query through the data handler | Leave URL clicks and history state in the browser tier. |
| Handle a slow trace query | Flow | `Pages/AnomaliesSpec.hs` keeps the existing timeout regression test | Keep this focused regression when changing trace-query execution. |

### Endpoints, patterns, issues, and anomalies

| Customer flow | Coverage | Evidence | Gap or next action |
|---|---|---|---|
| Discover endpoints from ingested spans | Flow | `EndpointDiscoverySpec.hs` and `ExtractionWorkerSpec.hs` | Add the page result to one ingest and discovery flow. |
| Merge endpoint templates without losing issues | Flow | `EndpointDiscoverySpec.hs` checks grouping, cleanup, and idempotency | Retain this focused integration flow. |
| Create a runtime-error issue from ingest | Flow | `Pages/ErrorPatternsSpec.hs` covers exception ingest, extraction, issue detail, trace drill-down, and project isolation | Retain this ordered lifecycle. |
| Detect a new log pattern and open an incident | Flow | `Lifecycle/LogPatternIncidentSpec.hs` | This is the model for future lifecycle tests. |
| Acknowledge an incident and resume after expiry | Flow | `Lifecycle/LogPatternIncidentSpec.hs` and notification tests use the test clock | Remove overlap only after assertion comparison. |
| Merge similar log patterns automatically | Contract | `Pages/LogPatternsSpec.hs` covers embedding, merge grouping, query aggregation, and internal unmerge override | No customer page or API action exposes log-pattern unmerge. Treat that as a product decision, not a missing integration test. |
| Resolve or archive stale issues | Flow | `Pages/ErrorPatternsSpec.hs` resolves through the page handler and verifies the issue remains visible with a `RESOLVED` badge. `Pages/AnomaliesSpec.hs` proves archive moves an issue out of Inbox and into Archived | Resolve records recovery; archive is the explicit removal action. |
| View anomaly lists and details | Flow | `Pages/AnomaliesSpec.hs` crosses ingest and jobs into list/detail handlers, then checks service/type filters, distinct pages, and the rendered zero state | Retain these checks in the shared anomaly lifecycle. |

### Monitors, alerts, and notifications

| Customer flow | Coverage | Evidence | Gap or next action |
|---|---|---|---|
| Create, update, mute, unmute, resolve, and delete a monitor | Flow | `CLI/CLILifecycleSpec.hs` and `Web/ApiV1Spec.hs` | Add the HTML form path only for fields that the API cannot prove. |
| Apply a monitor from configuration twice | Flow | CLI lifecycle checks idempotency by title | Add YAML golden coverage for all supported fields. |
| Evaluate a query monitor and enter alert state | Flow | `MonitoringSpec.hs` creates a widget monitor, ingests telemetry, runs the evaluator, and captures PagerDuty delivery | Check the rendered alert history or API result. |
| Recover an alert and notify once | Flow | The same widget lifecycle changes the query and captures the recovery delivery | Check the rendered alert history or API result. |
| Honor hysteresis, interval, mute, renotify, and stop-after settings | Flow | `MonitoringSpec.hs` uses three workflows for alert suppression and recovery, reminders and stop limits, and mute expiry | Retain these setting boundaries in the lifecycle assertions. |
| Route alerts to email, Slack, Discord, WhatsApp, and PagerDuty | Flow | `Pages/Projects/IntegrationsSpec.hs` uses one provider table and checks typed payloads, sent history, team targets, disabled-channel suppression, and multi-target delivery | Keep browser coverage for settings controls and provider redirects. |
| Simulate a monitor from telemetry through recovery | Flow | `MonitoringSpec.hs` creates a dashboard widget and alert through handlers, ingests a real span, advances the shared clock, and runs the production evaluator. It checks exact Email, Slack, Discord, WhatsApp, and PagerDuty trigger/recovery targets, a silent repeat tick, and settings-based channel gating | Use this lifecycle as the model for future time-dependent alert coverage. |
| Limit notification volume and flush digests | Flow | Notification and digest background-job specs | Add the final digest content and recipient assertion. |
| Stop notifications for deleted projects | Flow | `Lifecycle/LogPatternIncidentSpec.hs` proves delivery before the delete handler and suppresses ingest, reports, and monitor alerts after it sets both `deleted_at` and `active = false` | There is no separate customer action that only deactivates a project. |
| Test an integration from the settings page | Flow | `Pages/Projects/IntegrationsSpec.hs` sends all five channel types through the settings handler and checks the recorded result | Add a browser test only for the form interaction and visible result. |
| Connect and disconnect notification integrations | Flow | `Pages/Projects/IntegrationsSpec.hs` covers Slack install state and handler cleanup, PagerDuty validation/connect/disconnect, Discord and WhatsApp routing, and channel-form persistence | Discord has no disconnect route. WhatsApp is configured as team phone numbers, not as a separate connection. |
| Preserve third-party payload contracts | Flow | Bot response goldens cover Slack and Discord. Integration tests inspect typed Slack, Discord, WhatsApp, and PagerDuty payloads. Billing tests record Stripe and LemonSqueezy requests and process signed webhooks. | Add a golden only when a provider schema changes beyond these structural assertions. |

### Dashboards and widgets

| Customer flow | Coverage | Evidence | Gap or next action |
|---|---|---|---|
| Create, edit, star, duplicate, and delete a dashboard | Flow | CLI lifecycle, API v1, and page specs | Compare overlap. Keep the shortest test that covers each distinct entry point. |
| Apply dashboard configuration twice | Flow | CLI lifecycle and API v1 cover file-path idempotency | Add YAML round-trip coverage for tabs, layouts, and monitor fields. |
| Add, edit, duplicate, reorder, and remove widgets | Flow | `Pages/DashboardWidgetsSpec.hs` covers every widget type in one geometry lifecycle and keeps focused edit, removal, and stale-save cases | Retain the distinct edge cases around the main lifecycle. |
| Run log, metric, stat, table, and chart widgets | Flow | `Pages/DashboardsSpec.hs` sends one event through log, table, series, and stat widget decoders; metric ingestion covers the native metric path | Retain the one-dataset execution flow and the every-type persistence lifecycle. |
| Create an alert from a widget | Flow | `MonitoringSpec.hs` uses the saved widget ID across form submission, evaluation, delivery, query sync, recovery, and deletion | Add browser coverage only for the alert dialog behavior. |
| Use dashboard tabs | Flow | `Pages/DashboardWidgetsSpec.hs` loads a two-tab YAML dashboard, adds and duplicates on one tab, reorders the other tab, and reloads both | Add rename only when a customer handler supports it. |
| Auto-provision metric dashboards | Flow | `BackgroundJobs/DashboardAutoProvisionSpec.hs` | Add customer page visibility and opt-out persistence. |
| Filter dashboard data by time and parameters | Flow | `Pages/DashboardsSpec.hs` ingests current and old traces, applies a URL variable to a real widget query, and proves the selected range excludes old data | Retain this inside the multi-shape widget workflow. |
| Render an empty or failed widget clearly | Flow | `Pages/DashboardsSpec.hs` proves a malformed widget query returns local error data and a corrected retry clears it; the widget renderer has dedicated error-banner markup | Verify banner visibility and clearing in the browser-only tier. |

### Reports, replay, service map, and code context

| Customer flow | Coverage | Evidence | Gap or next action |
|---|---|---|---|
| Enable and disable report notifications | Flow | `Pages/ReportsSpec.hs` toggles both preferences, runs both jobs, captures Discord delivery, and checks muted generation | Add email-specific checks only for its distinct weekly content. |
| Generate daily and weekly reports | Flow | The report lifecycle checks generation, list, detail, delivery, muted generation, and project isolation | Add content assertions only for fields that customers use. |
| Record and play a replay session | External | The MinIO-backed group passes Kafka ingest, exact payload, merge, shard, player, and empty-session flows | Keep it in the external tier. Add share-link playback. |
| Recover from corrupt or missing replay data | Flow | `ReplaySpec.hs` checks decode errors and user-facing errors | Keep focused boundary tests for binary payload handling. |
| Expire replay data | Flow | `BackgroundJobs/ExpirySpec.hs` uses the test clock | Add object deletion to the MinIO tier. |
| Build and view a service map | Flow | `Pages/ServiceMapSpec.hs` covers ingest, rollup, filters, page data, and trace view | Retain algorithm checks only where the flow cannot locate a regression. |
| Connect a Git host and map source code | Flow | `Pages/GitSyncSpec.hs` connects, updates, and disconnects through settings handlers. `Pages/CodeContextSpec.hs` maps service frame paths, preserves revisions, fetches and caches a recorded blob, and renders unmapped guidance. `Pkg.GitSpec.hs` uses trimmed live-provider fixtures for four hosts | Keep the opt-in live-host smoke tier for vendor drift. |
| Share an ingested event and expire the link | Flow | `Pages/ShareSpec.hs` covers ingest, page and JSON API creation, ownership rejection, public rendering, clock expiry, and content removal. Trace shares can expose only their matching replay session | The product has no issue, query, or dashboard share-link routes. |

### Configuration and operational behavior

| Customer flow | Coverage | Evidence | Gap or next action |
|---|---|---|---|
| Configure Prometheus and ingest a scrape | Flow | `Pages/PrometheusSpec.hs` checks save failure, lease, and ingest | Add edit, disable, re-enable, and delete around a real local scrape target. |
| Configure object storage | External | `Pages/BusinessFlowsSpec.hs` connects, renders, and removes real MinIO settings. `ReplaySpec.hs` records and plays through MinIO. | Keep both groups in the external tier. |
| Run pending migrations safely | Flow | `System/MigrationRunnerSpec.hs` covers apply, repeat, drift, and invalid SQL | Retain this operational integration flow. |
| Continue processing after one background job fails | External | App job bodies cover failure and recovery locally. `jobsRunner` rethrows failures to Odd Jobs, whose pinned dependency suite proves a second job runs after the first fails | Add a real-worker smoke tier if the repository adopts lifecycle hooks that can start and stop Odd Jobs safely in tests. |
| Isolate all project-owned resources | Flow | Dashboards, widget monitors, reports, runtime issues, error patterns, shares, and replay sessions have cross-project assertions | Extend this matrix when a new project-owned resource appears. |

## Remaining test work

The local Haskell customer-flow pass is complete. Do the remaining work in this order unless new failures give stronger evidence:

1. Add optional real-service tiers for Kafka, MinIO, Git providers, and provider OAuth when their prerequisites are available.
2. Add a real Odd Jobs worker smoke tier when tests can start and stop the worker safely.
3. Implement the browser-only backlog below.

## Browser tests to write last

Haskell integration tests remain the default.
When DOM behavior, browser state, or JavaScript is the feature, use browser automation.

Covered now:

- Drag a dashboard widget, wait for its save request, reload, and verify the same position and complete canvas.
- Resize a dashboard widget and verify that its ECharts instance receives the new dimensions.
- Use the query editor completion menu with the keyboard. Verify selection, insertion, continued focus, and the next operator menu.
- Force a chart-data failure, verify its local banner, restore the real endpoint, and verify that a refresh clears the banner.
- Submit the complete onboarding sequence through HTMX swaps and the notification toast.
- Create a dashboard without a team and wait for the real redirect before checking the list.
- Keep dashboard creation and log search usable without horizontal overflow at a 320px viewport.

Remaining browser cases:

- Change a dashboard tab and check lazy loading, URL state, and browser history.
- Change the time range and check that all widgets refresh once with the same range.
- Open a log detail panel, expand a trace, and return without losing scroll position.
- Start live tail, reconnect after a transport loss, and show the loss or reconnect state.
- Play a replay session with multiple shards and seek across a shard boundary.
- Use destructive confirmation dialogs for dashboards, monitors, projects, and integrations.
- Verify the remaining navigation drawers and secondary actions on a narrow viewport.
- Add focus containment and restoration to the shared modal, then verify it through one representative dialog.

Do not use browser tests to repeat database state or handler response checks.
Each browser test must cover a browser-only risk from this list.

## Work log

### 2026-08-24

- Inspected repository instructions and the Haskell test harness.
- Found no repository `AGENTS.md` file.
- Recorded existing uncommitted edits and reserved them as user-owned work.
- Inventoried the integration specs, route groups, and existing lifecycle tests.
- Classified coverage by customer flow instead of file count or endpoint count.
- Identified monitor delivery, widget alerts, deletion suppression, and project isolation as the first gaps.
- Created this document before adding new tests.
- Found that the global monitor query did not filter inactive or deleted projects.
- Added one lifecycle test for alert delivery, project deletion, and later suppression.
- Changed active-monitor selection to require an active, non-deleted owning project.
- Found that two widget-monitor tests used IDs that the widget handler discarded.
- Replaced those tests with one lifecycle that uses the saved widget ID.
- The lifecycle covers monitor creation, alert delivery, query sync, recovery, and deletion.
- Found that daily channel delivery was inside the weekly preference branch.
- Moved channel delivery under the preference for the generated report type.
- Replaced the report toggle test and obsolete comments with one daily report lifecycle.
- The report lifecycle covers preferences, generation, Discord delivery, list, detail, and muted generation.
- Found that report detail loaded a global report ID after it checked only the URL project.
- Scoped report lookup by project ID and added a cross-project detail assertion.
- Found that widget monitor lookup also used a global widget ID.
- Changed its model API to require a project ID and added an isolation assertion.
- Extended the report lifecycle with weekly generation and Discord delivery.

### 2026-08-25

- Found that issue detail loaded a global issue ID after it checked only the URL project.
- Reused the existing project-scoped model lookup in issue detail and AI chat routes.
- Extended the error pipeline from exception ingest through the rendered issue page.
- Added a cross-project issue detail assertion to the same ordered pipeline.
- Connected the runtime issue page to the original trace ID and bounded timestamp.
- Found that dashboard page actions loaded global dashboard IDs after checking only the URL project.
- Added a project-scoped dashboard lookup and required it in all page loaders and actions.
- Added dashboard isolation to the widget-monitor lifecycle.
- Replaced two no-op share tests and their obsolete comments with one public-link lifecycle.
- The share lifecycle covers event ingest, link creation, public rendering, expiry, and content removal.
- Started the repository's local MinIO service and ran the real replay object-store tier.
- The replay tier passed ingest, exact payload, merge, shard, size-cap, player, and empty-session flows.
- Replaced the pending S3 placeholder and direct-SQL removal setup with one real settings lifecycle.
- The settings lifecycle connects MinIO, renders the saved bucket, removes it, and checks durable state.
- Strengthened the trace page flow to check the parent, child, and visible error count.
- Added an explorer flow for the `trace_id` query used by customer deep links.
- Reclassified provider contracts after checking the existing payload, request, webhook, and bot goldens.
- Found that share creation did not verify ownership of the target event.
- Required an exact project, timestamp, and event match before creating a share row.
- Added share and replay-session isolation to existing lifecycle examples.
- Consolidated eight ordered monitor examples into three explicit workflows.
- The workflows retain duplicate suppression, recovery, interval, renotify, stop-limit, and mute-expiry checks.
- Found that cached API keys still accepted ingest after project deletion.
- Restricted API-key resolution to active keys on active projects and invalidated cached keys during deletion.
- Restricted report generation to active projects and extended deletion coverage across ingest, reports, and alerts.
- Replaced eleven overlapping dashboard-widget examples with six stronger lifecycle and edge-case examples.
- Combined five tab fixtures into one load, add, duplicate, reorder, and reload workflow.
- Replaced separate provider notification tests with one table-driven settings workflow.
- Combined team routing, disabled-channel behavior, PagerDuty connection, and form inversion into focused lifecycles.
- Replaced five order-dependent API-key CRUD examples with one create, ingest, revoke, reject, activate, and ingest lifecycle.
- Refreshed the API-key cache on revoke and activation and restricted settings mutations to keys owned by the URL project.
- Found that the onboarding spec creates a fresh project for each step despite describing the examples as one sequence.
- Reclassified first-event onboarding coverage until one project moves from empty state to a customer query result.
- Consolidated five onboarding examples into one same-project handler lifecycle.
- Extended the lifecycle from the empty integration state to a filtered log-explorer result for the first trace.
- Extended the existing anomaly archive lifecycle from database state to Inbox removal and Archived-list visibility.
- Reclassified event search after confirming real ingest, filtering, trace deep links, and bidirectional pagination already reach the customer data handler.
- Extended the existing stat regression so one ingested trace executes as log, table, series, and scalar widget data.
- Extended the orphan-error notification workflow through settings disable, suppression, re-enable, and resumed delivery.
- Replaced ten order-dependent team examples with one routing lifecycle and one validation table.
- The routing lifecycle now creates the team through the page handler, verifies its member, sends Slack and PagerDuty tests, deletes it, and checks later suppression.
- Extended the real widget query workflow with URL-variable filtering and a fixed time range that excludes an older matching event.
- The same workflow now proves a malformed query returns widget-local error data and a corrected retry clears it.
- Confirmed that log-pattern unmerge has no customer page or API route; only the automatic merge job and internal model operation exist.
- Added one anomaly-list handler workflow for service and type filters, distinct result pages, and the rendered empty state.
- Found that bulk unacknowledge returned 500 when selected acknowledged recurrences shared the open-issue uniqueness key.
- Changed unacknowledge and timed acknowledgement expiry to keep one actionable recurrence and archive competitors.
- Extended the Slack install invariant through its real disconnect handler and verified credential and default-channel cleanup.
- Removed an integration-spec order dependency by giving the notification channel-form workflow its own Slack prerequisite.
- Confirmed that resolving a runtime error intentionally keeps it in Inbox as history; archive is the removal action.
- Extended the assign/resolve/subscribe workflow to verify the resolved issue title and badge on the rendered list.
- Removed the redundant inactive-project gap: the only customer action is deletion, which also sets `active = false` and is already covered end to end.
- Found that the JSON share API created rows without checking event ownership, unlike the page handler.
- Added the ownership check, moved the JSON entry point into the real shared-event lifecycle, and removed its random-ID API test.
- Corrected the share matrix: the product shares telemetry events with optional trace or replay context, not issues, queries, or dashboards.
- Consolidated three order-dependent Git settings examples into one connect, update, and disconnect lifecycle.
- Confirmed that recorded request and response contracts already cover GitHub, GitLab, Gitea, and Bitbucket source operations.
- Confirmed that the repository compose stack does not provide Kafka; retained real-Kafka Live Tail as an optional external tier.
- Confirmed that scheduler failure isolation belongs to the pinned Odd Jobs runtime. Its dependency suite covers a second job running after a first job fails; the local helper only runs job bodies and cannot prove scheduler status transitions.
- Started the browser-only phase from a green 48-test baseline.
- Consolidated eight onboarding examples into two customer journeys.
- Consolidated four billing examples into one page journey and one table-driven provider boundary.
- Consolidated twelve query-editor examples into three workflows and added real keyboard selection and focus coverage.
- Consolidated root-grid, nested-grid, and client-error checks into one dashboard canvas workflow.
- Removed the transient drag example because the persisted drag-and-reload workflow proves both movement and durable state.
- Extended widget resize coverage from GridStack metadata to the live ECharts dimensions.
- Found and removed a dashboard-creation navigation race by waiting for the redirect before loading the list.
- Reworked the widget-monitor lifecycle as a deterministic customer simulation: configure, ingest, advance time, evaluate, suppress a repeat, recover, change notification settings, and trigger again.
- Found that query monitors ignored `disabled_channels` and never sent WhatsApp monitor alerts.
- Made query-monitor dispatch honor every channel toggle and added a configurable WhatsApp monitor template for trigger and recovery.
- Added a browser failure/recovery test for the chart error banner.
- Added a 320px dashboard and log-explorer reflow test.

## Verification log

- `cabal build integration-tests --ghc-options='-O0 +RTS -A64m -RTS'`: passed.
- `integration-tests --match 'Deleted Project Notification Lifecycle'`: 1 example, 0 failures.
- `integration-tests --match 'Lifecycle.LogPatternIncident'`: 2 examples, 0 failures.
- `make test-shards SHARDS=6`: 3 shards passed. Five tests failed during concurrent PostgreSQL deadlocks.
- Each of the five failed tests passed alone: 5 examples, 0 failures in total.
- The broad-run failures are suite-contention evidence. They do not reproduce in sequential runs.
- `integration-tests --match 'Widget Monitor Lifecycle'`: 1 example, 0 failures.
- `integration-tests --match 'Report Notification Lifecycle'`: 1 example, 0 failures.
- `integration-tests --match 'Monitoring'`: 19 examples, 0 failures.
- `integration-tests --match 'Error Pattern Pipeline'`: 26 examples, 0 failures.
- `integration-tests --match 'Pages.DashboardWidgets'`: 27 examples, 0 failures.
- `integration-tests --match 'Dashboards Tests'`: 15 examples, 0 failures.
- `integration-tests --match 'Shared Event Lifecycle'`: 1 example, 0 failures.
- `integration-tests --match 'Pages.Telemetry'`: 7 examples, 0 failures.
- `integration-tests --match 'replay round-trip'`: 6 examples, 0 failures with local MinIO.
- Full sequential `integration-tests`: 774 examples, 0 failures, 29 pending.
- The optional MinIO replay cases passed separately before the full run.
- `integration-tests --match 'S3 Configuration'`: 1 example, 0 failures with local MinIO.
- `integration-tests --match 'trace overlay projection'`: 1 example, 0 failures.
- `integration-tests --match 'opens a trace-id deep link on the matching explorer events'`: 1 example, 0 failures.
- `integration-tests --match 'Shared Event Lifecycle'`: 1 example, 0 failures after the ownership check.
- `integration-tests --match 'sessionFileKeys'`: 2 examples, 0 failures.
- `integration-tests --match 'Monitoring'`: 10 examples, 0 failures after consolidation.
- `integration-tests --match 'Deleted Project Notification Lifecycle'`: 1 example, 0 failures with ingest and report suppression.
- `integration-tests --match 'Pages.DashboardWidgets'`: 16 examples, 0 failures after consolidation.
- `integration-tests --match 'Pages.Projects.Integrations'`: 8 examples, 0 failures after consolidation.
- `integration-tests --match 'Pages.Api'`: 1 example, 0 failures with real OTLP ingest across revoke and activation.
- `integration-tests --match 'Onboarding Flow'`: 1 example, 0 failures across all forms and the first customer query.
- `integration-tests --match 'Pages.Anomalies'`: 16 examples, 0 failures with Inbox-to-Archived visibility.
- `integration-tests --match 'renders one ingested trace as log, table, series, and scalar widgets'`: 1 example, 0 failures.
- `integration-tests --match 'Project settings disable and restore delivery for an orphaned runtime error'`: 1 example, 0 failures.
- `integration-tests --match 'BackgroundJobs.Notifications'`: 6 examples, 0 failures after the settings lifecycle extension.
- `integration-tests --match 'Pages.Projects.ManageMembers'`: 2 examples, 0 failures after team consolidation.
- `integration-tests --match 'Pages.Projects.Integrations'`: 8 examples, 0 failures with handler-created team routing and deletion.
- `integration-tests --match 'filters ingested data by time and URL parameters, reports a bad query, and recovers'`: 1 example, 0 failures.
- `integration-tests --match 'Pages.Anomalies'`: 17 examples, 0 failures with filters, pagination, zero state, duplicate unacknowledge, and timed expiry.
- `integration-tests --match 'Pages.Projects.Integrations'`: 8 examples, 0 failures with Slack and PagerDuty disconnect lifecycles.
- `integration-tests --match 'Pages.ErrorPatterns'`: 26 examples, 0 failures with rendered resolved-state coverage.
- `integration-tests --match 'Pages.Share'`: 1 example, 0 failures across page/API creation, ownership rejection, public rendering, and expiry.
- `integration-tests --match 'GitHub Sync Settings'`: 1 example, 0 failures after lifecycle consolidation.
- `integration-tests --match 'Pages.CodeContext'`: 8 examples, 0 failures.
- `unit-tests --match 'Pkg.Git'`: 17 examples, 0 failures against recorded provider fixtures.
- Full sequential `integration-tests`: 755 examples, 0 failures, 28 pending after the conciseness pass.
- Full sequential `integration-tests`: 739 examples, 0 failures, 28 pending after notification, API-key, onboarding, and archive lifecycle consolidation.
- Final full sequential `integration-tests`: 728 examples, 0 failures, 28 pending. The pending cases require external binaries, services, credentials, or an intentionally unavailable backend-failure fixture.
- Final full `unit-tests`: 272 examples, 0 failures.
- Browser baseline before consolidation: 48 Playwright tests passed.
- Final full `test-e2e`: 31 Playwright tests passed after consolidation and the chart-recovery and 320px additions.
- `integration-tests --match 'Widget Monitor Lifecycle'`: 1 deterministic multi-tick simulation, 0 failures.
- `integration-tests --match 'Monitoring'`: 10 examples, 0 failures after five-channel monitor dispatch.
- Focused chart failure/recovery browser test: 1 passed.
- Focused 320px responsive browser test: 1 passed.
- Post-routing full sequential `integration-tests`: 728 examples, 0 failures, 28 pending.
- Post-routing full `unit-tests`: 272 examples, 0 failures.
- Deleted-project notification guard after explicit PagerDuty enablement: 1 example, 0 failures.
- Final `git diff --check`: passed.
- Final matrix audit: no `Partial`, `Missing`, or `TODO` coverage rows remain.
- The focused tests use `/tmp/monoscope-codex.3GT5M3` as an isolated Cabal build directory.
- The isolated directory prevents the live development REPL from replacing test objects.
