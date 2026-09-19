# Built-in Routines: Research and Product Recommendations

## Summary

Monoscope needs two routine views:

- An installed-routines view for active and paused routines.
- A template gallery for built-in routines that a user can add.

The gallery must contain operational workflows with useful defaults. It must not be only a list of prompts.

## Baseline before this implementation

This section records the limitations that motivated the work. It describes the
code before the routine catalog changes that accompany this document.

A routine today is an AI conversation with an interval. There is no template concept and no run record.

- Storage: `apis.ai_routines` in [`0188_ai_threads_and_routines.sql`](../static/migrations/0188_ai_threads_and_routines.sql). One row for each conversation (`UNIQUE (project_id, conversation_id)`). It holds `interval_minutes` (5 to 10080), `timezone`, `active`, `next_run_at`, `last_run_at`, `running_since`.
- The task lives in the conversation history. The scheduled prompt is the fixed string `"Run the recurring task defined in this conversation now. Report what you did and the result."` ([`src/BackgroundJobs.hs:351`](../src/BackgroundJobs.hs)).
- Scheduling: [`src/Models/Apis/Issues.hs:1279-1332`](../src/Models/Apis/Issues.hs). `upsertRoutine` sets the first run, `claimRoutine` takes a lease, `completeRoutine` schedules the next run from the completion time, `pauseRoutine` clears `next_run_at`, `listDueRoutines` is the recovery source used at [`src/BackgroundJobs.hs:1338`](../src/BackgroundJobs.hs).
- Execution: [`src/BackgroundJobs.hs:343-382`](../src/BackgroundJobs.hs). A run has a 20-minute timeout. A lease older than 30 minutes can be reclaimed. A timeout or a failure writes a `ChatExecutionEvent` message and schedules the next run.
- Result delivery: every run appends a message to its conversation. There is no destination field and no run record.
- UI: an interval field and a pause control in the thread page and the sidebar ([`src/Pages/AIThreads.hs`](../src/Pages/AIThreads.hs), [`src/Pages/BodyWrapper.hs:880-950`](../src/Pages/BodyWrapper.hs)).
- The `timezone` column exists but no code reads it.

### Tools available at the baseline

A routine can only use the agent tools in [`src/Pkg/AI.hs:684-773`](../src/Pkg/AI.hs):

- Telemetry only: `get_services`, `get_schema`, `get_facets`, `get_field_values`, `count_query`, `sample_logs`, `run_query` (KQL), `run_sql_query` (SQL on `otel_logs_and_spans`).
- `send_to_slack`, because `actionsAllowed ScheduledRoutine = True`.
- GitHub tools (`get_deployments`, runbook and code reading) need a project repository mapping.
- The incident tools (`get_incident_context`, `get_related_incidents`, `get_investigation_history`) are limited to an authorized Slack thread. A routine started from the web cannot use them.

No tool could read monitors, issues, log patterns, dashboards, or billing usage. Every catalog entry that needed those required a new tool first.

## Implementation status

The accompanying first release adds:

- A category-indexed gallery with 18 built-in, versioned routine templates.
- Project-scoped agent tools for issues, incidents, monitors, endpoints, log patterns, dashboards, and project metadata.
- Project-scoped repository, runbook, code-context, and deployment reads for web routines when a source integration is configured.
- Public incident list/get operations in the API, MCP tool registry, and CLI, so operators and agents use the same project-scoped incident surface.
- Fixed-interval, daily, weekday, and weekly schedules in the project's IANA timezone.
- Pause, resume, and schedule deletion while keeping the conversation history.
- Read-only scheduled execution by default, with persisted action permission checked again before an external action.
- Durable run records for successes, failures, timeouts, and no-findings results. Findings-only templates suppress empty conversation messages.
- A recent-run interface, best-effort cancellation of an executing run, and explicit Slack delivery setup backed by persisted action permission.
- Estimated AI-token usage, configurable pricing, billing-meter submission, and billing-page visibility.

Optional follow-up work:

- Preview a template against recent data before installation.
- Add per-service scope editing and success-rate analytics.
- Promote more specialized entries from the full research catalog as their required product data becomes available.

## Principles from observability guidance

Industry guidance gives the following principles:

- Urgent alerts must focus on user-visible symptoms and actionable conditions.
- Scheduled routines must focus on trends, hygiene, planning, and follow-up work.
- SLO burn rates are better than isolated thresholds for many reliability decisions.
- Teams must regularly review alerts, incidents, and incomplete remediation work.
- Teams must monitor the observability pipeline itself.
- Teams must find missing service names, inconsistent attributes, excessive cardinality, and sampling gaps.
- Teams must monitor security events such as authentication failures and administrative changes.

Prometheus and Grafana recommend a small number of actionable alerts. They also recommend alerts that focus on user-visible symptoms:

- [Prometheus alerting practices](https://prometheus.io/docs/practices/alerting/)
- [Grafana alerting best practices](https://grafana.com/docs/grafana-cloud/observe-and-act/alert-and-measure-reliability/alerting/guides/best-practices/)

Google SRE recommends multi-window burn-rate alerts for SLOs:

- [Google SRE: Alerting on SLOs](https://sre.google/workbook/alerting-on-slos/)

Google also recommends regular postmortem reviews and tracked preventive actions:

- [Google SRE: Postmortem culture](https://sre.google/sre-book/postmortem-culture/)

OpenTelemetry documents collector queue saturation, refused telemetry, and exporter failures. These failures can hide system evidence:

- [OpenTelemetry Collector internal telemetry](https://opentelemetry.io/docs/collector/internal-telemetry/)

OpenTelemetry and Prometheus recommend stable resource names and controlled cardinality:

- [OpenTelemetry resources](https://opentelemetry.io/docs/concepts/resources/)
- [Prometheus instrumentation practices](https://prometheus.io/docs/practices/instrumentation/)

OWASP recommends monitoring authentication failures, authorization failures, administrative actions, and sensitive data leakage:

- [OWASP logging guidance](https://cheatsheetseries.owasp.org/cheatsheets/Logging_Cheat_Sheet.html)

The product distinction is:

- Monitors answer: "Does someone need to react now?"
- Routines answer: "What needs periodic attention, follow-up, or improvement?"

An hourly AI routine must not replace an availability or error-rate monitor.

## Recommended launch catalog

The **Needs** column states what is missing today. `Telemetry` means the routine works with the current tools.

| Routine | Default schedule | Needs | Result |
|---|---:|---|---|
| Daily Operations Briefing | Weekdays 09:00 | Telemetry; issues tool for the issue section; wall-clock schedule | Overnight incidents, error spikes, slow endpoints, traffic anomalies, and unresolved issues |
| Weekly Reliability Review | Weekly | Telemetry; wall-clock schedule | Reliability changes, recurring failures, latency regressions, and recommended priorities |
| Error Regression Radar | Hourly | Telemetry | Error groups that returned or materially increased after a quiet period |
| Slow Endpoint Watch | Hourly | Telemetry | Endpoints with latency that became worse than their recent baseline |
| Traffic Drop Watch | Every 30 minutes | Telemetry | Services or endpoints with unexpectedly low or absent traffic |
| Quiet Failure Finder | Daily | Telemetry | Low-volume failures that aggregate dashboards can hide |
| Dependency Degradation Digest | Hourly | Telemetry | Downstream services associated with increased errors or latency |
| New Service and Endpoint Digest | Daily | Telemetry; endpoints tool for confirmed new routes | New services, routes, operations, and environments |
| Telemetry Gap Detector | Hourly | Telemetry | Services that unexpectedly stopped logs, traces, or metrics |
| Telemetry Quality Check | Daily | Telemetry | Missing or inconsistent service, environment, version, trace, and deployment attributes |
| Noisy Log Sources | Daily | Telemetry | Services and messages that produce disproportionate log volume |
| Customer Impact Outliers | Hourly | Telemetry | Tenants, regions, or versions with materially worse reliability |
| On-call Handoff | At shift time | Issues tool; wall-clock schedule | Active issues, recent recoveries, current regressions, and watch items |
| Incident Follow-up Ledger | Daily | Incidents tool outside a Slack thread | Open incident actions, repeated symptoms, and items without observed improvement |
| Alert Quality Review | Weekly | Monitors tool | Flapping, repetitive, low-action, or poorly documented monitors |
| Observability Cost Review | Weekly | Telemetry; usage data for true cost | Fast-growing telemetry sources and possible filtering or sampling work |

The first release contains these 18 routines. The project-scoped tools added with it support their required reads:

1. Daily Operations Briefing
2. Weekly Reliability Review
3. Error Regression Radar
4. Slow Endpoint Watch
5. Telemetry Gap Detector
6. Telemetry Quality Check
7. Traffic Drop Watch
8. Dependency Degradation Digest
9. New Service and Endpoint Digest
10. On-call Handoff
11. Incident Follow-up Ledger
12. Alert Quality Review
13. Deployment Regression Review
14. Noisy Log Sources
15. Trace Correlation Audit
16. Telemetry Cost Watch
17. Authentication Failure Digest
18. Weekly SLO Review

These routines have broad value and need little setup. They also support the main Monoscope product functions.

Two limits identified during research were addressed by the first release:

- Daily and weekly templates now use stable project-local wall-clock schedules instead of drifting fixed intervals.
- Scheduled routines are now read-only by default instead of receiving action authority from their prompt.

Findings-only templates use a structured result flag, so empty runs are recorded without adding noise to the conversation.

## Full template catalog

Each section starts with the tools that its routines need.

### Reliability and performance

Telemetry tools are sufficient, except where an entry says otherwise. SLO routines also need stored SLO definitions, which do not exist.

#### SLO Budget Briefing

Report the consumed budget, remaining budget, and projected exhaustion date.

#### Fast and Slow Burn Digest

Find services with sustained error-budget consumption instead of transient threshold violations.

#### Capacity Runway

Find resources or throughput that approach known limits. Estimate the time until each limit.

#### Tail Latency Review

Find endpoints with worse p95 or p99 latency while the average remains stable.

#### Failure Ratio Review

Compare failures with total attempts instead of reporting raw failure counts.

#### Regional Health Comparison

Find regions that behave materially worse than their peers.

#### Version Health Comparison

Find regressions that affect one release or service version.

#### Critical Journey Health

Report the health of a configured workflow. Examples include login, checkout, upload, and search.

#### Background Job Watch

Find jobs that stopped succeeding, run late, or have longer durations.

#### Queue and Pipeline Health

Find growing lag, stalled processing, or unusual differences between input and output volume.

#### Database Symptom Review

Summarize timeouts, lock contention, connection failures, and slow operations.

#### Cache Effectiveness Review

Find lower cache hit rates that occur with increased downstream latency or load.

### Incident learning and operations

This section was blocked at the baseline because incident tools worked only inside an authorized Slack thread and no tool read issues or monitors. The first release adds project-scoped read tools for all three, plus public incident API, MCP, and CLI operations, so these routines can now be implemented without accepting caller-supplied project scope.

#### Postmortem Candidate Finder

Find incidents that meet configured impact or intervention criteria.

#### Postmortem Starter

Create a draft timeline with symptoms, affected services, and supporting evidence.

#### Recurring Incident Miner

Group recent incidents with similar errors, services, or environments.

#### Recovery Verifier

Make sure that telemetry returned to its baseline after an incident resolution.

#### Monitoring Failure Review

Find incidents that users found manually or alerts did not describe correctly.

#### Action Item Chase

Report promised remediation work that remains incomplete.

#### Runbook Coverage Audit

Find services with frequent failures and no associated runbook.

#### Runbook Effectiveness Review

Compare documented recovery steps with the steps that responders used.

#### Toil Report

Find repeated manual investigations or interventions that are candidates for automation.

#### Monthly Incident Review

Report MTTA, MTTR, recurrence, affected services, and important lessons.

### Telemetry quality

This is the strongest section for a first release. The telemetry tools are sufficient for all entries, except Collector Backpressure Watch, which needs collector self-telemetry in the project.

#### Unknown Service Cleanup

Report `unknown_service` values and unstable service names.

#### Missing Environment or Version

Find services that cannot connect telemetry to a deployment.

#### Trace Continuity Check

Find broken parent-child propagation and incomplete service paths.

#### Log and Trace Correlation Check

Find error logs without useful trace or span identifiers.

#### Schema Drift Review

Find new fields and incompatible attribute types.

#### Cardinality Risk Review

Find attributes with fast growth in unique values.

#### Sampling Coverage Review

Find services whose error traces or slow traces appear underrepresented.

#### Collector Backpressure Watch

Report enqueue failures, refused records, exporter failures, and full queues.

#### Clock Skew Detector

Find suspicious differences between event time and ingestion time.

#### Sensitive Data Review

Sample likely credentials, tokens, email addresses, and other sensitive fields.

#### Duplicate Telemetry Review

Find instrumentation that appears to emit the same event more than once.

#### Instrumentation Coverage Map

Find active services without logs, traces, metrics, or deployment metadata.

### Delivery and engineering effectiveness

These routines need the GitHub tools, which need a project repository mapping. `get_deployments` returns at most five deployment requests for each repository, which is too few for the frequency and trend reports. Flaky CI and vulnerable dependencies need new tools.

#### Deployment Health Check

Compare errors, latency, and traffic before and after each deployment.

#### Release Regression Digest

Rank deployments by their observed reliability impact.

#### Failed Deployment Follow-up

Find deployments that failed, rolled back, or did not reach a healthy state.

#### Unverified Deployment Finder

Find successful deployment records without matching production telemetry.

#### Change Failure Review

Find production changes associated with incidents or rollbacks.

#### Deployment Frequency Report

Report delivery trends by service or team.

#### Recovery Time Report

Measure the time from a production regression to measured recovery.

#### Stale Service Version Report

Find instances that still use old or mixed versions.

#### Flaky CI Digest

Find recurring unstable checks and affected repositories.

#### Vulnerable Dependency Digest

Prioritize exploitable dependencies that have available fixes.

### Security and abuse

The telemetry tools are sufficient for all entries, except Certificate and Credential Expiry. The quality of each result depends on the customer's own security logging.

#### Authentication Attack Watch

Find failure bursts, distributed failures, and password-spraying patterns.

#### Authorization Failure Review

Find repeated access-control failures by actor, route, or resource.

#### Privileged Action Digest

Report administrative and high-risk operations.

#### Suspicious Account Activity

Find unusual geography, clients, or behavior after authentication.

#### Input Validation Attack Review

Find repeated malformed or prohibited inputs.

#### Secret Leakage Review

Find likely credentials, API tokens, or connection strings in telemetry.

#### Security Logging Coverage

Find critical operations that do not produce enough audit evidence.

#### Certificate and Credential Expiry

Report certificates and credentials that are near expiration. This routine needs infrastructure or secret-manager integrations.

### Cost and data management

Volume routines can count rows and bytes with the telemetry tools. True cost needs the usage and billing tables. Dormant Monitor Review needs a monitors tool, and Unused Dashboard Review needs dashboard usage data that Monoscope does not record.

#### Telemetry Volume Forecast

Find projects that can exceed their current ingestion or storage trends.

#### Cost Anomaly Digest

Find sudden cost increases by service, environment, or signal.

#### Sampling Candidate Finder

Find high-volume healthy traces that are candidates for representative sampling.

#### Debug Log Cleanup

Find verbose logging that remains active in production.

#### Redundant Field Review

Find expensive attributes with little investigative value.

#### Retention Recommendation

Separate high-value forensic data from low-value bulk data.

#### Dormant Monitor Review

Find monitors that target services or fields that no longer exist.

#### Unused Dashboard Review

Find dashboards without recent usage. This routine needs dashboard usage data.

## Gallery design

Use a gallery similar to the Town reference. Use these Monoscope categories:

- Reliability
- Incidents
- Telemetry quality
- Delivery
- Security
- Cost
- Custom

Each template card must show:

- A one-sentence result.
- The default schedule.
- The required signals and integrations.
- The delivery destination.
- Whether it reports every run or only when it finds something.
- The state: `Added`, `Add`, or `Needs setup`.

Use `Add` when a routine needs a service scope, timezone, Slack destination, or other setup. After setup, the main control can become a toggle.

The setup flow can have these steps:

1. Select the template.
2. Preview findings from the last 24 hours or 7 days.
3. Select the scope, schedule, timezone, and destination.
4. Enable the routine.

Most routines must report only when they find something. Daily and weekly briefings can report after every run.

## Delivery

At the baseline every run appended its answer to the conversation, and Slack delivery depended on the prompt asking for it. The implementation now records every run and suppresses conversation messages when a findings-only routine explicitly returns `has_findings: false`. Each installation also has a persisted destination. Enabling Slack delivery grants only the Slack action, and that authority is checked again immediately before the external call.

## Data model

Built-in templates need versioned definitions. Each project needs a separate installation. The existing tables cover part of this:

```text
RoutineTemplate                      -- new
  key, version, category, title, description
  prompt/query plan, default schedule, requirements

RoutineInstallation                  -- extend apis.ai_routines
  existing: project, conversation, interval_minutes, timezone,
            active, next_run_at, last_run_at, running_since
  add:      template_key, template_version, destination,
            report_when (always | on findings), schedule kind

RoutineRun                           -- new
  scheduled time, started time, finished time, status
  findings, actions, error
```

Notes on the existing schema:

- A routine is always attached to a conversation (`UNIQUE (project_id, conversation_id)`). Installing a template means creating a conversation with the template prompt as its first message, then stamping the key and version on the routine row. The conversation stays the record of the prompt, so "an edit creates a custom routine" is only a change of `template_key` to null.
- At the baseline, chat messages were the only record. The executor now writes `RoutineRun` rows while retaining timeout and failure `ChatExecutionEvent` messages for conversation visibility. The routines page lists recent runs; aggregate success-rate presentation remains optional follow-up work.
- At the baseline, the routine row was deleted only by the conversation cascade. The implementation can now delete the schedule independently; retained run rows use `ON DELETE SET NULL` for the routine reference.

## Safety

At the baseline, every scheduled run enabled actions, and only the system prompt kept the model from using `send_to_slack`. The implementation now carries the routine identity and its persisted action grant in `ScheduledRoutine`, then rechecks the installation immediately before sending.

Built-in routines use read-only actions by default:

- Telemetry queries and summaries are allowed.
- A requested result can go to Slack.
- Monitor changes need a separate explicit permission.
- Ticket creation needs a separate explicit permission.
- Infrastructure changes need a separate explicit permission.
- Production remediation needs a separate explicit permission.

## Pause, cancel, and delete

The interface must separate these operations:

- **Pause:** Prevent future runs. The user can resume the routine.
- **Cancel run:** Stop the current run on a best-effort basis.
- **Delete routine:** Delete the installed schedule. Keep historical results according to the retention policy.
- **Delete conversation and history:** Delete the associated conversation data. This is a separate destructive operation.

The claim step already protects against stale queued jobs. `claimRoutine` only succeeds when the row is `active` and `next_run_at` still equals the scheduled instant of the job, and `pauseRoutine` clears `next_run_at`. A separate generation counter is unnecessary. A run must still read the state again before each external action, because a pause during a 20-minute run cannot stop it.

State at the baseline:

- **Pause:** exists (`pauseRoutine`), and the thread page and the sidebar expose it.
- **Resume:** does not exist. The user must save an interval again, which restarts the schedule from now.
- **Cancel run:** does not exist. A run stops only at the 20-minute timeout.
- **Delete routine:** does not exist on its own. The routine row disappears only when the conversation is deleted, through the foreign key cascade. Keeping the results while removing the schedule is impossible.
- **Delete conversation and history:** exists as the conversation delete.

The gaps are resume, cancel run, and a delete that keeps the run history.

The implementation closes all three gaps. Cancellation is best effort: it revokes action authority immediately and suppresses a late answer, while an already-running model request may continue until it returns or reaches the timeout.

## Schedule requirements

At the baseline, the engine supported fixed intervals from 5 minutes through 7 days. It calculated the next run from the completion time, which prevented overlapping runs and catch-up bursts but made every schedule drift by the run duration.

That model could not represent a schedule such as "09:00 every weekday in Europe/Berlin."

Built-in routines need these schedule functions:

- Daily and weekly wall-clock schedules.
- IANA timezones. The `timezone` column exists, but no code reads it.
- Weekday selection.
- Stable schedules that do not drift after a slow run. This means the next run is calculated from the scheduled instant, and a still-running routine skips instead of overlapping.
- Missed-run behavior, for a routine that was paused or a worker that was down over several instants.

Already met: stale queued jobs cannot reactivate a paused routine, and one scheduled instant admits exactly one worker. See `claimRoutine` and `pauseRoutine` in [`src/Models/Apis/Issues.hs:1295-1332`](../src/Models/Apis/Issues.hs).

The implementation adds the wall-clock schedule variants, uses the persisted project timezone, and calculates successors from the intended scheduled instant.

## Suggested order of work

1. Wall-clock and timezone schedules. Every briefing template depends on this.
2. `RoutineRun` records, and the no-findings signal that they make possible.
3. Template and installation fields, and the gallery on top of them.
4. A destination field, and installation-level action permissions.
5. New agent tools for issues, monitors, and endpoints. These open the incident, alert-quality, and on-call sections of the catalog.
6. Resume, cancel run, and a delete that keeps history.
