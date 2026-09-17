# Project improvement delivery checklist

This checklist is the implementation record for the seven requested improvements.
Each item has a measurable acceptance condition and an industry precedent so that
the product behavior is intentional rather than a collection of dashboard widgets.

## Research notes

- [New Relic Dynamic Flow Map](https://docs.newrelic.com/docs/service-architecture-intelligence/maps/dynamic-flow-map/)
  keeps a focal entity/transaction context through dependency and trace investigation.
- [New Relic service maps](https://docs.newrelic.com/docs/new-relic-solutions/new-relic-one/ui-data/service-maps/service-maps/)
  aggregate external dependencies to prevent map clutter while retaining drill-down.
- [Datadog Service Map](https://docs.datadoghq.com/tracing/services/services_map/)
  combines APM and RUM-derived dependency evidence and links map entities to adjacent telemetry.
- [Atatus Browser Monitoring](https://docs.atatus.com/docs/browser-monitoring/overview.html)
  brings page, AJAX, error, and session evidence together; its
  [dependency view](https://docs.atatus.com/docs/flowview-guide/service-dependency.html)
  keeps services, databases, queues, and uninstrumented external calls visible.
- [Moesif API Observability](https://www.moesif.com/docs/getting-started/api-observability/)
  combines server API calls with browser activity to reconstruct a customer journey, while
  [bookmarkable workspaces](https://www.moesif.com/docs/api-analytics/)
  preserve filters in a shareable URL.

The endpoint map therefore reads bounded direct-dependency rollups, preserves the
endpoint scope in its Service Map CTA, and exposes trace evidence rather than
claiming a global topology is endpoint-specific. Endpoint RUM only exposes replay
availability after an indexed recording match; application-level Web Vitals remain
on the RUM dashboard until the collector supplies a trace/session correlation key.

## 1. Compliant, fast dashboard navigation

- [x] Define a reusable `BodyWrapper` tab-navigation response contract that preserves
  full-page URLs, OOB nav morphing, history, and hover preload.
- [x] Remove duplicate dashboard-shell work from the tab path without creating a
  one-off `/content` endpoint or imperative active-tab state.
- [x] Add handler and browser regressions for response shape, active tab, history, and
  navigation latency.

Precedent: Datadog keeps service-navigation context while moving between APM views;
Sentry preserves trace/replay investigation context rather than opening a detached view.

The shared `NavigationResponse` contract returns a document for normal navigation and
the requested content plus an OOB tab-strip morph for HTMX. The tab handler processes
only the selected tab and returns client-fetched skeletons for swaps. `DashboardWidgetsSpec`
pins the response contract; `e2e/tests/endpoint-analytics.spec.ts` pins active-tab,
canonical-URL, Back-history, and the 1.5-second tab settlement budget. A real-browser
run of the supplied Dependencies route measured 1.04 s for Dependencies → Operations
(the HTMX response itself was 1.15 s / 93 KB, versus 7.04 s / 300 KB for full navigation).

## 2. Endpoint-to-dependency rollups

- [x] Define a time-bucketed endpoint-to-direct-dependency rollup with calls, failures,
  duration, trace count, and service/resource identity.
- [x] Populate it from span ingestion and make it safe to rebuild/idempotently backfill.
- [x] Use it for endpoint dependency health, regressions, and map overlays; retain a
  trace drill-down for source evidence.

Precedent: New Relic’s Dynamic Flow Map uses aggregated trace relationships to show
latency/error correlation; Datadog exposes service dependency data as a first-class API.

## 3. Endpoint-to-RUM journey

- [x] Establish the correlation contract between browser sessions, page activity,
  browser request spans, server endpoint spans, and replay availability.
- [x] Show affected sessions, replay-ready sessions, affected pages, browser cohorts,
  and request-linked Web Vitals only when telemetry supports them.
- [x] Provide truthful instrumenting empty states when correlation is unavailable.

Precedent: Sentry links replay to frontend and backend errors through tracing; New Relic
uses replay to reconstruct the user journey behind browser failures.

The implemented correlation contract is documented in
[endpoint-rum-correlation.md](endpoint-rum-correlation.md): endpoint hash + browser
session id scopes requests; an indexed replay must have the same session id; a Web
Vital is endpoint-linked only when its metric attributes carry that session id.

## 4. Dashboard performance observability

- [x] Instrument dashboard shell, tab, widget, and chart-query work with bounded metric
  dimensions and trace spans.
- [x] Emit cache outcome, response bytes, query duration, and a browser-reported
  `dashboard.time_to_settled` measurement.
- [x] Establish a tested performance budget for tab shell and initial widget settlement.

Precedent: modern APM products treat their own UI latency as an observable transaction,
not an anecdotal browser complaint.

`Pkg.Metrics` owns bounded shell, widget, query, cache-outcome, response-size, settlement,
and budget-violation instruments. Shell/widget/query operations have trace spans; the
browser posts validated completion samples after grid hydration. The budget and alert
contract is in [dashboard-performance-budget.md](dashboard-performance-budget.md).
TimeFusion supplies its real scan/decode byte telemetry at the engine boundary, where it
can be correlated to the dashboard query span without pretending pgwire response bytes
are scan bytes.

## 5. Ingestion redaction

- [x] Apply configured redaction before fields are persisted or forwarded.
- [x] Cover nested attributes, resource values, logs, and span event fields without
  changing unrelated telemetry.
- [x] Add regression tests proving raw secrets never reach the stored field payload.

Precedent: browser/replay vendors expose explicit privacy controls because collected
observability data can contain customer input and credentials.

## 6. Apple-Silicon CI parity

- [x] Make the local TimeFusion integration path runnable on Apple Silicon, or provide
  a documented remote parity runner that exercises the same integration contract.
- [x] Make `make ci-signoff` report the supported fallback distinctly from a failed test.
- [x] Document the chosen path and add a self-check.

Verified with `make ci-selftest` and `CI_NO_ATTEST=true make ci
CHECKS="integration-tests"`: a native `timefusion:local-arm64` image is
auto-detected, accepts pgwire connections, and gives the runner `tf-real`.
The full suite reached all four integration shards; any failures are now test
failures rather than a missing-extension/image limitation. The builder selects
the ARM `neoverse-n1` CPU baseline (rather than TimeFusion's AMD-only
`x86-64-v3` default). Unavailable TimeFusion makes `integration-tests`
explicitly unrunnable, while `CI_ALLOW_DEGRADED=true` is deliberately not
attested.

## 7. Endpoint-scoped dependency map

- [x] Add a dependency-map investigation panel to endpoint analytics, scoped to direct
  dependencies and decorated with calls, errors, and tail latency.
- [x] Link nodes to filtered traces and the broader Service Map without losing endpoint
  context.
- [x] Supply an explicit no-dependency state and test map scope/data shaping.

Precedent: New Relic’s Dynamic Flow Map focuses a graph on one entity and surfaces
correlated latency/error changes; its external-services view links map selections to
transaction and trace evidence.
