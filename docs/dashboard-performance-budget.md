# Dashboard performance budget

The dashboard shell emits bounded OpenTelemetry metrics and spans for its own
rendering. The actionable budgets are:

| Navigation | Budget | Violation signal |
| --- | ---: | --- |
| Initial dashboard settlement | 8 s | `monoscope.dashboard.time_to_settled.budget_exceeded{navigation="initial"}` |
| HTMX tab settlement | 1.5 s | `monoscope.dashboard.time_to_settled.budget_exceeded{navigation="tab"}` |

Alert when either violation signal is non-zero over ten minutes. Investigate its
companion distributions before changing a budget:

- `monoscope.dashboard.shell.duration` separates shell work by navigation kind.
- `monoscope.dashboard.widget.duration` separates widget kind and server/client
  prefill mode.
- `monoscope.dashboard.query.duration` and
  `monoscope.dashboard.query.cache_outcomes` distinguish query cost from cache
  misses.
- `monoscope.dashboard.response.size` and
  `monoscope.dashboard.time_to_settled` are measured by the browser after grid
  hydration.

Dashboard and project IDs remain trace attributes, not metric dimensions. TimeFusion
emits real `bytes_scanned` and decode-byte telemetry at the engine boundary (rather
than in the pgwire result). Correlate that engine telemetry with the dashboard query
span when a query-duration regression occurs. The PostgreSQL protocol exposes no
per-query scan-statistics field, so the dashboard never substitutes SQL text length,
response size, or an estimate for scanned bytes.
