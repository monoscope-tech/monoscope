# 01 — Environments (prod / staging / …), Datadog-style

## Goal

`deployment.environment.name` becomes a first-class dimension: a real TimeFusion column, a
facet, and a **sticky, app-wide selector** in the page shell that scopes every telemetry
surface until it is changed — the way Datadog's `env` selector works.

## Current state

- **Metrics** already promote it: `otel_metrics.resource___deployment___environment___name`
  (`Telemetry.hs:2555`, populated at `:2582`).
- **Spans/logs do not.** `otelColumns` (`Telemetry.hs:1674`) promotes
  `service.name`/`version`/`instance.id`/`namespace` and the telemetry SDK triple, but
  **not** `deployment.environment.name`. Today the value is only reachable inside the
  `resource` Variant blob.
- The service map already has an env facet, backed by its own rollup table
  (`0118_service_dependency_edges_env.sql`) — precisely because the span table has no such
  column. Commit `20506f422` is the tombstone of an earlier attempt: *"Revert environment
  facet: it queried a column Postgres does not have."* This task is what makes that column
  exist.
- TimeFusion's schema is YAML-driven (`timefusion/schemas/otel_logs_and_spans.yaml`) and
  supports additive nullable evolution (`SchemaMode::Merge`, `database.rs:5526`), so adding
  a column is a config change, not a rewrite.

## Design

### 1. The column

`resource___deployment___environment___name`, `Utf8`, nullable — the same name the metrics
table already uses, so one KQL field name resolves on both.

- **TF**: add to `schemas/otel_logs_and_spans.yaml` `columns:`. **Not** a sorting column:
  the sort key list is load-bearing for TopK pushdown (see the header comment in that file)
  and env is low-cardinality — a filter on it prunes badly by sort order and well by
  row-group stats, which it gets for free.
- **PG**: new append-only migration adding the nullable column to `otel_logs_and_spans`.
- **Ingest**: one `resourceText` entry in `otelColumns`. That single list drives the insert,
  the read projection and the `FromRow` order, so the read path follows automatically.

Backfill is deliberately **not** attempted: the value is still in the `resource` Variant for
old rows, and rewriting history in TF is the operation that has repeatedly OOM-killed it.
Old rows read as "no environment", which the selector treats as *unset*, not as *excluded*.

### 2. The selector

Sticky, per project, in the page shell:

- Rendered by `Pages.BodyWrapper` next to the time picker, so every page inherits it.
- Options come from the distinct environments seen in range. Reuse the service map's
  existing env source (`service_dependency_edges.environment`) rather than a
  `SELECT DISTINCT` over the span table — that is the cheap, already-maintained rollup, and
  it is exactly the set of environments that produced traffic.
- Persisted in a cookie (`env_<projectId>`) so it survives navigation, reload and a new tab
  — Datadog's behaviour. A `?env=` query param overrides the cookie for one request, so a
  shared link carries its environment.
- "All environments" is a real option and the default. An env with no data must still be
  selectable (otherwise switching to staging when staging is quiet looks broken).

### 3. Applying it

The selection is an implicit `AND` on the query, injected server-side at the query-building
boundary rather than textually into the user's KQL — the user's query box must keep showing
what the user typed.

Surfaces, in priority order:

1. Log explorer (list, patterns, chart/summary widgets) — `LogQueries.selectLogTable` and
   `fetchLogPatterns` take the env alongside the existing time range.
2. Service map — already has a facet; it becomes the *shell* selector instead of a
   page-local one.
3. Dashboards / charts — `Pages.Charts` query cfg.

Anything not yet wired keeps working unscoped; the selector is additive.

## Verification

- Doctest the cookie/param precedence (`?env=` beats cookie beats "all") and the
  empty-string-is-all-environments rule (`nonEmptyT`, already used by the service map).
- Integration test: ingest two spans with different `deployment.environment.name`, assert
  the log explorer returns both with no selection and one with each selection.
- Integration test that the promoted column is actually populated from the resource map.

## Risk

The TF schema edit lands in the `timefusion` repo and needs a TF deploy before the column
is readable in prod. The monoscope side must therefore tolerate the column being absent —
which it does, because the env filter is only emitted when an environment is selected, and
nothing is selected by default.
