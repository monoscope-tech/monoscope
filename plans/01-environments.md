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
- Persisted in an `env` cookie so it survives navigation, reload and a new tab — Datadog's
  behaviour.
- "All environments" is a real option and the default. An env with no data must still be
  selectable (otherwise switching to staging when staging is quiet looks broken).

**Changed during implementation.** The plan said a `?env=` query param would override the
cookie so a shared link carried its environment. Dropped: to be sticky at all, the parameter
would have to be declared on every route and threaded onto every link in the app — the
selection has to survive navigation to pages that never heard of it. The cookie alone does
that, and it rides on the session (read at auth time next to `theme` and `lang`), so every
handler that already resolves a session has it with no new plumbing. The cost is that a
shared link does not carry the environment; the query it links to still does, and a reader
can re-pick in one click.

The options come from the **learned facet values** (`SchemaCatalog.getFacetSummary`), not
the service-map rollup as planned — one indexed row read, it covers log-only projects that
have no service graph, and it is exactly the set the Log Explorer's facet sidebar offers.

### 3. Applying it

The selection is an implicit `AND` on the query, injected server-side at the query-building
boundary rather than textually into the user's KQL — the user's query box must keep showing
what the user typed.

The injection point is `SqlQueryCfg.environment` → `NormalizedQuery.nqEnvironment`, which
joins `project_id` and the date range in `buildWhere`. That covers **every** shape
`sqlFromQueryComponents` emits — data, summarize and the monitor alert query — from one
place, so a monitor scoped to staging cannot silently alert on prod.

Surfaces wired:

1. **Log explorer** — list, patterns, and (via `scopedQueryWhere`) sessions and event
   examples. The env travels on the session, so `logDataEnv` hands it to every data handler.
2. **Service map** — already has its own env facet backed by the
   `service_dependency_edges_env` rollup.

Still unscoped, and deliberately so for this pass: dashboards/charts (`Pages.Charts`) and
the v1 events API (`queryEvents` is API-key auth — there is no browser session and so no
sticky selection to read). Both keep working unscoped; the selector is additive.

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
