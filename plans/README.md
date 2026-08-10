# Overnight work plan — 2026-08-10

Master todo. One plan document per task; tick items here as each lands.

| # | Task | Plan | Status |
|---|------|------|--------|
| 1 | Environment switcher (prod/staging/…) — `deployment.environment.name` promoted to a first-class TimeFusion column + sticky app-wide selector, Datadog-style | [01-environments.md](01-environments.md) | **done** (`f78051166`) — ⚠ TF schema hunk left UNCOMMITTED, see below |
| 2 | Service map: fix prod render (hyperscript parse error + null deref), fix the node context menu (`viz_type=traces` is not a viz type; logs/traces are one surface here) | [02-service-map.md](02-service-map.md) | **done** (`4615b0cc1`) |
| 3 | Log-list latency column: break down by service and/or kind, rethink the child-row UX | [03-latency-column.md](03-latency-column.md) | **done** (`ab48f9c10`) |
| 4 | Error-in-source-context (Sentry/Datadog parity): upload source, link to a service, render the failing frame with surrounding lines | [04-code-context.md](04-code-context.md) | **done** |
| 5 | Log-item detail: show the full session/tenant facet set (tenant id, user id, …), not just email | [05-session-tenant-fields.md](05-session-tenant-fields.md) | **done** (`cc0c346bb`) |

## Working rules for this run

- Verify compiles by reading `build.log` (ghcid), never `cabal build`.
- Verify tests by reading `build-test-dev.log` (`make live-test-dev`), never `cabal test`.
- Migrations are append-only.
- Run `/hs-distill`, `/hs-lob-review`, `/hs-evasion-review`, `/code-review` before each commit.
- Commit per task (or per coherent sub-step), no Claude co-authorship.

## ⚠ Needs your hand in the morning

**`timefusion/schemas/otel_logs_and_spans.yaml` is edited but NOT committed.** Task 1 adds
`resource___deployment___environment___name` there. I left it uncommitted on purpose: that
working tree already held someone else's in-progress rollup work (`src/rollup.rs`,
`src/schema_loader.rs`, `src/database.rs`, `schemas/otel_metrics.yaml`), and committing
would have swept it up. My hunk is self-contained — the `- name:
resource___deployment___environment___name` block after `resource___service___namespace`.

Nothing breaks while it waits. The monoscope side only emits the environment predicate when
an environment is selected, and nothing is selected by default; TimeFusion evolves into the
column additively (`SchemaMode::Merge`) once it deploys. Postgres already has it via
migration 0122.
