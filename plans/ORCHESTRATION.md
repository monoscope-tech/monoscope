# Parallel workstream orchestration (started 2026-08-25)

Baseline commit: `c71ec614f`. Master is 7 commits ahead of origin — **not pushed = not deployed**.

## Streams

| Stream | Branch | Location | Migration no. | Status |
|---|---|---|---|---|
| Dashboards fix + e2e | `master` | main checkout | none | in progress |
| Billing / new pricing | `ws-billing` | `.claude/worktrees/ws-billing` | 0136 | in progress |
| Container monitoring | `ws-containers` | `.claude/worktrees/ws-containers` | 0137 | in progress |
| Metric exemplars | `ws-exemplars` | `.claude/worktrees/ws-exemplars` | 0138 | in progress |

## Rules every stream follows

1. **`.env` `DATABASE_URL` points at PRODUCTION.** Never run `monoscope-server`, `cabal run`,
   or anything that starts the app from a worktree — startup runs migrations against prod.
2. **Never `git push`.** Pushing master is a ~28 min production deploy. The orchestrator
   decides when a coherent state gets pushed.
3. Sharing between streams is by **local** merge into master; worktrees share the object DB.
4. Migration filenames are pre-assigned above to avoid the historical `0125` collision.
   Migrations are append-only and must be additive/safe.
5. Main checkout verifies compilation by reading `build.log` (ghcid watcher). Worktrees have no
   watcher, so they verify with `cabal build lib:monoscope --ghc-options="-O0 -j8"` in the
   worktree — never in the main checkout.
6. `e2e/` is owned exclusively by the dashboards stream.

## Pricing target (billing stream)

- $1 per 1,000,000 events (spans + logs) — existing `events_usage` meter.
- $1 per 10,000,000 metric datapoints — new meter, currently recorded but unsubmitted.
- $1 per 1,000 session replays (RUM) — new meter, no counting source yet.
