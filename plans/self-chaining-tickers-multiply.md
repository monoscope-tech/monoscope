# Self-chaining tickers multiply, and the job queue is now the biggest DB cost

## Symptom

`pg_stat_statements` on prod, top query by total time — by 38x:

| total time | calls | mean | query |
|---|---|---|---|
| **90,156s** | 3,218,573 | **28ms** | odd-jobs' lock: `update background_jobs set status…where id in (select id … order by run_at, attempts limit ? for update)` |
| 2,340s | 4,368,282 | 1ms | `INSERT INTO apis.log_patterns …` |

A single-row-ish update averaging 28ms is the tell. `background_jobs` holds **96,770 rows
(311 MB)**, of which **69,787 are queued** — and 60,162 of those are one job type.

## Root cause

Three handlers are self-chaining tickers (`BackgroundJobs.hs:313-322`):

```haskell
NotificationSweepJob scheduledTime -> do
  -- Re-enqueue first so a mid-tick failure still produces a next tick.
  rescheduleSelf authCtx Jobs.NotificationSweepJob (addUTCTime 600 scheduledTime)
  unlessStale "NotificationSweepJob" scheduledTime 1800 $ runNotificationSweep scheduledTime
```

`unlessStale` skips **the work**, not **the reschedule** — which runs first, unconditionally.
So a stale duplicate does nothing useful and still spawns a successor: a chain can never die.

The same types are *also* pre-seeded at startup (`BackgroundJobs.hs:813-820`), a safety net so
"a single restart can't leave a gap when the chain broke":

```haskell
seedJobs conn currentTime 24 3600 Jobs.InfraHealthCheck
```

Seeding N future ticks while every tick also self-chains creates **N overlapping immortal
chains**, and every restart adds N more.

The comparison is clean — the three self-chaining types are inflated, the three that are only
seeded sit at their seed count:

| job | seeded | queued | |
|---|---|---|---|
| InfraHealthCheck | 24 | **60,162** | self-chains |
| NotificationSweepJob | 144 | **1,358** | self-chains |
| NotificationDigestJob | 24 | **226** | self-chains |
| QueryMonitorsCheck | 1440 | 1,203 | seed only ✓ |
| PrometheusScrapeTick | 1440 | 1,203 | seed only ✓ |
| ProcessIssuesEnhancement | 24 | 20 | seed only ✓ |

Distribution confirms compounding rather than a stuck backlog: 60,162 InfraHealthCheck jobs
across **30 distinct `run_at` slots** — 37,337 in one hour, 22,807 in the previous, and exactly
**1** in every future slot. Created 22,880 → 37,262 over consecutive hours (~1.6x/hour).

## Why it matters

Every poll of the lock query orders a candidate set containing tens of thousands of past-due
rows, under `FOR UPDATE` (not `SKIP LOCKED`). With only 15 rows `locked` at a time the queue is
not draining, so the set keeps growing, so each poll gets slower.

## Fix directions (not applied — needs a decision)

1. **Reschedule only when not stale** — move `rescheduleSelf` inside `unlessStale`. A stale
   duplicate then dies instead of spawning. Keeps the crash-safety intent for live ticks.
   Loses it for the case the comment names (a tick that fails *after* the staleness window).
2. **Stop seeding self-chaining types.** Seeding exists to repair a broken chain; a chain that
   cannot break does not need it. Needs something to restart a chain that genuinely dies.
3. **Dedupe on enqueue** — unique on `(tag, run_at)` so overlapping chains collapse. Most
   robust, needs a migration and an `ON CONFLICT DO NOTHING` at the enqueue site.

Independently worth doing: a composite index on `(status, run_at, attempts)` for the poll, and
`SKIP LOCKED` if odd-jobs can be configured for it.

## Clearing the existing backlog

Deleting ~60k past-due duplicate rows is a destructive prod operation and is deliberately left
undone. Note the rows are *not* junk — each is a real scheduled tick; the duplicates are.
Keep one per `(tag, run_at)`.

## Re-check

```sql
select payload->>'tag', count(*) from background_jobs where status='queued' group by 1 order by 2 desc;
select count(distinct run_at), count(*) from background_jobs
  where status='queued' and payload->>'tag'='InfraHealthCheck';
select round(total_exec_time/1000)||'s', calls, round(mean_exec_time)||'ms'
  from pg_stat_statements order by total_exec_time desc limit 3;
```
