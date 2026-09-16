# Recurring jobs: prior art and a redesign

Follow-up to `plans/self-chaining-tickers-multiply.md` (the 60k-row runaway). That note
diagnoses; this one surveys how the industry schedules recurring work and proposes the
replacement architecture.

## What we do today, restated as architecture

- **Recurring work is pre-materialized as job rows.** A daily seeder creates every tick for
  the day up front: 24 `HourlyJob`, 24 `ProcessIssuesEnhancement`, 1440 `QueryMonitorsCheck`,
  1440 `PrometheusScrapeTick`, 288 `ServiceMapRollupTick`, 144 `NotificationSweepJob`,
  24 `NotificationDigestJob`, 24 `InfraHealthCheck` — **~3,400 ticker rows/day** before any
  real work.
- **Three types additionally self-chain** (handler re-enqueues its successor before the
  stale guard), so seeding × chaining = N immortal overlapping chains. Worse: an on-time
  duplicate is *not stale*, so both the chained tick and its seeded twin run, do the work
  twice, and each spawns a successor — the population doubles per period regardless of the
  stale guard. Moving `rescheduleSelf` inside `unlessStale` therefore does NOT fix it;
  only removing the chain (seed-only, like the healthy types) or the unique index does.
- **No dedupe anywhere**: no unique key, no leader, no idempotent insert. Two mechanisms
  writing the same logical tick cannot collapse into one row.
- **The queue drains one job per poll**: odd-jobs' poller takes a single row per round trip
  (`jobPollingSql` … `LIMIT 1 FOR UPDATE`, and the Haskell side errors on >1 row), no
  `SKIP LOCKED`. Cost observed in prod: the lock UPDATE at 3.2M calls / 28ms mean /
  90,156s total — 38× the next statement.

## Prior art

| System | Schedule lives in | Job rows materialized | Duplicate prevention | Missed runs after downtime | Fetch |
|---|---|---|---|---|---|
| **Quartz (JDBC store)** | `QRTZ_TRIGGERS`, one row per trigger holding `NEXT_FIRE_TIME` | never ahead of time; the trigger row is updated in place after each fire | row lock on the trigger row at acquire | explicit misfire policies per trigger | `acquireNextTriggers` takes a batch of N |
| **Oban (Elixir, OSS)** | in-code crontab, loaded at boot | inserted at the fire minute | only the cluster **leader** inserts (`Peer.leader?`) | none in OSS — resumes next minute (Pro sells guarantees) | per-queue batched fetch* |
| **River (Go)** | in-memory `PeriodicJob` on the client | inserted at fire time by the elected leader | leader election, optionally + unique jobs per period | `RunOnStart` on leader change; Pro persists schedules | batched `SKIP LOCKED` |
| **graphile-worker (Node)** | crontab file + `known_crontabs` table remembering last run | inserted when due | ACID insert race on `known_crontabs`; idempotent `job_key` | explicit backfill window (`fill=2d`), jobs get a `backfilled` flag | `LISTEN/NOTIFY` + batched `SKIP LOCKED` |
| **solid_queue (Rails)** | `config/recurring.yml` | enqueued at fire time; “each task schedules the next one” | **unique index on `(task_key, run_at)`** in `recurring_executions`, written in the same txn as the job | forward-looking only | dispatcher batches, `SKIP LOCKED`* |
| **pg-boss (Node)** | a **schedule table** (rows, not config) | a cron pass computes occurrences due in a 60s window and forwards them | **singleton key per (schedule, occurrence)** + 60s throttle slot; unique constraint collapses racing instances | per-schedule **missed policies** | batched fetch, `SKIP LOCKED` |
| **monoscope today** | nowhere — seeding code + handler chains | a full day ahead, per type | none | a day of seeded rows | 1 row/poll, `FOR UPDATE`, no `SKIP LOCKED` |

\* Fetch-column entries for Oban/River/solid_queue are from general knowledge of those
codebases, not tonight's fetched sources; schedule/dedupe/missed-run columns are sourced.

The consensus is total. Every system, whatever its ecosystem:

1. **The schedule is data (or config) — exactly one pending "next fire" per schedule.**
   Nobody materializes a day of ticks. Quartz updates one row in place; the rest insert at
   fire time.
2. **Duplicates are made impossible, not avoided by convention** — either a single writer
   (leader) or a unique key on `(schedule, occurrence)` with `ON CONFLICT DO NOTHING`.
   Systems that only have leadership (Oban OSS, River OSS) document skipped/duplicated
   ticks around elections as a known weakness; the DB-unique systems don't have it.
3. **Missed runs are an explicit policy** (Quartz misfire, pg-boss missed policies,
   graphile `fill`), not an accident of how many rows happen to be lying around.
4. **Fetching is batched over `SKIP LOCKED`.** One row per `FOR UPDATE` poll is the
   outlier; it serializes pollers against each other and multiplies round trips.

Our design is the one nobody ships: pre-materialization **and** self-chaining **and** no
dedupe **and** unbatched locking.

## Proposed design

Keep odd-jobs as the *executor* (retries, payloads, handlers unchanged — deleting seeded
`BgJobs` constructors breaks CI shards, so constructors and payload shapes stay). Replace
only how recurring ticks *enter* the queue, with the solid_queue/pg-boss shape — DB-unique
claim, no leader infrastructure:

### 1. A schedule table (~10 rows, replacing ~3,400 seeded rows/day)

```sql
CREATE TABLE background_job_schedules (
  tag             text PRIMARY KEY,          -- 'InfraHealthCheck', …
  every_seconds   int  NOT NULL,
  next_run_at     timestamptz NOT NULL,
  -- pg-boss missed policies: 'one' collapses any downtime into a single catch-up tick
  catchup         text NOT NULL DEFAULT 'one' CHECK (catchup IN ('skip','one','all')),
  enabled         boolean NOT NULL DEFAULT true
);
```

### 2. One scheduler tick, multi-replica-safe by row claim (not leadership)

Every 30–60s (the existing `ensureDailyJobScheduled` async loop, repointed):

```sql
-- claim: whichever replica locks the row owns materializing that occurrence.
-- CTE self-join because RETURNING sees the post-UPDATE value on pg16 — the naive
-- form would return the NEXT occurrence and poison the dedupe key.
WITH due AS (
  SELECT tag, next_run_at FROM background_job_schedules
  WHERE enabled AND next_run_at <= now()
  FOR UPDATE SKIP LOCKED
)
UPDATE background_job_schedules s
SET next_run_at = CASE s.catchup
      WHEN 'all' THEN due.next_run_at + s.every_seconds * interval '1 second'
      ELSE now() + s.every_seconds * interval '1 second'
    END
FROM due WHERE s.tag = due.tag
RETURNING s.tag, due.next_run_at AS fired_for;
```

The claimant inserts the corresponding `background_jobs` row (`run_at = fired_for`) **in
the same transaction**. Postgres row locking is our `Peer.leader?` — no election, no
config-designated scheduler instance to keep alive. `SKIP LOCKED` keeps concurrent
scheduler passes from queueing behind each other.

`'skip'` vs `'one'` is decided by the claimant, not the UPDATE (both jump `next_run_at`
to the future; `'skip'` simply doesn't insert when `fired_for` is older than one period).
`'all'` as written drains one missed period per scheduler pass — fine for hourly ticks,
hours of catch-up for a 60s cadence after a long outage; no current job wants 'all'.

### 3. Belt and braces: uniqueness on the occurrence

solid_queue's `recurring_executions` equivalent, directly on the jobs table:

```sql
CREATE UNIQUE INDEX background_jobs_one_tick
  ON background_jobs ((payload->>'tag'), run_at)
  WHERE status IN ('queued','locked') AND payload->>'tag' IN (…recurring tags…);
```

Insert with `ON CONFLICT DO NOTHING`. Even a buggy second writer (deploy overlap, manual
enqueue, the old code path during rollout) collapses into the existing tick instead of
multiplying. This single constraint would have made tonight's runaway structurally
impossible.

### 4. Delete both old mechanisms

- `seedJobs` and its eight call sites; the `HourlyJob` 24-row loop becomes a schedule row.
- `rescheduleSelf` from the three chaining handlers. `unlessStale` stays as a cheap guard
  during rollout, then dies too — `catchup='one'` is its principled replacement.
- `ensureDailyJobScheduled` keeps only the genuinely-daily one-offs (`MonoscopeAdminDaily`
  etc.), which also become schedule rows with `every_seconds = 86400`.

### 5. Queue mechanics (independent, do regardless)

- Partial index for the poll: `(run_at, attempts) WHERE status IN ('queued','retry')`.
- odd-jobs upstream: patch or PR `SKIP LOCKED` + batch fetch; with the ticker flood gone
  this is much less urgent, but 1-row-per-poll × N pollers is still the outlier pattern.

## Expected effect

| | today | after |
|---|---|---|
| rows representing future recurring work | ~3,400/day seeded (+runaway: 60k and compounding) | ~10 schedule rows, ≤1 pending tick each |
| duplicate ticks possible? | unbounded | unique-key impossible |
| restart behaviour | +N seeded rows per restart, chains multiply | no effect; `catchup='one'` runs at most one catch-up |
| lock query candidate set | tens of thousands | ≈ in-flight + due (tens) |
| top prod statement | 90,156s / 28ms mean | expected sub-ms mean; no longer top |

## Rollout order

1. Migration: table + unique index; seed schedule rows from the current cadences.
2. Scheduler tick lands in the same deploy that turns the old seeding off. The unique
   index does NOT make running both safe: `seedJobs` phases ticks off seed time, the
   schedule table off `next_run_at` — different phases, different `run_at`, nothing
   collapses. The index guards against same-occurrence races, not different-phase ones.
3. Remove `rescheduleSelf` calls + `seedJobs`. (Constructors stay.)
4. One-time backlog dedupe: `DELETE … keeping min(id) per (tag, run_at)` on the ~60k
   queued dupes — destructive, run by a human, SQL in the incident note.
5. Drop `unlessStale` once a full day looks clean.

Sources: [Oban.Cron source](https://github.com/oban-bg/oban/blob/main/lib/oban/cron.ex) ·
[River periodic jobs](https://riverqueue.com/docs/periodic-jobs) ·
[graphile-worker cron](https://github.com/graphile/worker/blob/main/website/docs/cron.md) ·
[solid_queue recurring](https://github.com/rails/solid_queue#recurring-tasks) ·
[pg-boss timekeeper](https://github.com/timgit/pg-boss/blob/master/src/timekeeper.ts) ·
[Quartz JobStoreSupport](https://www.quartz-scheduler.org/api/2.3.0/org/quartz/impl/jdbcjobstore/JobStoreSupport.html)
