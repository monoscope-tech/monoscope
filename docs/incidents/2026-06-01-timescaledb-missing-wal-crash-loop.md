# Incident: TimescaleDB crash loop on missing WAL segments

- **Date:** 2026-06-01
- **Severity:** SEV-1 (full production DB outage)
- **Detected:** ~12:00 UTC
- **Resolved:** 12:11:40 UTC
- **Last clean DB state:** 09:34:53 UTC
- **Total impact window:** ~2h 35m (from last clean state to recovery)
- **Active mitigation time:** ~10 min once root cause identified
- **Service:** `srv-captain--timescaledb` (monoscope primary Postgres + TimescaleDB, pg18)
- **Author:** Anthony Alaribe

## Summary

The primary TimescaleDB container entered a crash loop, exiting every 5–10s with `PANIC: could not find redo location 5423/A265CA98 referenced by checkpoint record at 5424/52A51458`. The WAL segments containing the redo location were missing from local `pg_wal/`, and Postgres's plain crash recovery does not invoke `restore_command`, so the configured pgbackrest archive recovery was never attempted automatically. We unblocked Postgres by dropping a `recovery.signal` file into PGDATA, which forced archive recovery — Postgres then pulled every missing WAL segment from the R2 pgbackrest archive, replayed to end-of-archive, and promoted to timeline 5.

## Impact

- All ingestion (OTLP gRPC :4317, Pub/Sub, Kafka) failing on DB writes for the duration.
- Monoscope UI was effectively down for the duration (read paths fail without DB).
- Background jobs (odd-jobs) repeatedly failed to acquire locks during the read-only recovery window.
- No data loss inside the recovered range. Bounded data loss between end-of-archive (`5424/5D`) and the actual crash LSN — typically seconds with `archive-async=y`, but exact bound depends on archive-push lag at the moment of the dirty shutdown.

## Timeline (UTC)

| Time | Event |
|------|-------|
| 09:34:53 | Last clean DB state per `pg_control` (`last known up at`). |
| ~11:5x | Container started crash-looping. Trigger of the dirty shutdown not yet identified. |
| 12:02 | First investigated logs show `PANIC: could not find redo location 5423/A265CA98`. |
| ~12:04 | Confirmed local `pg_wal/` earliest segment is `0000000400005423000000AC`; segments `A2..AB` missing. |
| ~12:06 | Verified pgbackrest archive on R2 contains the missing segments (archive max `00000004000054240000005D`). |
| ~12:07 | First attempt: scaled service to 0, copied `A2..AB` from R2 directly into `pg_wal/`, scaled back up. Crash recovery advanced from the panic to `5423/AC000058`, then failed: `xlog flush request 5423/D9037570 is not satisfied — flushed only to 5423/AC000058` (locally recycled segments past `AC` contained zeros — postgres saw an FSM page on disk with a later LSN than any WAL it could read). |
| ~12:10 | Diagnosed the real gotcha: `restore_command` is **only used in archive recovery**, not in crash recovery. Created `recovery.signal` to force archive recovery. |
| 12:11:06 | Postgres recovery began pulling segments from R2 via `pgbackrest archive-get`. |
| 12:11:38 | End-of-recovery checkpoint at LSN `5424/5E000028`; promotion to timeline 5. |
| 12:11:40 | `database system is ready to accept connections`. TimescaleDB background workers connected; continuous aggregate refreshes resumed. |

## Root cause

Two compounding factors:

1. **WAL loss on local disk.** Postgres restarted dirty with `pg_control` referencing a redo location (`5423/A265CA98`) for which the corresponding WAL segments (`A2..AB`) were no longer present in `pg_wal/`. This is the same family of failure as the 2026-05-30 archive-async data-loss incident: pgbackrest's `archive-async=y` marks segments `.done` before R2 confirms in some failure modes, and Postgres then recycles them locally. (The trigger of *this* dirty shutdown is still under investigation — see Followups.)
2. **`restore_command` ignored during crash recovery.** Postgres only consults `restore_command` when in archive recovery mode (`recovery.signal` or `standby.signal` present). Without a signal file, plain crash recovery only reads from local `pg_wal/`. So even with a perfectly healthy `restore_command` and a pgbackrest archive containing the missing segments, Postgres just panicked.

The result: an outage that *looks* like data corruption (`PANIC: could not find redo location`) but is actually just a configuration / recovery-mode mismatch — fixable in minutes once the mechanism is understood.

## Resolution

```bash
# 1. Stop the crash loop.
docker service scale srv-captain--timescaledb=0

# 2. Verify recovery config is sane:
#    - restore_command present
#    - recovery_target_action = 'promote'
#    - recovery_target_time NOT set (else promotion would wait for an unreachable time)
sudo grep -E 'recovery_target|restore_command' /home/ubuntu/timescaledb-data/data/postgresql.auto.conf

# 3. Force archive recovery on next start.
sudo touch /home/ubuntu/timescaledb-data/data/recovery.signal
sudo chown 1000:1000 /home/ubuntu/timescaledb-data/data/recovery.signal
sudo chmod 600     /home/ubuntu/timescaledb-data/data/recovery.signal

# 4. Bring it back up. Postgres now:
#      - reads checkpoint from pg_control,
#      - replays from redo location, fetching each WAL segment from R2 via restore_command,
#      - hits "not found" past end-of-archive,
#      - promotes (timeline +1) because recovery_target_action='promote',
#      - removes recovery.signal automatically.
docker service scale srv-captain--timescaledb=1
```

A `pgbackrest` PITR was explicitly avoided — it would have required restoring the most recent full backup (~140 GB compressed → ~298 GB) plus serial WAL replay, an estimated 6+ hour outage. The `recovery.signal` approach reuses the existing on-disk data and only replays the WAL gap, completing in seconds of wall-clock recovery.

### Diagnostic trick

Because the live service was crash-looping, we couldn't `docker exec` into it to run `pgbackrest`. Workaround was a one-off container with the same image and volumes:

```bash
docker run --rm \
  -v /home/ubuntu/timescaledb-data:/home/postgres/pgdata \
  -v /home/ubuntu/pgbackrest-config/pgbackrest.conf:/etc/pgbackrest/pgbackrest.conf:ro \
  --user 0 --entrypoint /bin/bash timescale/timescaledb-ha:pg18-all-amd64 \
  -c 'pgbackrest --config=/etc/pgbackrest/pgbackrest.conf --stanza=db-prod info'
```

Same pattern can `archive-get` individual segments directly into `pg_wal/` if there's a strong reason to keep the current timeline (no promotion). On this incident we tried that first and it surfaced the deeper "page LSN ahead of WAL" issue, which is what pushed us to `recovery.signal`.

## What went right

- pgbackrest archive on R2 was intact and contained every WAL segment we needed.
- Memory notes from the 2026-05-30 archive-async incident already documented `recovery_target_action='promote'` and the "remove `recovery_target_time`" trick — the auto.conf was already correctly configured for archive recovery, all we needed was the signal file.
- Disk was not full (1.1 TB free) so there was no secondary fire to fight.

## What went wrong

- **Restore_command vs. crash recovery semantics were not obvious.** First 30+ minutes of investigation were spent assuming Postgres would auto-fetch missing WAL because `restore_command` was set. Documenting this in the runbook (and saving as memory) is the highest-leverage learning from this incident.
- **Initial fix attempt (copy A2..AB into pg_wal) was incomplete.** It advanced the panic but didn't resolve it because Postgres also had on-disk pages with LSNs past local WAL — only archive recovery, which can fetch *all* needed segments, could resolve that. Lost ~5 minutes on the wrong fix.
- **The underlying dirty shutdown trigger is still unknown.** We restored the database but did not yet establish *why* the container exited dirty in the first place. Without that, we can't rule out recurrence.

## Followups

| # | Action | Owner | Priority |
|---|--------|-------|----------|
| 1 | Investigate root cause of the dirty shutdown (dmesg / OOM-killer / docker events / swarm task history around 11:50–12:00 UTC). | — | P0 |
| 2 | Add a runbook section documenting the `recovery.signal` recovery path for "missing redo location" panics. Saved to memory as `infra_20260601_missing_wal_crash_loop.md`; also worth surfacing in `docs/`. | — | P1 |
| 3 | Add an alert on TimescaleDB container restart count > 2/min — we caught this from the user, not from monitoring. | — | P1 |
| 4 | Revisit the archive-async durability gap. Same root family as 2026-05-30. Long-term fix is moving to 4h diffs and/or a hot standby (already tracked in `infra_backup_frequency_too_low`). | — | P1 |
| 5 | Verify all data within `5424/5D .. 5424/5E000028` ingested fine post-recovery, and quantify the seconds-of-data-loss bound on the producer side. | — | P2 |
| 6 | `VACUUM FULL apis.error_patterns` after recovery (per `infra_20260530_archive_replay_toast_bloat`) — archive replay re-creates TOAST bloat. | — | P2 |

## Lessons / save-to-runbook

1. **`PANIC: could not find redo location` + healthy pgbackrest archive ⇒ drop `recovery.signal`, not pg_resetwal, not PITR.**
2. **Crash recovery does not use `restore_command`. Archive recovery does.** Configuration alone is not enough — the signal file is what activates the code path.
3. **`recovery_target_action = 'promote'` + no `recovery_target_time` ⇒ replay all available archive then promote.** Bounded data loss = end-of-archive lag.
4. **The one-off-container pgbackrest pattern is the universal escape hatch** when the primary service is crash-looping and can't be `docker exec`'d.
