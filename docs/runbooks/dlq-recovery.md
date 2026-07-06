# Runbook: DLQ / TimeFusion write-loss recovery

How to detect and recover from the failure modes behind the 2026-07-06 silent
dual-write loss: dead consumers, a growing dead-letter/parking backlog, and
TimeFusion falling behind TimescaleDB. The prevention side of this (the hourly
`InfraHealthCheck` watchdog) lives in `src/BackgroundJobs.hs`; this document is
the response side.

All Kafka commands use `rpk`. Brokers and SASL creds come from the monoscope
`.env` (`KAFKA_BROKERS`, `KAFKA_USERNAME`, `KAFKA_PASSWORD`) — never hard-code
them. Export once:

```bash
export RPK_BROKERS="$(grep -m1 '^KAFKA_BROKERS=' .env | cut -d= -f2-)"
export RPK_USER="$(grep -m1 '^KAFKA_USERNAME=' .env | cut -d= -f2-)"
export RPK_PASS="$(grep -m1 '^KAFKA_PASSWORD=' .env | cut -d= -f2-)"
export RPK_SASL_MECHANISM=SCRAM-SHA-256
rpk() { command rpk --brokers "$RPK_BROKERS" --user "$RPK_USER" --password "$RPK_PASS" --sasl-mechanism "$RPK_SASL_MECHANISM" "$@"; }
```

Topic/group names (defaults): mainline group `apitoolkit_eu`, DLQ-replay group
`apitoolkit_eu_dlq`, base DLQ `otlp_deadletter`, retry tiers
`otlp_deadletter-retry-60s` / `-retry-600s`, terminal `otlp_deadletter-parking`.

## 1. How you'll know

`InfraHealthCheck` posts to the admin Discord webhook (hourly) on any of:

- `TF behind TS by …%` / `TF INGEST STALLED` — TimeFusion is losing writes.
- `consumer group … has 0 live members` — ingestion or DLQ replay is dead.
- `messages PARKED in the last 90m` — data is silently waiting for a human.
- `write-failures entered otlp_deadletter …` — a write leg is failing.

## 2. Diagnose

```bash
rpk group describe apitoolkit_eu apitoolkit_eu_dlq          # MEMBERS, LAG per partition
rpk topic describe otlp_deadletter otlp_deadletter-parking  # partitions, watermarks
# Retained backlog per topic (Σ high−low):
for t in otlp_deadletter otlp_deadletter-parking; do
  echo -n "$t backlog: "; rpk topic describe -p "$t" | awk 'NR>1{s+=$4-$3} END{print s}'
done
```

Note: **parking has no consumer group**, so `rpk group describe` will never show
it — you must inspect the topic directly (above). That invisibility is exactly
what let the incident hide.

## 3. Recovery A — dead / orphaned consumers (`0 members`)

Cause seen on 2026-07-06: `max.poll.interval.ms` too low + static membership
(`group.instance.id`) → an evicted static member never rejoins and squats its
partitions. Both are fixed in `src/Pkg/Queue.hs` (no static membership,
30-min poll interval). If a group is still stuck at 0 members with live pods:

```bash
# See which static instance IDs (if any) still hold partitions:
rpk group describe apitoolkit_eu_dlq
# Evict dead static members (only needed for pre-fix builds):
cd scripts/local/dlq-evict && go run . -user "$RPK_USER" -pass "$RPK_PASS" -instances <INSTANCE-ID,…>
```

If members are simply absent, restart the monoscope service so the consumers
rejoin (`docker service update --force srv-captain--monoscope` on the CapRover
host — this is a mutation; announce it).

## 4. Recovery B — re-drive the parking topic

Parking is terminal by design (nothing consumes it). To re-attempt those
messages after the underlying bug is fixed, enqueue the **`ReplayParkedMessages`**
background job — the sanctioned, in-app replacement for the old ad-hoc script.
It reads parking under a dedicated committing group (`apitoolkit_eu_parking_redrive`),
strips the exhausted retry-budget headers, republishes each to `otlp_deadletter`,
and self-reschedules until drained.

Enqueue one run (cap = messages per pass; it will chain until empty). Confirm
the column names first with `\d background_jobs`:

```sql
INSERT INTO background_jobs (run_at, status, payload, created_at, updated_at)
VALUES (now(), 'queued', '{"tag":"ReplayParkedMessages","contents":5000}', now(), now());
```

Watch progress via the `apitoolkit_eu_parking_redrive` group's LAG and the
`InfraHealthCheck` parking backlog line. It is idempotent at the row level
(deterministic UUIDv5 ids ⇒ TF dedup absorbs any double-writes).

## 5. Recovery C — bulk re-consume retained base-DLQ messages (offset rewind)

When messages were consumed-and-committed from `otlp_deadletter` during an
outage but never successfully written (retained in Kafka, past the group's
committed offset), rewind the DLQ-replay group to just before the incident and
let it re-consume. `rpk group seek` requires the group be **empty** (no active
members), so this needs a brief DLQ-consumer pause — mainline ingestion is
unaffected.

```bash
# 1. Pause ONLY the DLQ replay consumers (mainline keeps ingesting):
#    set ENABLE_KAFKA_DEAD_LETTER_SERVICE=false and redeploy monoscope,
#    OR scale the pods so the group drains to 0 members. Confirm:
rpk group describe apitoolkit_eu_dlq   # wait for MEMBERS: 0 / state Empty

# 2. Rewind the group's committed offsets to the incident start (epoch ms):
rpk group seek apitoolkit_eu_dlq --to-timestamp <START_EPOCH_MS> \
  --topics otlp_deadletter,otlp_deadletter-retry-60s,otlp_deadletter-retry-600s

# 3. Re-enable the DLQ service (revert step 1). The consumers rejoin and
#    re-process from the rewound offset through the fixed tiered-replay path.
```

Pick `START_EPOCH_MS` from the first `InfraHealthCheck`/parity alert (or the
first Discord 502/loss signal), minus a safety margin.

## 6. Verify recovery

Confirm TimeFusion caught up to TimescaleDB for the affected window with the
parity harness in the **timefusion** repo (`scripts/tf_vs_ts/counts_matrix.sh`),
or wait for the next `InfraHealthCheck` to report `parity: TS=… TF=…` with no
drift line. A settled window must match within the 1% floor.
