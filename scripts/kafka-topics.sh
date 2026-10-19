#!/usr/bin/env bash
# Converge broker-side config for topics monoscope owns. Idempotent: creates a missing topic,
# otherwise applies `alter-config --set` (incremental — keys not listed here are untouched).
#
# live_tail: retention can only delete CLOSED segments, so retention.* alone is not enough —
# with the broker defaults (segment.ms=14d, segment.bytes=128MiB > retention.bytes) the active
# segment never rolls and rows for tails closed days ago stay forever (2026-10-19: oldest was
# 9.3 days). segment.ms=10min is Redpanda's floor (log_segment_ms_min), so a row lives <= ~20min.
#
# Credentials: KAFKA_BROKERS / KAFKA_USERNAME / KAFKA_PASSWORD from the environment or .env.
set -euo pipefail
cd "$(dirname "$0")/.."
if [ -z "${KAFKA_BROKERS:-}" ] && [ -f .env ]; then
  set -a; eval "$(grep -E '^KAFKA_(BROKERS|USERNAME|PASSWORD)=' .env)"; set +a
fi
: "${KAFKA_BROKERS:?KAFKA_BROKERS not set}"
rpk_() {
  rpk -X brokers="$KAFKA_BROKERS" ${KAFKA_USERNAME:+-X user="$KAFKA_USERNAME" -X pass="$KAFKA_PASSWORD" -X sasl.mechanism=SCRAM-SHA-256} "$@"
}

converge() { # <topic> <partitions> <key=value>...
  local topic=$1 partitions=$2; shift 2
  if rpk_ topic describe "$topic" >/dev/null 2>&1; then
    rpk_ topic alter-config "$topic" $(printf -- '--set %s ' "$@")
  else
    rpk_ topic create "$topic" -p "$partitions" $(printf -- '-c %s ' "$@")
  fi
}

converge live_tail 6 retention.ms=600000 retention.bytes=67108864 segment.ms=600000 segment.bytes=16777216 cleanup.policy=delete
