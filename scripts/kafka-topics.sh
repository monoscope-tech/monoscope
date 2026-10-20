#!/usr/bin/env bash
# Converge broker-side config for topics monoscope owns. Idempotent: creates a missing topic,
# otherwise applies `alter-config --set` (incremental — keys not listed here are untouched).
# The partition count only applies on create; an existing topic keeps its own.
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
  # Read only the three keys, as data — never source or eval .env.
  while IFS='=' read -r key value; do
    value=${value%[\"\']}; value=${value#[\"\']}
    case "$key" in KAFKA_BROKERS | KAFKA_USERNAME | KAFKA_PASSWORD) export "$key=$value" ;; esac
  done < .env
fi
: "${KAFKA_BROKERS:?KAFKA_BROKERS not set}"
auth=()
[ -n "${KAFKA_USERNAME:-}" ] && auth=(-X "user=$KAFKA_USERNAME" -X "pass=${KAFKA_PASSWORD:-}" -X sasl.mechanism=SCRAM-SHA-256)
# ${auth[@]+…}: bash 3.2 (macOS /bin/bash) treats an empty array as unbound under set -u.
rpk_() { rpk -X "brokers=$KAFKA_BROKERS" ${auth[@]+"${auth[@]}"} "$@"; }

converge() { # <topic> <partitions> <key=value>...
  local topic=$1 partitions=$2 kv set=() create=()
  shift 2
  for kv in "$@"; do set+=(--set "$kv"); create+=(-c "$kv"); done
  if rpk_ topic describe "$topic" >/dev/null 2>&1; then
    rpk_ topic alter-config "$topic" "${set[@]}"
  else
    rpk_ topic create "$topic" -p "$partitions" "${create[@]}"
  fi
}

converge live_tail 6 retention.ms=600000 retention.bytes=67108864 segment.ms=600000 segment.bytes=16777216 cleanup.policy=delete
