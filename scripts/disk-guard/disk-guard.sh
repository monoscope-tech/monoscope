#!/usr/bin/env bash
# Keeps captain's root disk from filling. Postgres, Redpanda, TimeFusion,
# CapRover and our own alerting all share it and all die on ENOSPC (2026-11-21:
# 5h outage from a gtm-os Chromium crash loop writing 4 MB per respawn).
# Runs from disk-guard.timer; install: scripts/disk-guard/install.sh.
# Thresholds are % of the space non-root processes can use (df's pcent).
set -uo pipefail
MOUNT=/ RESERVE=/home/ubuntu/disk-reserve BALLAST=10
WARN=85 RECLAIM=90 RELEASE=95 REFILL=80
ALERT_WEBHOOK_URL=  # Slack or Discord incoming webhook
[ -f /etc/default/disk-guard ] && . /etc/default/disk-guard
STATE=/var/lib/disk-guard/level

used() { df --output=pcent "$MOUNT" | tail -1 | tr -dc 0-9; }
say() {
  logger -t disk-guard -p "user.$1" "$2"
  [ -n "$ALERT_WEBHOOK_URL" ] && curl -fsS -m 10 -H 'Content-Type: application/json' \
    -d "$(jq -nc --arg t "captain disk-guard: $2" '{text:$t,content:$t}')" "$ALERT_WEBHOOK_URL" >/dev/null
}

u=$(used)
if [ "$u" -ge "$RECLAIM" ]; then
  # Only state that is regenerable by construction.
  docker builder prune -af >/dev/null 2>&1
  docker image prune -f >/dev/null 2>&1
  find /var/lib/docker/volumes/*/_data -path '*/DeferredBrowserMetrics/*' -type f -mmin +60 -delete 2>/dev/null
  say warning "${u}% used, reclaimed caches -> $(used)%"
  u=$(used)
fi
if [ "$u" -ge "$RELEASE" ]; then
  f=$(ls "$RESERVE"/ballast-*.bin 2>/dev/null | tail -1)
  [ -n "$f" ] && rm -f "$f" && say crit "${u}% used, released $f -> $(used)%; find the writer now"
  [ -z "$f" ] && say crit "${u}% used and NO ballast left"
elif [ "$u" -lt "$REFILL" ]; then
  mkdir -p "$RESERVE"
  for i in $(seq -w 1 "$BALLAST"); do
    [ -f "$RESERVE/ballast-$i.bin" ] || { fallocate -l 10G "$RESERVE/ballast-$i.bin"; break; }
  done
fi

level=$(( $(used) >= RELEASE ? 3 : $(used) >= RECLAIM ? 2 : $(used) >= WARN ? 1 : 0 ))
mkdir -p "${STATE%/*}"
prev=$(cat "$STATE" 2>/dev/null || echo 0)
[ "$level" != "$prev" ] && say "$([ "$level" -gt "$prev" ] && echo warning || echo notice)" \
  "level $prev -> $level ($(used)% used, $(ls "$RESERVE"/ballast-*.bin 2>/dev/null | wc -l)/$BALLAST ballast)"
echo "$level" >"$STATE"
