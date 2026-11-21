#!/usr/bin/env bash
# Keeps captain's root disk from filling. Postgres, Redpanda, TimeFusion,
# CapRover and our own alerting all share it and all die on ENOSPC (2026-11-21:
# 5h outage from a gtm-os Chromium crash loop writing 4 MB per respawn).
# Runs from disk-guard.timer; install: scripts/disk-guard/install.sh.
# Thresholds are % of the space non-root processes can use (df's pcent).
set -uo pipefail
shopt -s nullglob
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

u=$(used) ballast=("$RESERVE"/ballast-*.bin)
if [ "$u" -ge "$RECLAIM" ]; then
  # Only state that is regenerable by construction.
  docker builder prune -af >/dev/null 2>&1
  docker image prune -f >/dev/null 2>&1
  say warning "${u}% used, reclaimed caches -> $(used)%"
  u=$(used)
fi
if [ "$u" -ge "$RELEASE" ]; then
  if [ ${#ballast[@]} -gt 0 ]; then
    rm -f "${ballast[-1]}"
    say crit "${u}% used, released ${ballast[-1]} -> $(used)%; find the writer now"
  else
    say crit "${u}% used and NO ballast left"
  fi
elif [ "$u" -lt "$REFILL" ] && [ ${#ballast[@]} -lt "$BALLAST" ]; then
  mkdir -p "$RESERVE"
  for i in $(seq -w 1 "$BALLAST"); do [ -f "$RESERVE/ballast-$i.bin" ] || { fallocate -l 10G "$RESERVE/ballast-$i.bin"; break; }; done
fi

u=$(used) ballast=("$RESERVE"/ballast-*.bin)
level=$((u >= RELEASE ? 3 : u >= RECLAIM ? 2 : u >= WARN ? 1 : 0))
prev=$(cat "$STATE" 2>/dev/null || echo 0)
sev=notice
((level > prev)) && sev=warning
[ "$level" != "$prev" ] && say "$sev" "level $prev -> $level (${u}% used, ${#ballast[@]}/$BALLAST ballast)"
echo "$level" >"$STATE"
