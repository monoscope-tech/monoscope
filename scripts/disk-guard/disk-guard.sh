#!/usr/bin/env bash
# Keeps captain's root disk from filling. Postgres, Redpanda, TimeFusion,
# CapRover and our own alerting all share it and all die on ENOSPC (2026-11-21:
# 5h outage from a gtm-os Chromium crash loop writing 4 MB per respawn).
# Runs from disk-guard.timer; install: scripts/disk-guard/install.sh.
# Thresholds are % of the space non-root processes can use (POSIX df Capacity).
set -uo pipefail
shopt -s nullglob

MOUNT=${MOUNT:-/}
STATE_DIR=${STATE_DIRECTORY:-/var/lib/disk-guard}
RESERVE=${RESERVE:-$STATE_DIR/ballast}
STATE=${STATE:-$STATE_DIR/level}
BALLAST=${BALLAST:-10}
BALLAST_MIB=${BALLAST_MIB:-10240}
WARN=${WARN:-85} RECLAIM=${RECLAIM:-90} RELEASE=${RELEASE:-95} REFILL=${REFILL:-80}
ALERT_WEBHOOK_URL=${ALERT_WEBHOOK_URL:-${SLACK_WEBHOOK_URL:-}}

df_info() { df -Pk "$1" | awk 'NR==2 {gsub(/%/,"",$5); print $1, $3, $4, $5}'; }
say() {
  local msg="captain disk-guard: $2" esc
  logger -t disk-guard -p "user.$1" "$2"
  [ -z "$ALERT_WEBHOOK_URL" ] && return 0
  esc=${msg//\\/\\\\}; esc=${esc//\"/\\\"}; esc=${esc//$'\n'/\\n}
  printf 'url = "%s"\n' "${ALERT_WEBHOOK_URL//\"/}" | curl -fsS -m 10 -K - \
    -H 'Content-Type: application/json' -d "{\"text\":\"$esc\",\"content\":\"$esc\"}" >/dev/null \
    || logger -t disk-guard -p user.err "webhook alert failed"
}

mkdir -p "$STATE_DIR" "$RESERVE"
read -r mnt_dev used_kib avail_kib u <<<"$(df_info "$MOUNT")"
read -r res_dev _ <<<"$(df_info "$RESERVE")"
[[ ${u:-} =~ ^[0-9]+$ ]] || { logger -t disk-guard -p user.err "df failed for $MOUNT"; exit 1; }
[ "$mnt_dev" = "$res_dev" ] || { logger -t disk-guard -p user.err "RESERVE ($RESERVE) is not on MOUNT ($MOUNT)"; exit 1; }

rc=0 sev='' release_err='' prune_err='' notes='' ballast=("$RESERVE"/ballast-*.bin)
note() { notes=${notes:+$notes; }$1; }
# Unlink ballast BEFORE talking to dockerd: unlink(2) needs no daemon and gives
# near-ENOSPC metadata transactions breathing room to succeed.
if ((u >= RELEASE && ${#ballast[@]} > 0)); then
  f=${ballast[${#ballast[@]} - 1]}
  if rm -f -- "$f"; then
    read -r _ used_kib avail_kib after <<<"$(df_info "$MOUNT")"
    sev=crit
    note "${u}% used, released ${f##*/} -> ${after}%; find the writer now"
    u=$after
  else
    logger -t disk-guard -p user.err "failed to release $f at ${u}% used"
    release_err="; FAILED to release ${f##*/}" rc=1
  fi
fi
if ((u >= RECLAIM)); then
  before=$u
  # Only state that is regenerable by construction.
  if timeout 60 docker builder prune -af >/dev/null 2>&1 && timeout 60 docker image prune -f >/dev/null 2>&1; then
    read -r _ used_kib avail_kib u <<<"$(df_info "$MOUNT")"
    ((u < before)) && { [ -z "$sev" ] && sev=warning; note "${before}% used, reclaimed caches -> ${u}%"; }
  else
    logger -t disk-guard -p user.err "docker prune failed at ${before}% used"
    prune_err='; docker prune failed' rc=1
  fi
elif ((u < REFILL && ${#ballast[@]} < BALLAST)); then
  need_kib=$((BALLAST_MIB * 1024)) total_kib=$((used_kib + avail_kib))
  if ((avail_kib > need_kib && (used_kib + need_kib) * 100 < WARN * total_kib)); then
    for ((i = 1; i <= BALLAST; i++)); do
      printf -v id '%02d' "$i"
      target="$RESERVE/ballast-$id.bin" tmp="$RESERVE/.ballast-$id.tmp"
      [ -f "$target" ] && continue
      if fallocate -l "$((BALLAST_MIB * 1048576))" "$tmp" && chmod 600 "$tmp" && mv -f "$tmp" "$target"; then
        read -r _ used_kib avail_kib u <<<"$(df_info "$MOUNT")"
      else
        rm -f "$tmp"
        logger -t disk-guard -p user.err "ballast refill failed for ${target##*/}"
        rc=1
      fi
      break
    done
  fi
fi

ballast=("$RESERVE"/ballast-*.bin)
prev=$(cat "$STATE" 2>/dev/null || true)
[[ $prev =~ ^[0-4]$ ]] || prev=0
thr() { ((u >= $1 || (prev >= $2 && u >= $1 - 2))); }
if thr "$RELEASE" 3; then
  level=$((${#ballast[@]} == 0 || ${#release_err} > 0 ? 4 : 3))
elif thr "$RECLAIM" 2; then
  level=2
elif thr "$WARN" 1; then
  level=1
else
  level=0
fi

if ((level != prev)); then
  if [ -z "$sev" ]; then
    if ((level >= 3)); then sev=crit; elif ((level > prev)); then sev=warning; else sev=notice; fi
  fi
  extra="$release_err$prune_err"
  ((level == 4 && ${#ballast[@]} == 0)) && extra="; NO ballast left — find the writer now$extra"
  note "level $prev -> $level (${u}% used, ${#ballast[@]}/$BALLAST ballast$extra)"
fi
[ -n "$notes" ] && say "${sev:-notice}" "$notes"
{ printf '%s\n' "$level" >"$STATE.tmp" 2>/dev/null && mv -f "$STATE.tmp" "$STATE"; } || printf '%s\n' "$level" >"$STATE" || rc=1
exit "$rc"
