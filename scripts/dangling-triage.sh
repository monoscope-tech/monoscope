#!/usr/bin/env bash
# How much of a dangling commit is genuinely missing from the working tree?
#
# `git show --stat` tells you how big it was, not how much survived. Comparing raw lines does
# not work either: fourmolu and the deep-clean sweeps reword almost every line, so a landed
# change looks 100% missing. This checks the thing refactors preserve — the *identifiers* a
# commit introduced (function names, test names, record fields, CSS classes, SQL columns).
#
# 0% missing = the work is in the tree under some other sha. A high number with real names
# listed = still lost; recover with `git tag rescued-<name> <sha>`.
#
#   scripts/dangling-triage.sh <sha> [<sha>…]
set -uo pipefail
cd "$(git rev-parse --show-toplevel)"

for c in "$@"; do
  subj=$(git log -1 --format='%s' "$c" 2>/dev/null) || { echo "$c: not an object"; continue; }
  date=$(git log -1 --format='%ad' --date=format:'%Y-%m-%d %H:%M' "$c")

  # Identifiers the commit's added lines mention, minus the ones its parent already had —
  # so we test what it INTRODUCED, not the untouched context around it.
  git show --format= -U0 "$c" 2>/dev/null | grep '^+' | grep -v '^+++' \
    | grep -oE '[A-Za-z_][A-Za-z0-9_]{11,}' | sort -u > /tmp/.dt-new
  git show --format= -U0 "$c^" 2>/dev/null | grep -oE '[A-Za-z_][A-Za-z0-9_]{11,}' | sort -u > /tmp/.dt-old
  comm -23 /tmp/.dt-new /tmp/.dt-old > /tmp/.dt-ids

  tot=0; miss=0; missing=""
  while read -r id; do
    [ -n "$id" ] || continue
    tot=$((tot + 1))
    if ! git grep -qF "$id" HEAD -- 2>/dev/null; then
      miss=$((miss + 1))
      [ ${#missing} -lt 400 ] && missing="$missing $id"
    fi
  done < /tmp/.dt-ids

  files=$(git show --stat --format= "$c" 2>/dev/null | tail -1 | sed 's/^ *//')
  pct=0; [ "$tot" -gt 0 ] && pct=$(( miss * 100 / tot ))
  verdict="LANDED"
  [ "$pct" -ge 15 ] && verdict="PARTIAL"
  [ "$pct" -ge 60 ] && verdict="LOST"
  [ "$tot" -eq 0 ] && verdict="(no new identifiers — formatting/docs only)"

  printf '%-9s %s  %-8s %3d%% of %d new identifiers absent   %s\n' \
    "${c:0:9}" "$date" "$verdict" "$pct" "$tot" "$subj"
  printf '          %s\n' "$files"
  [ -n "$missing" ] && printf '          absent:%s\n' "$missing"
done
rm -f /tmp/.dt-new /tmp/.dt-old /tmp/.dt-ids
