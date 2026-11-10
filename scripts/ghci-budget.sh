#!/usr/bin/env bash
# Refuse to start another GHCi watcher once GHCI_MAX are live. Each holds 5-15 GB; on
# 2026-11-10 nine of them filled swap and hung OrbStack's Docker mid sign-off.
# GHCI_MAX=0 disables the check.
set -u
max=${GHCI_MAX:-6}
[ "$max" -gt 0 ] || exit 0
live=$(pgrep -f 'ghc-[0-9.]+ .*--interactive' | while read -r p; do lsof -a -p "$p" -d cwd -Fn 2>/dev/null | sed -n 's/^n//p'; done)
n=$(printf '%s\n' "$live" | grep -c .)
[ "$n" -lt "$max" ] && exit 0
echo "ghci-budget: $n GHCi sessions are already running (GHCI_MAX=$max). Stop an idle watcher first, or raise GHCI_MAX:" >&2
printf '%s\n' "$live" | sort | uniq -c >&2
exit 1
