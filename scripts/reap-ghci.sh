#!/usr/bin/env bash
# Kill GHCi sessions orphaned by a stopped or restarted ghcid. ghcid stops `cabal repl`
# but not its `setup repl` -> `ghc --interactive` children, which reparent to launchd and
# keep 5-50 GB each until the Mac runs out of memory. A watched session always has a
# live parent, so anything here with ppid 1 is dead weight.
# Scope: sessions whose cwd is under $1 (default: the directory holding this checkout,
# so sibling worktrees are covered). DRY_RUN=1 only lists.
set -u
root=${1:-$(cd "$(dirname "$0")/../.." && pwd)}
tree() { local c; for c in $(pgrep -P "$1"); do tree "$c"; done; echo "$1"; }
victims=()
while read -r pid; do
  cwd=$(lsof -a -p "$pid" -d cwd -Fn 2>/dev/null | sed -n 's/^n//p')
  case $cwd in "$root"/*) ;; *) continue ;; esac
  pids=$(tree "$pid" | tr '\n' ' ')
  echo "reap-ghci: orphaned $(ps -o command= -p "$pid" | cut -c1-70)… in $cwd: $pids"
  victims+=($pids)
done < <(ps -axo pid=,ppid=,command= | awk '$2 == 1 && (/ghc-[0-9.]+ .*--interactive/ || /\/setup\/setup .* repl / || /^ *[0-9]+ +1 +cabal repl/) { print $1 }')
[ ${#victims[@]} -gt 0 ] && [ -z "${DRY_RUN:-}" ] || exit 0
kill "${victims[@]}" 2>/dev/null; sleep 1; kill -9 "${victims[@]}" 2>/dev/null
exit 0
