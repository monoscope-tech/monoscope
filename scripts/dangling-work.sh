#!/usr/bin/env bash
# Find work that exists but is not on any branch — the failure mode that lost a day of UI
# work on 2026-09-02 (`git add -A`, rebase --abort, `reset --hard`, and the commit became a
# dangling object that `git log --all` cannot see).
#
# Two independent hazards, both reported:
#
#   COMMITS   — commits reachable from no ref. Split into ORPHAN (the patch appears nowhere
#               on a ref: real loss) and REBASED (same patch-id already landed under another
#               sha: noise from rebases and cherry-picks, which is most of them).
#   WORKTREES — uncommitted changes and unpushed commits across every worktree and sibling
#               repo. Not lost yet, but this is exactly the state that got swept into someone
#               else's `git add -A` last time.
#
#   scripts/dangling-work.sh              # all known repos
#   scripts/dangling-work.sh /path/to/repo
#   SINCE=90 scripts/dangling-work.sh     # widen the age window (default 30 days)
#
# Read-only. Recover anything it finds with:  git tag rescued-<name> <sha>
set -uo pipefail

SINCE_DAYS=${SINCE:-30}
REPOS=("$@")
if [ ${#REPOS[@]} -eq 0 ]; then
  # This repo, plus every sibling checkout beside it. Derived rather than hardcoded so
  # the script works for anyone: the sibling sweep is the point, since the work that got
  # destroyed lived in a checkout nobody was looking at.
  self=$(git rev-parse --show-toplevel 2>/dev/null || pwd)
  REPOS=("$self")
  for sib in "$(dirname "$self")"/*/; do
    sib=${sib%/}
    [ "$sib" = "$self" ] && continue
    git -C "$sib" rev-parse --git-dir >/dev/null 2>&1 && REPOS+=("$sib")
  done
fi

cutoff=$(( $(date +%s) - SINCE_DAYS * 86400 ))

scan_repo() {
  local repo=$1
  git -C "$repo" rev-parse --git-dir >/dev/null 2>&1 || return 0
  local name; name=$(basename "$repo")
  echo "════════════════════════════════════════════════════════════════"
  echo "REPO  $name  ($repo)"
  echo "════════════════════════════════════════════════════════════════"

  local tmp; tmp=$(mktemp -d); trap 'rm -rf "$tmp"' RETURN

  # Every commit any ref can reach. Membership test for "is this already kept?".
  git -C "$repo" rev-list --all > "$tmp/reachable" 2>/dev/null
  sort -u "$tmp/reachable" > "$tmp/reachable.sorted"

  # Candidates: reflog entries (recoverable but detached) plus objects no reflog holds either.
  {
    git -C "$repo" reflog --all --format='%H' 2>/dev/null
    git -C "$repo" fsck --unreachable --no-reflogs 2>/dev/null | awk '$2=="commit"{print $3}'
  } | sort -u > "$tmp/candidates"

  comm -23 "$tmp/candidates" "$tmp/reachable.sorted" > "$tmp/orphans"

  # Patch-ids of everything on a ref in the window, so a rebased copy of a commit is not
  # reported as lost work. This is the difference between a useful report and 1400 lines.
  git -C "$repo" rev-list --all --since="$SINCE_DAYS days ago" 2>/dev/null \
    | while read -r c; do
        printf '%s ' "$(git -C "$repo" diff-tree -p --no-commit-id "$c" 2>/dev/null | git patch-id --stable 2>/dev/null | awk '{print $1}')"
        echo "$c"
      done | awk 'NF==2{print $1}' | sort -u > "$tmp/kept-patch-ids"

  local n_orphan=0 n_rebased=0
  while read -r c; do
    [ -n "$c" ] || continue
    local ts; ts=$(git -C "$repo" log -1 --format='%ct' "$c" 2>/dev/null) || continue
    [ -z "$ts" ] && continue
    [ "$ts" -lt "$cutoff" ] && continue

    local subj date pid files
    subj=$(git -C "$repo" log -1 --format='%s' "$c")
    date=$(git -C "$repo" log -1 --format='%ad' --date=format:'%Y-%m-%d %H:%M' "$c")
    # Stash commits describe a working tree, not a landed change; label rather than rank them.
    local kind="commit"
    case "$subj" in "WIP on "*|"On "*) kind="stash" ;; esac

    # Noise, all of it expected and none of it work anyone authored:
    #   check=…  — this repo's own CI attestations under refs/ci-attest/v1/* (see ci/checks.tsv)
    #   index on — the index half of a stash; its content is in the paired "On …" commit
    #   dependabot — superseded pushes to a bot branch that later force-updated
    case "$subj" in
      check=*|"index on "*) n_rebased=$((n_rebased + 1)); continue ;;
      "chore(deps): bump"*|"chore(ci): bump"*|"Bump "*) n_rebased=$((n_rebased + 1)); continue ;;
    esac

    pid=$(git -C "$repo" diff-tree -p --no-commit-id "$c" 2>/dev/null | git patch-id --stable 2>/dev/null | awk '{print $1}')
    if [ -n "$pid" ] && grep -qxF "$pid" "$tmp/kept-patch-ids"; then
      n_rebased=$((n_rebased + 1))
      continue
    fi

    # Does it still say something master does not? A commit whose every changed file already
    # matches master is landed work under a different sha, however it got there.
    local differing=0 touched
    touched=$(git -C "$repo" show --stat --format= --name-only "$c" 2>/dev/null | grep -v '^$' || true)
    while read -r f; do
      [ -n "$f" ] || continue
      git -C "$repo" diff --quiet "$c" origin/master -- "$f" 2>/dev/null || differing=$((differing + 1))
    done <<< "$touched"
    if [ "$differing" -eq 0 ]; then
      n_rebased=$((n_rebased + 1))
      continue
    fi

    # A same-subject commit on a ref means this is almost certainly a pre-rebase copy: the
    # work is kept, just under a different sha. Say so rather than making someone diff it.
    local twin verdict
    twin=$(git -C "$repo" log --all --format='%h' --fixed-strings --grep="$subj" 2>/dev/null | head -1)
    if [ -n "$twin" ]; then verdict="LANDED-AS $twin"; else verdict="NO COPY ON ANY REF"; fi

    files=$(git -C "$repo" show --stat --format= "$c" 2>/dev/null | tail -1 | sed 's/^ *//')
    n_orphan=$((n_orphan + 1))
    printf 'ORPHAN  %s  %s  [%s]  %s file(s) differ from master  — %s\n        %s\n        %s\n' \
      "$date" "${c:0:9}" "$kind" "$differing" "$verdict" "$subj" "${files:-(no diff vs parent)}"
  done < "$tmp/orphans"

  echo "-- commits: $n_orphan orphaned, $n_rebased rebased/duplicate (suppressed), window ${SINCE_DAYS}d"
  echo

  # Uncommitted and unpushed work, per worktree. The other half of the hazard.
  #
  # With PRESERVE=1 each dirty worktree also gets a snapshot commit tagged
  # `snapshot/<worktree>/<date>`. `git stash create` builds the commit object WITHOUT
  # touching the index or the working tree, so this is safe to run against a worktree
  # another session is actively editing — it just makes the content survive a reset.
  git -C "$repo" worktree list --porcelain 2>/dev/null | awk '/^worktree /{print $2}' | while read -r wt; do
    [ -d "$wt" ] || continue
    local dirty ahead branch
    dirty=$(git -C "$wt" status --porcelain 2>/dev/null | wc -l | tr -d ' ')
    if [ "${PRESERVE:-0}" = "1" ] && [ "$dirty" != "0" ]; then
      local snap tagname
      snap=$(git -C "$wt" stash create "dangling-work snapshot" 2>/dev/null)
      if [ -n "$snap" ]; then
        tagname="snapshot/$(basename "$wt")/$(date +%Y%m%d-%H%M)"
        # Tag from INSIDE the worktree. `git worktree list` also reports sibling checkouts
        # that are really their own clones (monoscope-2 is one), and their objects live in
        # their own store — tagging from $repo fails with "nonexistent object".
        git -C "$wt" tag -f "$tagname" "$snap" >/dev/null 2>&1 && echo "SNAPSHOT $tagname -> ${snap:0:9}"
      fi
    fi
    branch=$(git -C "$wt" rev-parse --abbrev-ref HEAD 2>/dev/null)
    ahead=$(git -C "$wt" rev-list --count '@{u}..HEAD' 2>/dev/null || echo "no-upstream")
    if [ "$dirty" != "0" ] || { [ "$ahead" != "0" ] && [ "$ahead" != "no-upstream" ]; }; then
      printf 'WORKTREE %-58s %-34s dirty=%-4s unpushed=%s\n' "${wt/#$HOME/~}" "$branch" "$dirty" "$ahead"
      git -C "$wt" status --porcelain 2>/dev/null | head -8 | sed 's/^/           /'
      [ "$dirty" -gt 8 ] 2>/dev/null && echo "           … $((dirty - 8)) more"
    fi
  done

  local st; st=$(git -C "$repo" stash list 2>/dev/null)
  [ -n "$st" ] && { echo; echo "STASHES:"; echo "$st" | sed 's/^/  /'; }
  echo
}

for r in "${REPOS[@]}"; do scan_repo "$r"; done
