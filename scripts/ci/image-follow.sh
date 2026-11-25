#!/usr/bin/env bash
# Keep production images flowing: build master's head, finish it, then build whatever master's head
# is by then, skipping the heads in between. Never more than one build, which three overlapping
# 70-minute builds once showed the Mac needs; never cancelling one either: when a newer head
# cancelled the build in flight, a master merging every 30 minutes outran 40-minute builds and no
# image finished, so every deploy timed out waiting. Each finished build writes the compile cache
# back, so the next is incremental. Runs from its own clean clone, so a dirty checkout here never
# blocks it. `make image-follow-install` keeps it running under launchd; stop a foreground one with Ctrl-C.
set -euo pipefail
set -m # each build gets its own process group, so cancelling reaches buildx too
IMAGE=${MONOSCOPE_IMAGE:-ghcr.io/monoscope-tech/monoscope}
dir=${IMAGE_FOLLOW_DIR:-$HOME/.cache/monoscope-image-follow}
interval=${IMAGE_FOLLOW_INTERVAL:-60}
upstream=$(git remote get-url origin)

if [ ! -d "$dir/.git" ]; then
  git clone -q --no-hardlinks --local "$(git rev-parse --show-toplevel)" "$dir"
  git -C "$dir" remote set-url origin "$upstream"
fi
cd "$dir"

pid='' target=''
cancel() { [ -z "$pid" ] || kill -- "-$pid" 2>/dev/null || true; wait "$pid" 2>/dev/null || true; pid=''; }
trap 'cancel; exit 130' INT TERM

while :; do
  head=$(git ls-remote origin refs/heads/master | cut -f1) || head='' # offline: try again next tick
  if [ -n "$pid" ] && ! kill -0 "$pid" 2>/dev/null; then
    wait "$pid" && echo "image-follow: built $target" || echo "image-follow: build of $target failed; retrying on the next head"
    pid=''
  fi
  if [ -z "$pid" ] && [ -n "$head" ] && [ "$head" != "$target" ]; then
    target=$head
    git fetch -q origin master
    if docker buildx imagetools inspect "$IMAGE:tree-$(git rev-parse "$head^{tree}")" >/dev/null 2>&1; then
      echo "image-follow: $head already has an image" # make land builds the merged tree before merging
    else
      git checkout -q --detach "$head" && git clean -qfdx
      echo "image-follow: building $head"
      scripts/ci/ci.sh image "$head" &
      pid=$!
    fi
  fi
  sleep "$interval"
done
