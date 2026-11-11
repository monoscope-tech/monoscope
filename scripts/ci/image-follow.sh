#!/usr/bin/env bash
# Keep production images flowing: build the image for master's head, and only that one.
# A newer head cancels the build of an older one, since CI's deploy refuses stale commits
# anyway and three overlapping 70-minute builds once starved the Mac. Runs from its own
# clean clone, so a dirty checkout here never blocks it. Stop with Ctrl-C.
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
  head=$(git ls-remote origin refs/heads/master | cut -f1)
  if [ -n "$pid" ] && ! kill -0 "$pid" 2>/dev/null; then
    wait "$pid" && echo "image-follow: built $target" || echo "image-follow: build of $target failed; retrying on the next head"
    pid=''
  fi
  if [ -n "$head" ] && [ "$head" != "$target" ]; then
    [ -z "$pid" ] || echo "image-follow: $head supersedes $target; cancelling its build"
    cancel
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
