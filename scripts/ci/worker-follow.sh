#!/usr/bin/env bash
# Runs as root on the proof-worker host, every 2 minutes (monoscope-worker-follow.timer; install with
# `make worker-follow-install`). Keeps the worker on the build CapRover runs, however it got deployed:
# CI, `make ship` or a rollback. Pull-based, so CI needs no way into this host. A restart happens only
# when the worker's own inputs changed (release.sh worker_inputs), and only once no review is running.
set -euo pipefail
# The timer and `make deploy-worker` may run it at once; the second has nothing to add.
exec 9>/run/monoscope-worker-follow.lock
flock -n 9 || exit 0
W=ghcr.io/monoscope-tech/monoscope-impact-worker
say() { echo "worker-follow: $*"; }
label() { docker image inspect --format "{{index .Config.Labels \"$2\"}}" "$1" 2>/dev/null | sed 's/^<no value>$//'; }

image=$(docker service inspect srv-captain--monoscope --format '{{.Spec.TaskTemplate.ContainerSpec.Image}}')
tree=$(label "$image" io.monoscope.source-tree || { docker pull -q "$image" >/dev/null && label "$image" io.monoscope.source-tree; } || true)
[ -n "$tree" ] || { say "production image $image carries no source tree"; exit 0; }
want=$W:tree-$tree
docker pull -q "$want" >/dev/null 2>&1 || { say "no worker image for tree $tree yet (the Mac builds it with the server image)"; exit 0; }
new=$(label "$want" io.monoscope.worker-inputs || true)
running=$(docker inspect --format '{{.Image}}' monoscope-impact-worker 2>/dev/null || true)
[ -z "$running" ] || [ "$new" != "$(label "$running" io.monoscope.worker-inputs || true)" ] || exit 0

docker tag "$want" "$W:current"
# A restart kills the reviews it interrupts, so wait at most a review's budget. A waiter already
# queued restarts onto the new :current, so it is neither started nor announced twice.
systemctl is-active -q monoscope-impact-worker-restart && exit 0
say "tree $tree changes the worker (inputs ${new:-unlabelled}); restarting once no review is running"
systemd-run --quiet --collect --unit monoscope-impact-worker-restart sh -c \
  'for _ in $(seq 200); do docker top monoscope-impact-worker -o pid,args | grep -qE "pi-host|sandbox.mjs" || break; sleep 15; done; systemctl restart monoscope-impact-worker'
