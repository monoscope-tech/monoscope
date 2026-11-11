#!/usr/bin/env bash
# Land the current branch's PR: sign off, wait for its image, and merge only while origin/master
# is still the base the image was built for; if master moved, rebase and repeat. A busy master
# otherwise outruns the image build, and a merge without that exact tree's image cannot deploy.
# Usage: scripts/land.sh <pr-number> [attempts]
set -uo pipefail
PR=${1:?usage: scripts/land.sh <pr-number> [attempts]}; ATTEMPTS=${2:-3}
BR=$(git rev-parse --abbrev-ref HEAD)
export DOCKER_CONTEXT=${DOCKER_CONTEXT:-orbstack} MONOSCOPE_BUILDER=${MONOSCOPE_BUILDER:-monoscope-local-amd64}
for attempt in $(seq 1 "$ATTEMPTS"); do
  git fetch -q origin
  if [ "$(git merge-base HEAD origin/master)" != "$(git rev-parse origin/master)" ]; then
    git rebase -q origin/master || { git rebase --abort; echo "land: rebase conflict, resolve by hand"; exit 1; }
    (cd web-components && npm run build >/dev/null)
  fi
  git push -q --force-with-lease origin "$BR"
  echo "land: attempt $attempt on $(git rev-parse --short HEAD), base $(git rev-parse --short origin/master)"
  make ci-signoff || true
  if make ci-status 2>&1 | grep -q '^run '; then echo "land: checks still unproven after sign-off"; make ci-status 2>&1 | grep '^run '; exit 1; fi
  until docker buildx imagetools inspect "ghcr.io/monoscope-tech/monoscope:tree-$(git rev-parse 'HEAD^{tree}')" >/dev/null 2>&1; do
    make deploy-image || { echo "land: image build failed"; exit 1; }
  done
  git fetch -q origin
  if [ "$(git merge-base HEAD origin/master)" = "$(git rev-parse origin/master)" ]; then
    gh pr merge "$PR" --squash --delete-branch && { echo "land: merged #$PR"; exit 0; }
    echo "land: merge refused"; exit 1
  fi
  echo "land: master moved during attempt $attempt"
done
echo "land: gave up after $ATTEMPTS attempts"; exit 1
