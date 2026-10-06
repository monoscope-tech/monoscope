#!/usr/bin/env bash
# Registry artifacts are keyed by source tree and immutable amd64 base images.
set -euo pipefail
IMAGE=${MONOSCOPE_IMAGE:-ghcr.io/monoscope-tech/monoscope}
REMOTE=${CI_ATTEST_REMOTE:-origin}

die() { echo "release: $*" >&2; exit 1; }
output() { [ -z "${GITHUB_OUTPUT:-}" ] || printf '%s=%s\n' "$1" "$2" >> "$GITHUB_OUTPUT"; }
sha256() { if command -v sha256sum >/dev/null 2>&1; then sha256sum | cut -d' ' -f1; else shasum -a 256 | cut -d' ' -f1; fi; }
registry_digest() {
  docker buildx imagetools inspect "$1" --format '{{.Manifest.Digest}}'
}
current_master() {
  local current
  current=$(git ls-remote --exit-code "$REMOTE" refs/heads/master) || die "cannot verify current master"
  [ "${current%%[[:space:]]*}" = "$1" ] || die "$1 is stale; master is ${current%%[[:space:]]*}"
}

build() {
  local sha tree deps runtime fingerprint digest metadata
  sha=$(git rev-parse "${1:-HEAD}^{commit}")
  [ "$sha" = "$(git rev-parse HEAD)" ] || die "requested revision does not match checkout"
  [ -z "$(git status --porcelain)" ] || die "working tree is dirty"
  tree=$(git rev-parse 'HEAD^{tree}')
  deps="ghcr.io/monoscope-tech/monoscope-deps@$(registry_digest ghcr.io/monoscope-tech/monoscope-deps:latest)"
  runtime="debian@$(registry_digest debian:12-slim)"
  fingerprint=$(printf 'monoscope-image-v1\nlinux/amd64\n%s\n%s\n%s\n' "$tree" "$deps" "$runtime" | sha256)
  if digest=$(registry_digest "$IMAGE:inputs-$fingerprint"); then
    echo "Reusing $IMAGE@$digest for tree $tree" >&2
    # Alias the merge SHA without modifying the image's build revision/provenance.
    docker buildx imagetools create --prefer-index=false -t "$IMAGE:$sha" "$IMAGE@$digest"
  else
    metadata=$(mktemp -t monoscope-image.XXXXXX)
    trap 'rm -f "$metadata"' EXIT
    set --
    if [ -n "${MONOSCOPE_BUILDER:-}" ] && docker buildx inspect "$MONOSCOPE_BUILDER" >/dev/null 2>&1; then
      set -- --builder "$MONOSCOPE_BUILDER"
    fi
    git archive "$sha" | docker buildx build "$@" --platform linux/amd64 -f Dockerfile \
      --build-arg "DEPS_IMAGE=$deps" --build-arg "RUNTIME_IMAGE=$runtime" \
      --build-arg "GIT_HASH=$sha" --build-arg "GIT_COMMIT_DATE=$(git show -s --format=%cI HEAD)" \
      --label "org.opencontainers.image.revision=$sha" \
      --label "org.opencontainers.image.source=https://github.com/monoscope-tech/monoscope" \
      --label "io.monoscope.builder=${GITHUB_ACTOR:-$(git config user.name)}" \
      --label "io.monoscope.source-tree=$tree" --label "io.monoscope.build-inputs=$fingerprint" \
      --cache-from "type=registry,ref=$IMAGE:buildcache" \
      --cache-to "type=registry,ref=$IMAGE:buildcache,mode=max" \
      --metadata-file "$metadata" --provenance=false --push \
      -t "$IMAGE:inputs-$fingerprint" -t "$IMAGE:$sha" -
    digest=$(node -p 'JSON.parse(require("node:fs").readFileSync(process.argv[1], "utf8"))["containerimage.digest"].toString()' "$metadata")
    rm -f "$metadata"
    trap - EXIT
  fi
  output digest "$digest"
  output fingerprint "$fingerprint"
  printf '%s@%s\n' "$IMAGE" "$digest"
}

case "${1:-}" in
  build) shift; build "$@" ;;
  current-master) current_master "$2" ;;
  latest)
    current_master "$2"
    docker buildx imagetools create --prefer-index=false -t "$IMAGE:latest" "$IMAGE@$3"
    ;;
  *) die "usage: release.sh build [sha] | current-master sha | latest sha digest" ;;
esac
