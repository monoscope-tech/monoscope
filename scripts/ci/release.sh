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

require_local_context() {
  local endpoint
  [ -z "${GITHUB_ACTIONS:-}" ] || die "production image is missing; generate it locally on the Mac before updating the pull request"
  [ "$(uname -s)" = Darwin ] || die "production image generation requires the Mac's local OrbStack builder"
  [ "${DOCKER_CONTEXT:-}" = orbstack ] || die "set DOCKER_CONTEXT=orbstack for local image generation"
  [ "${MONOSCOPE_BUILDER:-}" = monoscope-local-amd64 ] || die "set MONOSCOPE_BUILDER=monoscope-local-amd64; remote builders and fallback builds are prohibited"
  endpoint=$(docker context inspect orbstack --format '{{.Endpoints.docker.Host}}') || die "cannot inspect the local OrbStack context"
  case "$endpoint" in unix:///*) ;; *) die "the OrbStack context must use a local Unix socket" ;; esac
}

require_local_builder() {
  local endpoints
  require_local_context
  endpoints=$(docker buildx inspect "$MONOSCOPE_BUILDER" | awk '$1 == "Endpoint:" {print $2}') || die "local builder is unavailable; follow docs/local-ci.md"
  [ "$endpoints" = orbstack ] || die "the local builder must have exactly one node using the orbstack endpoint"
}

build() {
  local sha tree deps runtime fingerprint digest metadata
  sha=$(git rev-parse "${1:-HEAD}^{commit}")
  [ "$sha" = "$(git rev-parse HEAD)" ] || die "requested revision does not match checkout"
  [ -z "$(git status --porcelain)" ] || die "working tree is dirty"
  tree=$(git rev-parse 'HEAD^{tree}')
  if [ -n "${GITHUB_ACTIONS:-}" ]; then
    digest=$(registry_digest "$IMAGE:$sha") || digest=$(registry_digest "$IMAGE:tree-$tree") ||
      die "production image is missing; generate it locally on the Mac before updating the pull request"
    fingerprint=$(docker buildx imagetools inspect "$IMAGE@$digest" --format '{{json .Image}}' | node -e '
      let input = "";
      process.stdin.on("data", chunk => input += chunk);
      process.stdin.on("end", () => {
        const image = JSON.parse(input), labels = image.config?.Labels ?? {};
        if (image.os !== "linux" || image.architecture !== "amd64") throw Error("image platform must be linux/amd64");
        if (labels["io.monoscope.source-tree"] !== process.argv[1]) throw Error("image source tree does not match checkout");
        const fingerprint = labels["io.monoscope.build-inputs"];
        if (!/^[a-f0-9]{64}$/.test(fingerprint ?? "")) throw Error("image build provenance is missing");
        console.log(fingerprint);
      });
    ' "$tree") || die "cannot verify production image"
    docker buildx imagetools create --prefer-index=false -t "$IMAGE:$sha" "$IMAGE@$digest"
  else
    deps=${MONOSCOPE_DEPS_IMAGE:-"ghcr.io/monoscope-tech/monoscope-deps@$(registry_digest ghcr.io/monoscope-tech/monoscope-deps:latest)"}
    [[ "$deps" =~ ^ghcr\.io/monoscope-tech/monoscope-deps@sha256:[a-f0-9]{64}$ ]] || die "MONOSCOPE_DEPS_IMAGE must name an immutable monoscope-deps digest"
    runtime="debian@$(registry_digest debian:12-slim)"
    fingerprint=$(printf 'monoscope-image-v1\nlinux/amd64\n%s\n%s\n%s\n' "$tree" "$deps" "$runtime" | sha256)
    if digest=$(registry_digest "$IMAGE:inputs-$fingerprint"); then
      echo "Reusing $IMAGE@$digest for tree $tree" >&2
      # Alias the merge SHA without modifying the image's build revision/provenance.
      docker buildx imagetools create --prefer-index=false -t "$IMAGE:$sha" -t "$IMAGE:tree-$tree" "$IMAGE@$digest"
    else
      require_local_builder
      metadata=$(mktemp -t monoscope-image.XXXXXX)
      trap 'rm -f "$metadata"' EXIT
      set -- --builder "$MONOSCOPE_BUILDER"
      git archive "$sha" | docker buildx build "$@" --platform linux/amd64 -f Dockerfile \
        --build-arg "DEPS_IMAGE=$deps" --build-arg "RUNTIME_IMAGE=$runtime" \
        --build-arg "GIT_HASH=$sha" --build-arg "GIT_COMMIT_DATE=$(git show -s --format=%cI HEAD)" \
        --label "org.opencontainers.image.revision=$sha" \
        --label "org.opencontainers.image.source=https://github.com/monoscope-tech/monoscope" \
        --label "io.monoscope.builder=${GITHUB_ACTOR:-$(git config user.name)}" \
        --label "io.monoscope.source-tree=$tree" --label "io.monoscope.build-inputs=$fingerprint" \
        --cache-from "type=registry,ref=$IMAGE:buildcache" \
        --metadata-file "$metadata" --provenance=false --push \
        -t "$IMAGE:inputs-$fingerprint" -t "$IMAGE:$sha" -t "$IMAGE:tree-$tree" -
      digest=$(node -p 'JSON.parse(require("node:fs").readFileSync(process.argv[1], "utf8"))["containerimage.digest"].toString()' "$metadata")
      rm -f "$metadata"
      trap - EXIT
    fi
  fi
  output digest "$digest"
  output fingerprint "$fingerprint"
  printf '%s@%s\n' "$IMAGE" "$digest"
}

# The proof worker takes the server build from the production image by digest, so only its own layers build.
worker() {
  local sha deps
  sha=$(git rev-parse "$1^{commit}")
  require_local_builder
  deps=${MONOSCOPE_DEPS_IMAGE:-"ghcr.io/monoscope-tech/monoscope-deps@$(registry_digest ghcr.io/monoscope-tech/monoscope-deps:latest)"}
  git archive "$sha" | docker buildx build --builder "$MONOSCOPE_BUILDER" --platform linux/amd64 -f Dockerfile --target impact-worker \
    --build-context "server=docker-image://$IMAGE@$(registry_digest "$IMAGE:$sha")" \
    --build-arg "DEPS_IMAGE=$deps" --build-arg "RUNTIME_IMAGE=debian@$(registry_digest debian:12-slim)" --build-arg "GIT_HASH=$sha" \
    --label "org.opencontainers.image.revision=$sha" --cache-from "type=registry,ref=$IMAGE:buildcache" \
    --provenance=false --push -t "$IMAGE-impact-worker:$sha" -
}

case "${1:-}" in
  build) shift; build "$@" ;;
  worker) worker "$2" ;;
  local-context) require_local_context ;;
  local-builder) require_local_builder ;;
  current-master) current_master "$2" ;;
  latest)
    current_master "$2"
    docker buildx imagetools create --prefer-index=false -t "$IMAGE:latest" "$IMAGE@$3"
    ;;
  *) die "usage: release.sh build [sha] | worker sha | local-context | local-builder | current-master sha | latest sha digest" ;;
esac
