#!/usr/bin/env bash
# Portable CI runner + attestation cache.
#
# One definition of "CI" (ci/checks.tsv) that both GitHub Actions and a developer
# laptop execute. A successful run publishes an ATTESTATION — a git ref naming the
# check, a content fingerprint of everything the check depends on, the platform,
# and the capabilities the environment actually had. The gate in the workflow
# looks those refs up and skips any check already proven for the exact tree it is
# about to test, whoever proved it.
#
#   scripts/ci/ci.sh fingerprint [check...]   print check -> fingerprint
#   scripts/ci/ci.sh caps                     capabilities this environment provides
#   scripts/ci/ci.sh gate [check...]          decide skip/run per check (writes $GITHUB_OUTPUT)
#   scripts/ci/ci.sh run [check...]           run checks here, attest each success
#   scripts/ci/ci.sh attest <check>           publish an attestation by hand
#   scripts/ci/ci.sh local [check...]         run the whole thing in CI's own containers
#   scripts/ci/ci.sh shell                    a shell inside the local CI container
#   scripts/ci/ci.sh down                     stop the local CI containers (keeps build caches)
#   scripts/ci/ci.sh clean                    …and delete the cached build volumes
#   scripts/ci/ci.sh image [sha]              build+push the deploy image for a commit (linux/amd64)
#   scripts/ci/ci.sh image-who <sha>          who built the deploy image for a commit
#   scripts/ci/ci.sh gc [days]                delete attestations older than N days (default 30)
#   scripts/ci/ci.sh selftest                 exercise this script's own logic
#
# Bash 3.2 compatible (macOS ships it): no associative arrays, no mapfile.
set -euo pipefail

NS=refs/ci-attest/v1
# CI_ROOT because `ci.sh local` runs this script from a COPY at /tmp (so that editing it
# mid-run is safe). Deriving the root from the script's own path then resolves to `/`, and
# every check dies with "unknown check" before doing any work.
ROOT=${CI_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}
REMOTE=${CI_ATTEST_REMOTE:-origin}
# Bump to invalidate every attestation at once (e.g. after fixing a fingerprint bug).
EPOCH=1
# Capabilities this environment provides; set once per run by cmd_run.
CAPS=''

cd "$ROOT"

# Container CI jobs run as root over a checkout owned by another uid, and git then
# refuses every command in it with "detected dubious ownership" (exit 128). Only
# touches the global config when git is already unusable here, which is the same
# thing anyone would have to do by hand.
git rev-parse --git-dir >/dev/null 2>&1 || git config --global --add safe.directory "$ROOT"

die() { echo "ci: $*" >&2; exit 1; }
note() { echo "── $*" >&2; }

sha256() { if command -v sha256sum >/dev/null 2>&1; then sha256sum | cut -d' ' -f1; else shasum -a 256 | cut -d' ' -f1; fi; }

# ---------------------------------------------------------------- path sets

# Everything the Haskell build and its test suites read. Deliberately wide: the
# frontend bundles are in here because BodyWrapper TH-splices their content hash
# into rendered pages, so a bundle change is a Haskell-visible change.
PATHSET_hs='app src shared cli test tests config proto static/migrations package.yaml cabal.project cabal.project.freeze hpack-includes monoscope.cabal shared/monoscope-shared.cabal cli/monoscope-cli.cabal'
PATHSET_fe='web-components/src web-components/test web-components/package.json web-components/package-lock.json web-components/vite.config.mjs web-components/tsconfig.json web-components/vitest.config.ts web-components/index.html package.json package-lock.json config/tailwind.config.js static/public/assets/css/tailwind.css'
# The CLI package links only monoscope-shared, so its tests cannot observe src/.
PATHSET_cli='cli shared cabal.project cabal.project.freeze'
# Prepended to every check: changing what a check DOES, or the environment it
# runs in, must invalidate it. Named precisely rather than as `.github/workflows`
# — an edit to the Claude review or CLI release workflow says nothing about the
# test suite, and invalidating a 40-minute suite over it would train people to
# distrust the cache.
PATHSET_meta='ci/checks.tsv ci/compose.yml scripts/ci .github/workflows/pullrequest.yml .github/workflows/haskell.yml'

pathset() { eval "printf '%s' \"\${PATHSET_$1:-}\""; }

expand_inputs() {
  local out='' tok
  for tok in $1; do
    case "$tok" in
      @*) out="$out $(pathset "${tok#@}")" ;;
      *) out="$out $tok" ;;
    esac
  done
  printf '%s' "$out"
}

# ---------------------------------------------------------------- checks file

checks_all() { grep -v '^#' ci/checks.tsv | grep -v '^[[:space:]]*$' | cut -f1; }

check_field() { # <name> <1-based field>
  local line
  line=$(grep -v '^#' ci/checks.tsv | awk -F'\t' -v n="$1" '$1==n' | head -1)
  [ -n "$line" ] || die "unknown check '$1' (have: $(checks_all | tr '\n' ' '))"
  printf '%s' "$line" | cut -f"$2"
}

check_requires() { check_field "$1" 2; }
check_inputs() { check_field "$1" 3; }

# ---------------------------------------------------------------- fingerprint

# A tree object for the WORKING tree (tracked edits + new non-ignored files), not
# for HEAD. So a fingerprint taken on a dirty checkout still matches the commit
# that later records exactly that content — you can attest before you commit.
WORKTREE_TREE=''
worktree_tree() {
  if [ -z "$WORKTREE_TREE" ]; then
    local idx
    idx=$(mktemp -t ci-index.XXXXXX)
    GIT_INDEX_FILE=$idx git read-tree HEAD
    GIT_INDEX_FILE=$idx git add -A .
    WORKTREE_TREE=$(GIT_INDEX_FILE=$idx git write-tree)
    rm -f "$idx"
  fi
  printf '%s' "$WORKTREE_TREE"
}

# Fingerprints are PINNED once per run. Steps mutate the tree as they go — hpack
# rewrites monoscope.cabal, `npm ci` can touch a lockfile, the lint job refactors
# src/ in place — and recomputing afterwards would attest a fingerprint the gate
# never looked up, so nothing would ever be reused.
pin_fingerprints() { # <check...>
  local c
  mkdir -p .ci
  : > .ci/fingerprints.tsv
  for c in "$@"; do printf '%s\t%s\n' "$c" "$(fingerprint "$c")" >> .ci/fingerprints.tsv; done
  export CI_FINGERPRINTS=.ci/fingerprints.tsv
}

fingerprint() { # <check>
  local paths tree pinned
  if [ -n "${CI_FINGERPRINTS:-}" ] && [ -f "${CI_FINGERPRINTS}" ]; then
    pinned=$(awk -F'\t' -v n="$1" '$1==n{print $2}' "$CI_FINGERPRINTS")
    [ -n "$pinned" ] && { printf '%s' "$pinned"; return 0; }
  fi
  tree=$(worktree_tree)
  paths=$(expand_inputs "$PATHSET_meta $(check_inputs "$1")")
  # ls-tree lists blob SHAs, so this hashes content without reading a single file.
  # Missing pathspecs are silently empty, which is what we want for optional files.
  { printf 'monoscope-ci\t%s\t%s\t%s\n' "$EPOCH" "$1" "$(check_requires "$1")"
    # shellcheck disable=SC2086
    git ls-tree -r --full-tree "$tree" -- $paths | sort
  } | sha256
}

# ---------------------------------------------------------------- capabilities

probe_tcp() { # host port
  (exec 3<>"/dev/tcp/$1/$2") 2>/dev/null && exec 3<&- 2>/dev/null || return 1
}

# Split a postgres URL into host/port for probing without needing psql.
url_hostport() { # url -> "host port"
  local rest=${1#*://}
  rest=${rest#*@}
  rest=${rest%%/*}
  case "$rest" in *:*) printf '%s %s' "${rest%%:*}" "${rest##*:}" ;;
                  *) printf '%s 5432' "$rest" ;; esac
}

detect_caps() {
  local caps=''
  command -v cabal >/dev/null 2>&1 && caps="$caps ghc"
  command -v node >/dev/null 2>&1 && caps="$caps node"
  command -v bun >/dev/null 2>&1 && caps="$caps bun"
  command -v hlint >/dev/null 2>&1 && caps="$caps hlint"
  probe_tcp "${DB_HOST:-localhost}" "${DB_PORT:-5432}" && caps="$caps pg"
  # shellcheck disable=SC2086
  [ -n "${MINIO_ENDPOINT:-}" ] && probe_tcp $(url_hostport "$MINIO_ENDPOINT") && caps="$caps minio"
  # tf-real is claimed only for a reachable TimeFusion. The suite's
  # Postgres-as-TF fallback is a different thing and must not earn the capability.
  # shellcheck disable=SC2086
  [ -n "${TIMEFUSION_PG_TEST_URL:-}" ] && probe_tcp $(url_hostport "$TIMEFUSION_PG_TEST_URL") && caps="$caps tf-real"
  printf '%s' "$(echo "$caps" | tr ' ' '\n' | grep -v '^$' | sort -u | tr '\n' ' ' | sed 's/ $//')"
}

# provides ⊇ requires
caps_satisfy() { # "<provides>" "<requires>"
  local r
  for r in $2; do
    case " $1 " in *" $r "*) ;; *) return 1 ;; esac
  done
  return 0
}

platform_tag() { printf '%s-%s' "$(uname -s | tr '[:upper:]' '[:lower:]')" "$(uname -m | sed 's/arm64/aarch64/;s/x86_64/amd64/')"; }

# ---------------------------------------------------------------- attestations

# refs/ci-attest/v1/<check>/<fingerprint>/<platform>/<caps.joined>/<yyyymmdd>
# Everything the gate needs is in the ref NAME, so deciding costs one ls-remote
# and zero object fetches.
attest_ref() { # <check> <fingerprint> <caps> [platform]
  printf '%s/%s/%s/%s/%s/%s' "$NS" "$1" "$2" "${4:-$(platform_tag)}" "$(echo "$3" | tr ' ' '.')" "$(date -u +%Y%m%d)"
}

REMOTE_REFS_CACHE=''
remote_refs() {
  if [ -z "$REMOTE_REFS_CACHE" ]; then
    REMOTE_REFS_CACHE=$(mktemp -t ci-refs.XXXXXX)
    if [ "${CI_ATTEST_DISABLED:-}" = "true" ]; then
      note "CI_ATTEST_DISABLED=true — ignoring all attestations"
    else
      git ls-remote "$REMOTE" "$NS/*" 2>/dev/null | cut -f2 > "$REMOTE_REFS_CACHE" || true
    fi
  fi
  cat "$REMOTE_REFS_CACHE"
}

# Echo the matching ref if this check is already proven for this fingerprint by
# an environment good enough for it, else nothing.
find_attestation() { # <check>
  local fp req ref caps
  fp=$(fingerprint "$1")
  req=$(check_requires "$1")
  for ref in $(remote_refs | grep "^$NS/$1/$fp/" || true); do
    caps=$(printf '%s' "$ref" | cut -d/ -f7 | tr '.' ' ')
    if caps_satisfy "$caps" "$req"; then printf '%s' "$ref"; return 0; fi
  done
  return 1
}

record_result() { # <check> — attest now, or stage for a caller that has push access
  local fp caps
  fp=$(fingerprint "$1")
  caps=${CAPS:-$(detect_caps)}
  if [ -n "${CI_ATTEST_OUT:-}" ]; then
    mkdir -p "$(dirname "$CI_ATTEST_OUT")"
    printf '%s\t%s\t%s\t%s\n' "$1" "$fp" "$caps" "$(platform_tag)" >> "$CI_ATTEST_OUT"
    note "recorded $1 for publishing"
  else
    publish_attestation "$1" "$fp" "$caps" "$(platform_tag)"
  fi
}

# Publish the results staged by a run that could not push (the local CI container
# has no credentials — origin is ssh and the image has no keys).
cmd_publish() { # [file]
  local f c fp caps plat
  f=${1:-${CI_ATTEST_OUT:-.ci/attest.tsv}}
  [ -s "$f" ] || { note "nothing to publish ($f)"; return 0; }
  while IFS=$'\t' read -r c fp caps plat; do
    [ -n "$c" ] && publish_attestation "$c" "$fp" "$caps" "$plat"
  done < "$f"
  rm -f "$f"
}

publish_attestation() { # <check> [fingerprint] [caps] [platform]
  local fp caps plat ref tree commit runner_id
  fp=${2:-$(fingerprint "$1")}
  caps=${3:-$(detect_caps)}
  plat=${4:-$(platform_tag)}
  ref=$(attest_ref "$1" "$fp" "$caps" "$plat")
  # Who proved it, so a ref traces back to a run or a machine. NOT
  # ${X:+a$X}${X:-b} — the :- arm yields $X itself when set, which emitted the
  # run id twice and made the link unusable.
  if [ -n "${GITHUB_RUN_ID:-}" ]; then runner_id="github-run-$GITHUB_RUN_ID"; else runner_id="$(whoami)@$(hostname)"; fi
  tree=$(git mktree </dev/null)
  commit=$(GIT_AUTHOR_NAME=${GIT_AUTHOR_NAME:-ci-attest} GIT_AUTHOR_EMAIL=${GIT_AUTHOR_EMAIL:-ci@monoscope.tech} \
           GIT_COMMITTER_NAME=${GIT_COMMITTER_NAME:-ci-attest} GIT_COMMITTER_EMAIL=${GIT_COMMITTER_EMAIL:-ci@monoscope.tech} \
           git commit-tree "$tree" -m "check=$1
fingerprint=$fp
caps=$caps
platform=$plat
commit=$(git rev-parse HEAD)
runner=$runner_id")
  if git push -q "$REMOTE" "${commit}:${ref}" 2>/dev/null; then
    note "attested $1 → $ref"
  else
    note "could not publish attestation for $1 (no push access to $REMOTE?) — result is still valid, just not cached"
  fi
}

# ---------------------------------------------------------------- check bodies

# ONE ghc-options string for every cabal invocation. cabal's build plan hash
# includes these, so `build` and the test checks disagreeing means each test step
# recompiles all ~184 modules that `build` just compiled. That is most of the
# waste in this job, and it is also a memory cliff: without -A64m GHC uses its
# default allocation area, and on a 4-vCPU runner the recompile OOMs and dies
# with no error at all — the failure mode that blocked the deploy of b052d6cf4.
CABAL_OPTS='--ghc-options=-O0 +RTS -A64m -n2m -RTS'

run_body() { # <check>
  case "$1" in
    frontend)
      mkdir -p static/public/assets/css static/public/assets/web-components/dist/js static/public/assets/web-components/dist/css
      npm ci --prefer-offline --no-audit
      npx tailwindcss -i ./static/public/assets/css/tailwind.css -o ./static/public/assets/css/tailwind.min.css --minify
      (cd web-components && npm ci --prefer-offline --no-audit && NODE_ENV=production npx vite build --mode production --sourcemap false)
      ;;
    build)      cabal build all -j "$CABAL_OPTS" ;;
    doctests)   cabal test doctests "$CABAL_OPTS" --test-show-details=direct ;;
    unit-tests) cabal test unit-tests "$CABAL_OPTS" --test-show-details=direct ;;
    cli-tests)  cabal test monoscope-cli:cli-tests "$CABAL_OPTS" --test-show-details=direct ;;
    weeder)
      command -v weeder >/dev/null 2>&1 || cabal install weeder --install-method=copy --installdir=/usr/local/bin --overwrite-policy=always
      weeder --config config/weeder.toml --hie-directory dist-newstyle
      ;;
    hlint)   hlint src/ ;;
    ui-tests) (cd web-components && npm ci --prefer-offline --no-audit && npm test) ;;
    # Drives the real server in a real browser. scripts/e2e.sh starts that server itself on
    # 8081 against a throwaway database, so this only has to supply the binary and chromium.
    e2e)
      cabal build monoscope-server "$CABAL_OPTS"
      (cd e2e && npm ci --prefer-offline --no-audit && npx playwright install --with-deps chromium)
      scripts/e2e.sh
      ;;
    integration-tests) run_integration ;;
    *) die "no body for check '$1'" ;;
  esac
}

# Process-sharded: N copies of the binary, each its own RTS running a disjoint
# shard sequentially. In-process hspec `parallel` deadlocks on the per-test
# resource-pool lifecycle (see test/integration/Main.hs). Keep shards * ~9 conns
# under the server's max_connections.
run_integration() {
  local shards bin i green
  shards=${CI_SHARDS:-4}
  # Without a reachable TimeFusion the URL must go, or every example dies dialling
  # a dead host instead of taking TestUtils' Postgres-as-TimeFusion fallback. Only
  # reached under CI_ALLOW_DEGRADED — cmd_run refuses this check otherwise.
  case " ${CAPS:-} " in *" tf-real "*) ;; *) unset TIMEFUSION_PG_TEST_URL ;; esac
  export USE_EXTERNAL_DB=true LOG_LEVEL=${LOG_LEVEL:-warn}
  (cd web-components && npm ci --prefer-offline --no-audit)
  make build-chart-cli
  cabal build integration-tests "$CABAL_OPTS"
  bin=$(cabal list-bin integration-tests)
  rm -f build-shard-*.log
  for i in $(seq 0 $((shards - 1))); do
    ( start=$(date +%s); SHARD_INDEX=$i SHARD_TOTAL=$shards "$bin" --color > "build-shard-$i.log" 2>&1
      echo "[shard-time] $(( $(date +%s) - start ))s" >> "build-shard-$i.log" ) &
  done
  wait
  echo "=== per-shard (wall-clock | result) — wide spreads ⇒ rebalance ==="
  for i in $(seq 0 $((shards - 1))); do
    printf "shard %s: %-6s | %s\n" "$i" "$(sed -n 's/.*\[shard-time\] //p' "build-shard-$i.log" | tail -1)" \
      "$(grep -hE 'examples?, [0-9]+ failures?' "build-shard-$i.log" | tail -1)"
  done
  green=$(grep -lE "examples?, 0 failures" build-shard-*.log | wc -l | tr -d ' ')
  # green==N already means every shard printed a clean summary; don't also grep for
  # "error:" — the app logs error: lines at LOG_LEVEL=warn on passing runs.
  if [ "$green" -ne "$shards" ] || grep -qE "[1-9][0-9]* failures?|Interrupted" build-shard-*.log; then
    echo "SHARDED RUN FAILED ($green/$shards green):"
    for f in build-shard-*.log; do
      grep -qE "examples?, 0 failures" "$f" && continue
      echo "### $f"
      # The whole Failures section, not a fixed tail: hspec spends ~8 lines per
      # failure, so `tail -40` hides every failure after the first.
      sed -n '/^Failures:/,$p' "$f"
      grep -q '^Failures:' "$f" || tail -60 "$f"
    done
    return 1
  fi
  echo "ALL SHARDS GREEN"
}

# ---------------------------------------------------------------- subcommands

selected_checks() { if [ "$#" -gt 0 ]; then printf '%s\n' "$@"; else checks_all; fi; }

cmd_fingerprint() { local c; for c in $(selected_checks "$@"); do printf '%s\t%s\n' "$c" "$(fingerprint "$c")"; done; }

cmd_gate() {
  local c ref out skip_all=true summary
  out=${GITHUB_OUTPUT:-/dev/null}
  summary=${GITHUB_STEP_SUMMARY:-/dev/null}
  # shellcheck disable=SC2046
  pin_fingerprints $(selected_checks "$@")
  # Hand the pinned values to the jobs that will do the work, so they attest the
  # exact fingerprints this gate just looked up.
  { echo 'fingerprints<<CI_FP_EOF'; cat .ci/fingerprints.tsv; echo CI_FP_EOF; } >> "$out"
  echo "| check | decision | attestation |" >> "$summary"
  echo "|---|---|---|" >> "$summary"
  for c in $(selected_checks "$@"); do
    if ref=$(find_attestation "$c"); then
      echo "$(printf '%s' "skip_$c" | tr '-' '_')=true" >> "$out"
      printf 'skip  %-18s %s\n' "$c" "$ref"
      echo "| \`$c\` | ⏭ skipped | \`$ref\` |" >> "$summary"
    else
      echo "$(printf '%s' "skip_$c" | tr '-' '_')=false" >> "$out"
      printf 'run   %-18s (fingerprint %s)\n' "$c" "$(fingerprint "$c")"
      echo "| \`$c\` | ▶ running | none for \`$(fingerprint "$c")\` |" >> "$summary"
      skip_all=false
    fi
  done
  echo "skip_all=$skip_all" >> "$out"
}

cmd_run() {
  local c req ref rc=0 unrunnable='' degraded=''
  CAPS=$(detect_caps)
  note "capabilities: ${CAPS:-none}"
  # shellcheck disable=SC2046
  [ -n "${CI_FINGERPRINTS:-}" ] || pin_fingerprints $(selected_checks "$@")
  for c in $(selected_checks "$@"); do
    req=$(check_requires "$c")
    # A missing capability is never a silent pass — this run can say nothing about
    # that check. CI_ALLOW_DEGRADED still runs it for the feedback (e.g. the suite
    # against the Postgres-as-TimeFusion fallback) but refuses to attest it, so a
    # weaker environment can never satisfy CI on a stronger one's behalf. Either
    # way the rest of the sweep continues and the exit code stays honest.
    if ! caps_satisfy "$CAPS" "$req"; then
      if [ "${CI_ALLOW_DEGRADED:-}" != "true" ]; then
        note "CANNOT RUN $c here — needs [$req], have [$CAPS]"
        unrunnable="$unrunnable $c"; rc=1; continue
      fi
      note "running $c DEGRADED — needs [$req], have [$CAPS]; result will not be attested"
      degraded="$degraded $c"
    elif [ "${CI_FORCE:-}" != "true" ] && ref=$(find_attestation "$c"); then
      note "$c already proven ($ref) — skipping (CI_FORCE=true to override)"
      continue
    fi
    note "running $c"
    if run_body "$c"; then
      # A green check stays green even if we cannot record it. Publishing touches
      # the network and the object store; neither is part of what the check proved.
      case " $degraded " in *" $c "*) ;; *) [ "${CI_NO_ATTEST:-}" = "true" ] \
        || record_result "$c" || note "could not record $c — it still passed, just isn't cached" ;; esac
    else
      rc=1
      note "FAILED $c"
      [ "${CI_KEEP_GOING:-}" = "true" ] || return 1
    fi
  done
  [ -z "$unrunnable" ] || note "not run here (CI will still have to):$unrunnable"
  [ -z "$degraded" ] || note "run degraded, NOT attested (CI will still run these):$degraded"
  return $rc
}

cmd_attest() { [ "$#" -ge 1 ] || die "attest needs a check name"; publish_attestation "$1"; }

cmd_gc() {
  local days cutoff ref d deleted=0
  days=${1:-30}
  cutoff=$(date -u -d "-$days days" +%Y%m%d 2>/dev/null || date -u -v-"$days"d +%Y%m%d)
  # Batched: one push per 200 refs, not one per ref — a year's worth of daily runs
  # is thousands of refs and a round trip each would take longer than the CI it saves.
  local batch=''
  for ref in $(remote_refs); do
    d=${ref##*/}
    case "$d" in [0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]) ;; *) continue ;; esac
    [ "$d" -lt "$cutoff" ] || continue
    batch="$batch :${ref}"
    deleted=$((deleted + 1))
    if [ "$((deleted % 200))" -eq 0 ]; then
      # shellcheck disable=SC2086
      git push -q "$REMOTE" $batch; batch=''
    fi
  done
  # shellcheck disable=SC2086
  [ -z "$batch" ] || git push -q "$REMOTE" $batch
  note "deleted $deleted attestation(s) older than $days days"
}

# ---------------------------------------------------------------- local docker

COMPOSE_FILE=ci/compose.yml
compose() { docker compose -f "$COMPOSE_FILE" --project-name monoscope-ci "$@"; }

cmd_local() {
  command -v docker >/dev/null 2>&1 || die "docker is required for \`ci.sh local\`"
  docker compose version >/dev/null 2>&1 || die "docker compose v2 is required"
  note "starting CI services…"
  # An arm64 TimeFusion built here is the difference between `integration-tests`
  # running locally and it being CI's job forever — the published image is amd64
  # only and segfaults under emulation. Pick it up automatically: needing two
  # exported variables to get the one check that cannot otherwise run is exactly
  # the friction that sends people back to pushing and waiting.
  if [ -z "${MONOSCOPE_CI_TF_IMAGE:-}" ] && [ "$(uname -m)" = arm64 ] \
     && docker image inspect "$TF_LOCAL_IMAGE" >/dev/null 2>&1; then
    export MONOSCOPE_CI_TF_IMAGE="$TF_LOCAL_IMAGE" MONOSCOPE_CI_TF_PLATFORM=linux/arm64
    note "using locally built $TF_LOCAL_IMAGE (native arm64)"
  fi
  compose up -d --wait postgres minio
  # TimeFusion publishes no arm64 image, and the amd64 one dies under emulation on
  # Apple Silicon (SIGSEGV). Don't let that sink the rest of the sweep: warn, carry
  # on, and let cmd_run refuse the one check that needs it. docs/local-ci.md has
  # the build-it-locally recipe.
  # Readiness is probed from the runner, not by a compose healthcheck: TF's image
  # is distroless, so it cannot run one (see ci/compose.yml). Waiting on the port
  # this way also proves the thing the check actually needs — pgwire accepting
  # connections — rather than that the container is up.
  if compose up -d timefusion >/dev/null 2>&1 \
     && compose run --rm -T --no-deps runner bash -c \
          'for _ in $(seq 1 60); do (exec 3<>/dev/tcp/timefusion/5432) 2>/dev/null && exit 0; sleep 2; done; exit 1' >/dev/null 2>&1; then
    note "TimeFusion is accepting pgwire connections"
  else
    note "TimeFusion did not start (${MONOSCOPE_CI_TF_PLATFORM:-linux/amd64} on $(uname -m)) — integration-tests will be left to CI; see docs/local-ci.md"
  fi
  rm -f .ci/attest.tsv
  local rc=0
  # --rm so a failed run leaves nothing behind; the caches live in named volumes.
  # Forward the run's knobs; `compose run` only passes what it is told to.
  # Run from a COPY inside the container, not from the bind mount. bash reads a
  # script incrementally by file offset, so editing scripts/ci/ci.sh while a run
  # is in flight makes the running shell resume at a stale offset and die with
  # "syntax error near unexpected token". A `make ci` lasts tens of minutes —
  # long enough that editing it meanwhile is a normal thing to do, not a mistake.
  compose run --rm \
    -e CI_ROOT=/build \
    -e "CI_ALLOW_DEGRADED=${CI_ALLOW_DEGRADED:-}" -e "CI_FORCE=${CI_FORCE:-}" -e "CI_KEEP_GOING=${CI_KEEP_GOING:-}" \
    runner sh -c 'cp scripts/ci/ci.sh /tmp/ci-run.sh && exec bash /tmp/ci-run.sh run "$@"' ci-run "$@" || rc=$?
  # Publish whatever passed even if a later check failed — a green check is green.
  cmd_publish .ci/attest.tsv
  [ "$rc" -eq 0 ] || note "local CI failed (exit $rc); services left up for debugging — \`make ci-down\` to clean up"
  return $rc
}

cmd_shell() {
  compose up -d --wait postgres minio
  compose up -d --wait timefusion 2>/dev/null || note "TimeFusion did not start; the shell has no tf-real"
  compose run --rm runner bash
}
# Volumes survive `down` on purpose: they hold the cabal store and dist-newstyle,
# and losing them means the next `make ci` is a cold build. `clean` is the nuke.
cmd_down() { compose down --remove-orphans; }
cmd_clean() { compose down -v --remove-orphans; }

# ---------------------------------------------------------------- deploy image

# The deploy image is keyed by commit SHA, so the REGISTRY is its cache and the
# tag is its fingerprint — no attestation needed to decide whether to build it.
# What an attestation does add is provenance: an image is the artifact that runs
# in production, and unlike a fingerprint it can't be re-derived from source
# (a Haskell build isn't bit-reproducible), so "who built what's running" has to
# be recorded at build time or it is unanswerable.
IMAGE=${MONOSCOPE_IMAGE:-ghcr.io/monoscope-tech/monoscope}

# The image must be linux/amd64 because prod is. On an Apple Silicon laptop that
# means emulation, and an emulated GHC build is the slowest thing in this repo —
# slow enough that it pushes people back to "push it and let CI do it", which is
# the habit this whole file exists to kill. A BUILDER naming a native amd64
# buildx endpoint removes the emulation instead of tolerating it.
#
# `make builder-setup` creates one over SSH. Nothing here requires it: with no
# builder configured this falls back to the local emulated build, which is what
# it always did.
BUILDER=${MONOSCOPE_BUILDER:-monoscope-amd64}

# A TimeFusion image built for this machine's own architecture. `make tf-image`
# produces it; cmd_local picks it up on its own.
TF_LOCAL_IMAGE=${MONOSCOPE_TF_LOCAL_IMAGE:-timefusion:local-arm64}

image_exists() { docker manifest inspect "$IMAGE:$1" >/dev/null 2>&1; }

# The configured builder, but only if it actually exists and is usable — a stale
# name in the environment must degrade to the default builder, not fail a deploy.
builder_args() {
  [ -n "$BUILDER" ] || return 0
  docker buildx inspect "$BUILDER" >/dev/null 2>&1 || return 0
  printf -- '--builder\n%s' "$BUILDER"
}

cmd_image() { # [sha] — build and push the production image for a commit
  local sha date args
  sha=${1:-HEAD}
  [ -z "$(git status --porcelain)" ] || die "working tree is dirty; the image would not match $sha"
  git cat-file -e "$sha^{commit}" 2>/dev/null || die "unknown commit $sha"
  # Tags are full SHAs. Taking a short one at face value would look up a tag that
  # cannot exist and report the image as missing, which reads as "the build
  # failed" rather than "you abbreviated".
  sha=$(git rev-parse "$sha^{commit}")
  if image_exists "$sha"; then
    note "$IMAGE:$sha already exists — nothing to do (CI will skip its build)"
    return 0
  fi
  date=$(git show -s --format=%cI "$sha")
  args=$(builder_args)
  if [ -n "$args" ]; then
    note "building $IMAGE:$sha on builder '$BUILDER' (native linux/amd64)"
  else
    note "building $IMAGE:$sha for linux/amd64 (no native builder — emulated here; see \`make builder-setup\`)"
  fi
  # :latest must move with :<sha>. docker-compose.yml (self-hosters) pulls
  # :latest, and CI tags both — a local build that tagged only the sha would
  # leave :latest pointing at whatever CI last published.
  # Share CI's registry build cache, in both directions. Without it a laptop
  # build starts cold every time — it re-does the dependency layers CI already
  # published, which is most of the wall clock and the reason a local build felt
  # slower than letting CI do it. Writing it back means a local build also warms
  # the cache for CI and for everyone else.
  # shellcheck disable=SC2086
  docker buildx build ${args:+$args} --platform linux/amd64 -f Dockerfile \
    --build-arg "GIT_HASH=$sha" --build-arg "GIT_COMMIT_DATE=$date" \
    --cache-from "type=registry,ref=$IMAGE:buildcache" \
    --cache-to "type=registry,ref=$IMAGE:buildcache,mode=max" \
    --provenance=false --push -t "$IMAGE:$sha" -t "$IMAGE:latest" .
  # Record who built it BEFORE anyone can deploy it.
  publish_attestation image "$sha" docker linux-amd64
  note "pushed. Push commit $sha and CI will reuse this image instead of rebuilding."
}

# A native amd64 builder, so the image build is a build and not an emulation.
# BUILD_HOST is any amd64 machine you can `ssh` to that runs Docker; the deploy
# host itself is the obvious one, and it is also the machine that will pull the
# image, so the push is a local-network hop.
#
# The buildkit container is capped, because that host is usually production:
# BUILD_CPUS cores and BUILD_MEMORY are all it may take. Uncapped, a -j48 GHC
# build competes with the thing it is being built to replace.
cmd_builder() { # setup | rm | status
  local host=${BUILD_HOST:-} cpus=${BUILD_CPUS:-12} mem=${BUILD_MEMORY:-48g} container
  container="buildx_buildkit_${BUILDER}0"
  case "${1:-status}" in
    setup)
      [ -n "$host" ] || die "BUILD_HOST is unset, e.g. BUILD_HOST=ubuntu@build.example.com make builder-setup"
      ssh -o BatchMode=yes "$host" 'docker info >/dev/null' \
        || die "cannot reach docker on $host over ssh"
      case "$(ssh -o BatchMode=yes "$host" 'uname -m')" in
        x86_64|amd64) ;;
        *) die "$host is not amd64 — a builder there would emulate too" ;;
      esac
      docker buildx inspect "$BUILDER" >/dev/null 2>&1 && { note "builder '$BUILDER' already exists"; return 0; }
      docker buildx create --name "$BUILDER" --driver docker-container \
        --platform linux/amd64 --driver-opt env.BUILDKIT_STEP_LOG_MAX_SIZE=-1 "ssh://$host" >/dev/null
      docker buildx inspect --bootstrap "$BUILDER" >/dev/null
      # Resource limits are applied to the container rather than passed as
      # driver-opts: buildx only learned those opts after the version Docker
      # Desktop ships, and a builder that refuses to be created is worse than
      # one that is capped a second later.
      ssh -o BatchMode=yes "$host" "docker update --cpus $cpus --memory $mem --memory-swap $mem $container" >/dev/null \
        || note "could not cap the builder container — it will use the whole host"
      note "builder '$BUILDER' ready on $host (capped at ${cpus} cpus / ${mem})"
      ;;
    rm)     docker buildx rm "$BUILDER" >/dev/null 2>&1 && note "removed '$BUILDER'" || note "no builder '$BUILDER'" ;;
    status) docker buildx inspect "$BUILDER" 2>/dev/null | grep -E '^(Name|Status|Platforms|Endpoint):' || note "no builder '$BUILDER' — \`make builder-setup\`" ;;
    *)      die "usage: ci.sh builder [setup|rm|status]" ;;
  esac
}

# ---------------------------------------------------------------- deploy
#
# Deploying is one HTTP call telling CapRover which image to run, so there is no
# reason it had to live only inside a GitHub job. Doing it here is what closes
# the loop: checks, image and deploy all happen on the machine that has the
# code, and CI becomes a verifier rather than the critical path.
#
# Credentials are env-only (.env is read for them, and .env is gitignored). A
# per-app deploy token, not the admin password: it deploys this one app and can
# be rotated without touching anything else.
caprover_env() {
  if [ -z "${CAPROVER_URL:-}" ] && [ -f .env ]; then
    set -a
    # shellcheck disable=SC1091
    . ./.env
    set +a
  fi
  [ -n "${CAPROVER_URL:-}" ]    || die "CAPROVER_URL is unset (see docs/local-ci.md)"
  [ -n "${CAPROVER_APP:-}" ]    || die "CAPROVER_APP is unset (see docs/local-ci.md)"
  [ -n "${CAPROVER_APP_TOKEN:-}" ] || die "CAPROVER_APP_TOKEN is unset (see docs/local-ci.md)"
}

cmd_deploy() { # [sha] — point the CapRover app at this commit's image
  local sha body code
  sha=${1:-HEAD}
  git cat-file -e "$sha^{commit}" 2>/dev/null || die "unknown commit $sha"
  sha=$(git rev-parse "$sha^{commit}")
  caprover_env
  # Never deploy an image that is not in the registry: CapRover would accept the
  # request and then fail to pull, taking the app down rather than leaving the
  # previous version running.
  image_exists "$sha" || die "$IMAGE:$sha is not in the registry — run \`make deploy-image\` first"
  # Refuse to deploy a commit origin has never seen. What runs in production has
  # to be a commit someone else can check out; otherwise a rollback has nothing
  # to roll back to and provenance is a local-only claim.
  git fetch -q "$REMOTE" 2>/dev/null || true
  git merge-base --is-ancestor "$sha" "$REMOTE/master" 2>/dev/null \
    || die "$sha is not on $REMOTE/master — push it before deploying"
  note "deploying $IMAGE:$sha to $CAPROVER_APP"
  body=$(printf '{"captainDefinitionContent":"{\\"schemaVersion\\":2,\\"imageName\\":\\"%s:%s\\"}","gitHash":"%s"}' "$IMAGE" "$sha" "$sha")
  code=$(curl -sS -o /tmp/caprover-deploy.$$ -w '%{http_code}' -X POST \
    "$CAPROVER_URL/api/v2/user/apps/appData/$CAPROVER_APP?detached=1" \
    -H 'Content-Type: application/json' -H 'x-namespace: captain' \
    -H "x-captain-auth: $CAPROVER_APP_TOKEN" -d "$body")
  # CapRover answers 200 with a status field even when it refuses, so the body
  # decides, not the HTTP code.
  if [ "$code" = 200 ] && grep -q '"status":100' /tmp/caprover-deploy.$$; then
    rm -f /tmp/caprover-deploy.$$
    publish_attestation deploy "$sha" caprover "$CAPROVER_APP"
    note "deploy accepted. CapRover is pulling the image; \`make deploy-status\` to watch it."
  else
    note "CapRover refused the deploy (http $code): $(cat /tmp/caprover-deploy.$$)"
    rm -f /tmp/caprover-deploy.$$
    return 1
  fi
}

# Reading what is deployed needs an admin session; the per-app deploy token can
# only deploy. That asymmetry is the right way round — the credential that sits
# in every developer's .env is the one that cannot enumerate the server — so
# status is the optional extra here, not deploy.
cmd_deploy_status() { # what the app is running right now
  local token
  caprover_env
  [ -n "${CAPROVER_PASSWORD:-}" ] || { note "CAPROVER_PASSWORD unset — deploy works without it, status needs it"; return 0; }
  token=$(curl -sS -X POST "$CAPROVER_URL/api/v2/login" -H 'Content-Type: application/json' \
    -H 'x-namespace: captain' -d "{\"password\":\"$CAPROVER_PASSWORD\"}" \
    | python3 -c 'import json,sys; print(json.load(sys.stdin).get("data",{}).get("token",""))')
  [ -n "$token" ] || { note "could not log in to $CAPROVER_URL for status"; return 0; }
  curl -sS "$CAPROVER_URL/api/v2/user/apps/appDefinitions" \
    -H 'x-namespace: captain' -H "x-captain-auth: $token" \
    | python3 -c "
import json,sys
for a in json.load(sys.stdin).get('data',{}).get('appDefinitions',[]):
    if a['appName']==sys.argv[1]:
        v={x['version']:x for x in a.get('versions',[])}.get(a.get('deployedVersion'),{})
        print('deployed: version', a.get('deployedVersion'), '|', v.get('deployedImageName','?'))
" "$CAPROVER_APP"
}

# Has this commit already been deployed from someone's machine? The gate asks so
# CI does not restart production a second time with the identical image.
cmd_deployed() { # <sha> — exit 0 if a deploy attestation exists
  local sha
  sha=$(git rev-parse "${1:-HEAD}^{commit}" 2>/dev/null) || sha=$1
  remote_refs | grep -q "^$NS/deploy/$sha/" || return 1
}

cmd_image_who() { # <sha> — one line naming who built the image for this commit
  local ref
  ref=$(remote_refs | grep "^$NS/image/$1/" | head -1 || true)
  if [ -z "$ref" ]; then echo "built by: CI (no local-build record)"; return 0; fi
  git fetch -q "$REMOTE" "$ref" 2>/dev/null \
    && echo "built by: $(git cat-file commit FETCH_HEAD | sed -n 's/^runner=//p')" \
    || echo "built by: (record exists but could not be read) $ref"
}

# ---------------------------------------------------------------- ship
#
# The whole path in one command: prove the tree, build the image, publish the
# commit, deploy it. Every step already existed; what did not exist was doing
# them in one place, so the slow half (image + deploy, ~85 minutes of CI wall
# clock) stopped being something you waited on a remote queue for.
#
# The order is deliberate. Checks first, because everything after them is
# expensive and irreversible-ish. Image before push, so CI's probe finds it and
# skips its own build. Push before deploy, because production must run a commit
# that exists on origin.
cmd_ship() {
  local sha rc=0
  [ -z "$(git status --porcelain)" ] || die "working tree is dirty — commit or stash first"
  [ "$(git rev-parse --abbrev-ref HEAD)" = master ] || [ -n "${SHIP_ANY_BRANCH:-}" ] \
    || die "not on master (deploying another branch needs SHIP_ANY_BRANCH=1)"
  git fetch -q "$REMOTE" 2>/dev/null || true
  git merge-base --is-ancestor "$REMOTE/master" HEAD 2>/dev/null \
    || die "$REMOTE/master has commits you do not have — rebase before shipping"
  sha=$(git rev-parse HEAD)
  # Pin BEFORE the checks run, for the same reason the gate does: the run
  # rewrites the tree as it goes (hpack regenerates the cabal file, `npm ci`
  # touches lockfiles), so a fingerprint computed afterwards would not be the one
  # anything attested, and every check would look unproven.
  # shellcheck disable=SC2046
  pin_fingerprints $(selected_checks "$@")

  note "ship: 1/4 checks"
  # Keep going past a failure. The sweep runs in checks.tsv order, so stopping at
  # the first one would let a red `weeder` — which does not gate the deploy —
  # prevent `e2e`, which does, from ever running. Decide what blocks below, on
  # evidence, rather than on which check happened to fail first.
  CI_KEEP_GOING=true cmd_local "$@" || rc=$?
  # weeder and hlint are not on the deploy path (they gate pull requests, not
  # this), so a failure there must not block a ship the same way a failed test
  # does. Anything on the deploy path failing is fatal.
  if [ "$rc" -ne 0 ]; then
    local blocking=''
    # The checks just published refs; the cached listing predates them.
    REMOTE_REFS_CACHE=''
    for c in build doctests unit-tests cli-tests integration-tests e2e; do
      find_attestation "$c" >/dev/null 2>&1 || blocking="$blocking $c"
    done
    [ -z "$blocking" ] || die "not shipping: unproven deploy-path checks:$blocking"
    note "checks reported a failure, but every deploy-path check is proven — continuing"
  fi

  note "ship: 2/4 image"
  cmd_image "$sha"

  note "ship: 3/4 push $sha"
  git push -q "$REMOTE" HEAD:master

  note "ship: 4/4 deploy"
  cmd_deploy "$sha"
  cmd_deploy_status
}

# ---------------------------------------------------------------- selftest

assert() { # <desc> <expected> <actual>
  if [ "$2" = "$3" ]; then echo "ok   $1"; else echo "FAIL $1: expected [$2] got [$3]"; SELFTEST_RC=1; fi
}

cmd_selftest() {
  SELFTEST_RC=0

  assert "caps superset"      ok "$(caps_satisfy 'ghc pg minio tf-real' 'ghc pg' && echo ok)"
  assert "caps exact"         ok "$(caps_satisfy 'ghc' 'ghc' && echo ok)"
  assert "caps missing one"   no "$(caps_satisfy 'ghc pg minio' 'ghc pg minio tf-real' || echo no)"
  assert "caps empty require" ok "$(caps_satisfy '' '' && echo ok)"
  # 'tf' must not satisfy 'tf-real': substring matches would silently accept the
  # Postgres-as-TimeFusion fallback for a check that needs the real service.
  assert "caps no substring"  no "$(caps_satisfy 'ghc tf' 'tf-real' || echo no)"

  assert "url host:port"  'db 5433' "$(url_hostport postgresql://u:p@db:5433/x)"
  assert "url default pt" 'db 5432' "$(url_hostport postgresql://u:p@db/x)"
  assert "url no creds"   'h 9000'  "$(url_hostport http://h:9000)"

  assert "inputs expand" " src .hlint.yaml" "$(expand_inputs 'src .hlint.yaml')"
  assert "pathset expand nonempty" yes "$([ -n "$(expand_inputs '@hs')" ] && echo yes)"

  # Every check in the TSV must have a body and a fingerprint.
  local c seen=''
  for c in $(checks_all); do
    grep -q "^    $c)" "$0" || { echo "FAIL $c has no run_body case"; SELFTEST_RC=1; }
    seen="$seen $(fingerprint "$c")"
  done
  assert "fingerprints distinct" "$(echo $seen | tr ' ' '\n' | wc -l | tr -d ' ')" \
                                 "$(echo $seen | tr ' ' '\n' | sort -u | wc -l | tr -d ' ')"
  assert "fingerprint stable" "$(fingerprint hlint)" "$(WORKTREE_TREE=''; fingerprint hlint)"

  # A change under a check's inputs must move its fingerprint; one outside must not.
  # Probe with NEW files only — never edit-and-restore a tracked file, which would
  # discard whatever the developer has uncommitted in it.
  local before after probe
  probe=.ci-selftest-probe-$$
  before=$(fingerprint hlint)
  : > "src/$probe"; WORKTREE_TREE=''; after=$(fingerprint hlint); rm -f "src/$probe"
  assert "input change moves fp" changed "$([ "$before" != "$after" ] && echo changed)"

  WORKTREE_TREE=''
  : > "web-components/src/$probe"; WORKTREE_TREE=''; after=$(fingerprint hlint); rm -f "web-components/src/$probe"
  assert "unrelated change keeps fp" same "$([ "$before" = "$after" ] && echo same)"

  # A typo in an input path is invisible — git silently matches nothing — and
  # silently narrows what the check depends on, which is how an untested change
  # ships. Every declared path must exist.
  local p missing=''
  for p in $(expand_inputs "$PATHSET_meta $(for c in $(checks_all); do check_inputs "$c"; echo; done | tr '\n' ' ')" | tr ' ' '\n' | sort -u); do
    [ -e "$p" ] || missing="$missing $p"
  done
  assert "every declared input path exists" "" "$missing"

  # Pinning is what survives a step that rewrites the tree mid-run.
  WORKTREE_TREE=''
  before=$(fingerprint hlint)
  pin_fingerprints hlint
  : > "src/$probe"; WORKTREE_TREE=''; after=$(fingerprint hlint); rm -f "src/$probe"
  unset CI_FINGERPRINTS; rm -rf .ci
  assert "pinned fp survives tree change" "$before" "$after"

  assert "ref roundtrip caps" 'ghc pg' \
    "$(printf '%s' "$(attest_ref x deadbeef 'ghc pg')" | cut -d/ -f7 | tr '.' ' ')"
  assert "ref roundtrip check" x "$(printf '%s' "$(attest_ref x deadbeef 'ghc')" | cut -d/ -f4)"
  assert "ref roundtrip fp" deadbeef "$(printf '%s' "$(attest_ref x deadbeef 'ghc')" | cut -d/ -f5)"

  # A stale or mistyped builder name must fall back to the default builder. The
  # alternative — failing the build — would mean one developer's leftover
  # environment variable blocks a deploy on a machine that could build fine.
  assert "builder falls back" "" "$(BUILDER=definitely-not-a-builder builder_args)"
  assert "no builder configured" "" "$(BUILDER='' builder_args)"

  # deploy/image records reuse the attestation ref shape, so `deployed` can
  # answer from ref names alone — same one-ls-remote property as the checks.
  assert "deploy ref sha"  deadbeef "$(printf '%s' "$(attest_ref deploy deadbeef caprover monoscope)" | cut -d/ -f5)"
  assert "deploy ref app"  monoscope "$(printf '%s' "$(attest_ref deploy deadbeef caprover monoscope)" | cut -d/ -f6)"
  assert "deploy ref check" deploy "$(printf '%s' "$(attest_ref deploy deadbeef caprover monoscope)" | cut -d/ -f4)"

  # Every subcommand the Makefile and the workflows invoke must exist. A rename
  # here is otherwise found by a failing deploy, at the worst possible moment.
  local sub
  for sub in ship deploy deploy-status deployed image image-who builder; do
    assert "dispatch has $sub" yes "$(grep -qE "^  $sub\)|^  $sub\b" "$0" && echo yes)"
  done

  [ "$SELFTEST_RC" -eq 0 ] && echo "selftest: all good"
  return $SELFTEST_RC
}

# ----------------------------------------------------------------------------

cmd=${1:-help}; shift || true
case "$cmd" in
  fingerprint) cmd_fingerprint "$@" ;;
  caps)        detect_caps; echo ;;
  gate)        cmd_gate "$@" ;;
  run)         cmd_run "$@" ;;
  attest)      cmd_attest "$@" ;;
  publish)     cmd_publish "$@" ;;
  local)       cmd_local "$@" ;;
  shell)       cmd_shell ;;
  down)        cmd_down ;;
  clean)       cmd_clean ;;
  image)       cmd_image "$@" ;;
  image-who)   cmd_image_who "$@" ;;
  deploy)      cmd_deploy "$@" ;;
  deploy-status) cmd_deploy_status ;;
  deployed)    cmd_deployed "$@" ;;
  ship)        cmd_ship "$@" ;;
  builder)     cmd_builder "$@" ;;
  gc)          cmd_gc "$@" ;;
  selftest)    cmd_selftest ;;
  checks)      checks_all ;;
  *)           sed -n '2,30p' "$0" | sed 's/^# \{0,1\}//' ;;
esac
