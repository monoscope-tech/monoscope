#!/usr/bin/env bash
# Tier-1 OTTL collector test harness.
#
# Boots otelcol-contrib in docker with a test config that injects the
# transform/tier1 block from the production config (so the test can never
# drift from prod), replays OTLP/HTTP fixtures, and golden-diffs the output.
#
#   ./run.sh                  # run all tests
#   UPDATE_GOLDEN=true ./run.sh  # rewrite golden files from current output
#   FIXTURE=02 ./run.sh       # run a single fixture
#
# Requires: docker, yq (v4+), jq, curl.

set -euo pipefail

cd "$(dirname "$0")"

readonly COLLECTOR_IMAGE="otel/opentelemetry-collector-contrib:0.151.0"
readonly CONTAINER_NAME="monoscope-tier1-test"
readonly OTLP_PORT="${OTLP_PORT:-14318}"
readonly UPDATE_GOLDEN="${UPDATE_GOLDEN:-false}"
readonly FIXTURE_FILTER="${FIXTURE:-}"
readonly PROD_CONFIG="../../static/public/otelcol.yaml"

require() {
  command -v "$1" >/dev/null 2>&1 || { echo "missing dependency: $1" >&2; exit 1; }
}
require docker
require yq
require jq
require curl

# Fail fast if the body-promotion list has drifted from Utils.messageKeys.
# This check is independent of docker so we run it first. We're already
# `cd`'d into the script's directory above.
./check-drift.sh || exit 1

work=$(mktemp -d)
trap 'docker rm -f "$CONTAINER_NAME" >/dev/null 2>&1 || true; rm -rf "$work"' EXIT

# Splice the prod transform/tier1 block into the test template. yq merge
# guarantees the test runs the same OTTL statements as production.
yq eval-all '
  select(fileIndex==0) as $base |
  select(fileIndex==1) as $prod |
  $base | .processors."transform/tier1" = $prod.processors."transform/tier1"
' test-config.template.yaml "$PROD_CONFIG" > "$work/config.yaml"

mkdir -p "$work/out"
chmod 777 "$work/out"  # collector runs as non-root in the container

echo "starting $COLLECTOR_IMAGE on :$OTLP_PORT..."
# Pin platform so we don't accidentally pull e.g. amd64 on an arm64 host.
PLATFORM="linux/$(uname -m | sed 's/x86_64/amd64/;s/aarch64/arm64/')"

docker run -d --rm \
  --name "$CONTAINER_NAME" \
  --platform "$PLATFORM" \
  -p "${OTLP_PORT}:4318" \
  -v "$work/config.yaml:/etc/otelcol/config.yaml:ro" \
  -v "$work/out:/out" \
  "$COLLECTOR_IMAGE" \
  --config /etc/otelcol/config.yaml >/dev/null

# Wait for OTLP HTTP to bind. Collector returns 405 on GET /v1/logs once ready.
for i in $(seq 1 30); do
  code=$(curl -s -o /dev/null -w '%{http_code}' "http://localhost:${OTLP_PORT}/v1/logs" || true)
  if [[ "$code" == "405" || "$code" == "415" ]]; then break; fi
  sleep 0.5
  if [[ $i -eq 30 ]]; then
    echo "collector failed to start; logs:" >&2
    docker logs "$CONTAINER_NAME" >&2 || true
    exit 1
  fi
done

# Normalize one OTLP/JSON line into a single log record with sorted attrs and
# stripped resource scaffolding, so the diff isolates what tier1 changed.
# Send every fixture first, then read the output file once. Each fixture
# carries scope.name="<basename>" so we can match records back to fixtures
# without depending on output ordering or file-exporter flush timing.
fixtures=()
for fixture in fixtures/*.json; do
  name=$(basename "$fixture" .json)
  if [[ -n "$FIXTURE_FILTER" && "$name" != ${FIXTURE_FILTER}* ]]; then continue; fi
  fixtures+=("$name")

  http_code=$(curl -sS -o "$work/curl.out" -w '%{http_code}' \
    -H 'content-type: application/json' \
    --data-binary "@$fixture" \
    "http://localhost:${OTLP_PORT}/v1/logs")
  if [[ "$http_code" != "200" ]]; then
    echo "FAIL $name — OTLP POST returned $http_code: $(cat "$work/curl.out")"
    exit 1
  fi
done

# Wait for the file exporter to flush every record. We expect one log
# record per fixture; loop until we see them all (or time out).
expected_count=${#fixtures[@]}
for _ in $(seq 1 50); do
  count=$(jq -s '[.[].resourceLogs[].scopeLogs[].logRecords[]] | length' \
            < "$work/out/logs.jsonl" 2>/dev/null || echo 0)
  [[ "$count" -ge "$expected_count" ]] && break
  sleep 0.2
done

# Flatten all records into one stream, keyed by scope.name. Recursively sort
# any kvlistValue.values arrays so nested map ordering doesn't make the
# golden flap between runs (Go's map iteration is randomized).
jq -c '
  def deepsort:
    if type == "object" then
      with_entries(.value |= deepsort)
      | if has("kvlistValue") then
          .kvlistValue.values |= (sort_by(.key) | map(.value |= deepsort))
        else . end
    elif type == "array" then map(deepsort)
    else . end;
  .resourceLogs[] as $rl
  | $rl.scopeLogs[] as $sl
  | $sl.logRecords[]
  | {scope: $sl.scope.name, record: (. | .attributes |= (sort_by(.key) | map(.value |= deepsort)))}
' < "$work/out/logs.jsonl" > "$work/all.jsonl"

pass=0
fail=0
for name in "${fixtures[@]}"; do
  golden="golden/${name}.json"
  actual=$(jq -c "select(.scope == \"${name}\") | .record" < "$work/all.jsonl" | head -1 | jq -S . 2>/dev/null)

  if [[ -z "$actual" ]]; then
    echo "FAIL $name — no matching record in output"
    fail=$((fail+1))
    continue
  fi

  if [[ "$UPDATE_GOLDEN" == "true" ]]; then
    echo "$actual" > "$golden"
    echo "WROTE $golden"
    pass=$((pass+1))
    continue
  fi

  if [[ ! -f "$golden" ]]; then
    echo "FAIL $name — no golden file. Run with UPDATE_GOLDEN=true to create."
    echo "actual:"; echo "$actual"
    fail=$((fail+1))
    continue
  fi

  expected=$(jq -S . < "$golden")
  if diff -u <(echo "$expected") <(echo "$actual") >/dev/null; then
    echo "PASS $name"
    pass=$((pass+1))
  else
    echo "FAIL $name"
    diff -u <(echo "$expected") <(echo "$actual") || true
    fail=$((fail+1))
  fi
done

echo
echo "results: $pass passed, $fail failed"
[[ $fail -eq 0 ]]
