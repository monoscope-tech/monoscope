#!/usr/bin/env bash
# Tier-1 throughput benchmark.
#
# Runs the same OTLP/HTTP payload (N JSON-bodied log records in one batch)
# against two collector configs:
#   1. WITH transform/tier1 (production behavior)
#   2. WITHOUT — same pipeline minus the processor (baseline)
# Reports records/sec for each and the per-record overhead.
#
# Single-host / single-batch numbers — gives a ceiling estimate, not a
# distributed-load benchmark. If `with` throughput is within ~30% of
# `without`, tier-1 is comfortably practical at any production scale.
#
#   ./bench.sh             # default 50,000 records, 5 runs each
#   N=200000 RUNS=3 ./bench.sh

set -euo pipefail
cd "$(dirname "$0")"

readonly COLLECTOR_IMAGE="otel/opentelemetry-collector-contrib:0.151.0"
readonly CONTAINER_NAME="monoscope-tier1-bench"
readonly OTLP_PORT="${OTLP_PORT:-14319}"
readonly N="${N:-50000}"
readonly RUNS="${RUNS:-5}"
readonly PROD_CONFIG="../../static/public/otelcol.yaml"
PLATFORM="linux/$(uname -m | sed 's/x86_64/amd64/;s/aarch64/arm64/')"

require() { command -v "$1" >/dev/null 2>&1 || { echo "missing: $1" >&2; exit 1; }; }
require docker; require yq; require jq; require python3; require curl

work=$(mktemp -d)
trap 'docker rm -f "$CONTAINER_NAME" >/dev/null 2>&1 || true; rm -rf "$work"' EXIT

# Generate the payload once. Each record has a representative JSON body
# (level + msg + a few extra keys) so tier-1 exercises ParseJSON, severity
# normalization, and body promotion — the realistic hot path.
python3 - <<PY > "$work/payload.json"
import json, sys
N = $N
records = [{
    "timeUnixNano": "0",
    "observedTimeUnixNano": "1700000000000000000",
    "severityNumber": 0,
    "severityText": "",
    "body": {"stringValue": json.dumps({
        "level": "info",
        "msg": "request handled",
        "method": "GET",
        "path": "/api/v1/users",
        "status": 200,
        "duration_ms": 12,
        "i": i,
    })},
    "attributes": [],
} for i in range(N)]
json.dump({
    "resourceLogs": [{
        "resource": {"attributes": [{"key": "service.name", "value": {"stringValue": "bench"}}]},
        "scopeLogs": [{"scope": {"name": "bench"}, "logRecords": records}],
    }]
}, sys.stdout)
PY
payload_size=$(wc -c < "$work/payload.json" | tr -d ' ')
echo "payload: $N records, $((payload_size / 1024 / 1024)) MB"
echo

# Build two configs: WITH tier1 (prod) and WITHOUT (pipeline omits the
# processor). Both export to /dev/null via debug exporter (verbosity:basic
# emits one line per batch — minimal exporter overhead).
build_config() {
  local mode="$1" out="$2"  # mode: none | full | no-promote
  local pipeline_processors='[]'
  [[ "$mode" != "none" ]] && pipeline_processors='["transform/tier1"]'
  yq eval-all "
    select(fileIndex==0) as \$base |
    select(fileIndex==1) as \$prod |
    \$base
    | .processors.\"transform/tier1\" = \$prod.processors.\"transform/tier1\"
    | .exporters = {\"debug\": {\"verbosity\": \"basic\"}}
    | .service.pipelines.logs.exporters = [\"debug\"]
    | .service.pipelines.logs.processors = $pipeline_processors
  " test-config.template.yaml "$PROD_CONFIG" > "$out"
  # `no-promote` strips the 99 body-promotion lines from transform/tier1 so
  # we can see what the rest of the OTTL block costs in isolation.
  if [[ "$mode" == "no-promote" ]]; then
    sed -i.bak '/- set(body, attributes\[/d' "$out" && rm -f "$out.bak"
  fi
}

run_bench() {
  local label="$1" config="$2"
  docker run -d --rm --name "$CONTAINER_NAME" --platform "$PLATFORM" \
    -p "${OTLP_PORT}:4318" \
    -v "$config:/etc/otelcol/config.yaml:ro" \
    "$COLLECTOR_IMAGE" --config /etc/otelcol/config.yaml >/dev/null

  for i in $(seq 1 30); do
    code=$(curl -s -o /dev/null -w '%{http_code}' "http://localhost:${OTLP_PORT}/v1/logs" || true)
    [[ "$code" == "405" || "$code" == "415" ]] && break
    sleep 0.5
  done

  # Warm-up
  curl -sS -o /dev/null -H 'content-type: application/json' \
    --data-binary "@$work/payload.json" "http://localhost:${OTLP_PORT}/v1/logs"

  local total_ms=0
  local results=()
  for r in $(seq 1 "$RUNS"); do
    local elapsed
    elapsed=$(python3 -c "
import time, urllib.request
data = open('$work/payload.json','rb').read()
req = urllib.request.Request('http://localhost:${OTLP_PORT}/v1/logs', data=data,
                             headers={'content-type':'application/json'})
t0 = time.perf_counter()
urllib.request.urlopen(req).read()
print(int((time.perf_counter() - t0) * 1000))
")
    results+=("$elapsed")
    total_ms=$((total_ms + elapsed))
  done

  docker rm -f "$CONTAINER_NAME" >/dev/null

  local avg_ms=$((total_ms / RUNS))
  local rps=$((N * 1000 / avg_ms))
  printf "%-20s avg=%4dms  min=%4dms  max=%4dms  →  %'d records/sec\n" \
    "$label" "$avg_ms" \
    "$(printf '%s\n' "${results[@]}" | sort -n | head -1)" \
    "$(printf '%s\n' "${results[@]}" | sort -n | tail -1)" \
    "$rps" >&2
  echo "$rps"
}

build_config none       "$work/baseline.yaml"
build_config no-promote "$work/no-promote.yaml"
build_config full       "$work/with-tier1.yaml"

echo "running $RUNS POSTs of $N records each (after one warm-up)..."
echo
baseline_rps=$(run_bench   "no transform        " "$work/baseline.yaml"   | tail -1)
no_promote_rps=$(run_bench "tier1 minus promote " "$work/no-promote.yaml" | tail -1)
tier1_rps=$(run_bench      "tier1 (full)        " "$work/with-tier1.yaml" | tail -1)

echo
python3 - <<PY
def us_per_rec(rps): return 1_000_000 / rps
b, np_, t = $baseline_rps, $no_promote_rps, $tier1_rps
print(f"per-record cost:")
print(f"  baseline parse+ship:   {us_per_rec(b):6.2f}µs")
print(f"  + tier1 OTTL (no promote): {us_per_rec(np_)-us_per_rec(b):+6.2f}µs   (ParseJSON, severity, time)")
print(f"  + body promotion:          {us_per_rec(t)-us_per_rec(np_):+6.2f}µs   (top-N message-alias → body)")
print(f"  total tier1 overhead:      {us_per_rec(t)-us_per_rec(b):+6.2f}µs")
PY
