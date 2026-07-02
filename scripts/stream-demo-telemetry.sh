#!/usr/bin/env bash
# Stream synthetic logs / traces / metrics into a LOCAL monoscope server,
# routed to the demo project (UUID.nil) which needs no auth.
#
# Pointing the CLI at local is just two env vars:
#   MONOSCOPE_API_URL   -> the CLI derives the OTLP gRPC endpoint from this
#                          (http://localhost:8080 => http://localhost:4317)
#   --resource at-project-id:<nil>  -> routes ingestion to the demo project
#
# Usage:
#   scripts/local/stream-demo-telemetry.sh                 # continuous, all kinds
#   scripts/local/stream-demo-telemetry.sh --kind trace    # only one kind
#   RATE=5 COUNT=100 scripts/local/stream-demo-telemetry.sh
#   MONOSCOPE_API_URL=http://localhost:9090 scripts/local/stream-demo-telemetry.sh
set -euo pipefail

NIL_PROJECT="00000000-0000-0000-0000-000000000000"
export MONOSCOPE_API_URL="${MONOSCOPE_API_URL:-http://localhost:8080}"
RATE="${RATE:-1}"
COUNT_ARG=(); [[ -n "${COUNT:-}" ]] && COUNT_ARG=(--count "$COUNT")

# Prefer an installed `monoscope`; fall back to `cabal run` from the repo.
if command -v monoscope >/dev/null 2>&1; then
  CLI=(monoscope)
else
  CLI=(cabal run -v0 monoscope --)
fi

# Kinds to stream: args override the default of all three.
KINDS=("$@"); [[ ${#KINDS[@]} -eq 0 ]] && KINDS=(--kind trace --kind log --kind metric)
# Normalize: strip the flag so we can loop over bare kind names.
kinds=(); for a in "${KINDS[@]}"; do [[ "$a" == --kind ]] || kinds+=("$a"); done

echo "Streaming ${kinds[*]} → ${MONOSCOPE_API_URL} (OTLP :4317), project ${NIL_PROJECT}, ${RATE}/s"

pids=()
for k in "${kinds[@]}"; do
  "${CLI[@]}" telemetrygen \
    --kind "$k" --rate "$RATE" --service "demo-$k" \
    --resource "at-project-id:${NIL_PROJECT}" \
    "${COUNT_ARG[@]}" &
  pids+=($!)
done

trap 'kill "${pids[@]}" 2>/dev/null || true' INT TERM
wait
