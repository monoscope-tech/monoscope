#!/usr/bin/env bash
# End-to-end ingest throughput bench. Brings up TimeFusion, the local TimescaleDB
# (Docker), and monoscope-server; seeds a project + API key; runs `telemetrygen`
# against monoscope's OTLP/gRPC port and prints spans/sec. Stops everything on
# exit.
#
# Override workload:
#   BENCH_WORKERS=8 BENCH_DURATION=30s ./bench/bench-ingest.sh
#   BENCH_SPANS_PER_TRACE=10 ./bench/bench-ingest.sh
#
# Override topology (assumes you already have these pieces running):
#   TIMEFUSION_DIR=../timefusion SKIP_TF=1 SKIP_PG=1 ./bench/bench-ingest.sh

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

TIMEFUSION_DIR="${TIMEFUSION_DIR:-$ROOT/../timefusion}"
PG_PORT="${PG_PORT:-5432}"
TF_PORT="${TF_PORT:-12345}"
OTLP_PORT="${OTLP_PORT:-4317}"
MONO_HTTP_PORT="${MONO_HTTP_PORT:-8080}"

BENCH_WORKERS="${BENCH_WORKERS:-8}"
BENCH_DURATION="${BENCH_DURATION:-15s}"
BENCH_RATE="${BENCH_RATE:-0}"            # 0 = unthrottled
BENCH_SPANS_PER_TRACE="${BENCH_SPANS_PER_TRACE:-10}"

PIDS=()
cleanup() {
  echo
  echo "--- cleanup ---"
  for pid in "${PIDS[@]}"; do
    echo "cleanup: kill pid=$pid ($(ps -p "$pid" -o command= 2>/dev/null | head -c 80))"
    kill "$pid" 2>/dev/null || true
  done
  # Only tear down TF/PG if we started them this run; leave shared instances alone.
  if [[ -z "${SKIP_TF:-}" ]] && [[ -n "${TF_STARTED:-}" ]]; then
    (cd "$TIMEFUSION_DIR" && make tf-stop) >/dev/null 2>&1 || true
  fi
  if [[ -z "${SKIP_PG:-}" ]] && [[ -n "${PG_STARTED:-}" ]]; then
    docker stop monoscope-bench-pg >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT

require() {
  command -v "$1" >/dev/null 2>&1 || { echo "missing binary: $1" >&2; exit 2; }
}
require psql
require telemetrygen
require docker
require nc

# 1. TimescaleDB (Docker; the CI image, includes all extensions)
if [[ -z "${SKIP_PG:-}" ]]; then
  if ! pg_isready -h localhost -p "$PG_PORT" -q 2>/dev/null; then
    echo "Starting TimescaleDB (Docker)..."
    docker run -d --rm --name monoscope-bench-pg \
      -p "${PG_PORT}:5432" \
      -e POSTGRES_PASSWORD=postgres \
      timescale/timescaledb-ha:pg16-all >/dev/null
    PG_STARTED=1
    until pg_isready -h localhost -p "$PG_PORT" -q 2>/dev/null; do sleep 1; done
    echo "  ready on :$PG_PORT"
  else
    echo "TimescaleDB already on :$PG_PORT, reusing"
  fi
fi

# 2. TimeFusion with frozen clock so test-timestamp data isn't evicted.
if nc -z 127.0.0.1 "$TF_PORT" 2>/dev/null; then
  echo "TimeFusion already on :$TF_PORT, reusing"
elif [[ -z "${SKIP_TF:-}" ]]; then
  echo "Starting TimeFusion (frozen at 2025-01-01)..."
  TF_STARTED=1
  (cd "$TIMEFUSION_DIR" && \
    set -a; source .env.minio; set +a; \
    TIMEFUSION_FROZEN_TIME=2025-01-01T00:00:00Z make tf-start) >/dev/null
  for _ in $(seq 1 120); do
    nc -z 127.0.0.1 "$TF_PORT" 2>/dev/null && break
    sleep 1
  done
fi
nc -z 127.0.0.1 "$TF_PORT" 2>/dev/null || { echo "TF never came up"; exit 1; }
echo "  TF ready on :$TF_PORT"

# 3. Apply monoscope migrations to the bench PG, then seed a project + API key.
DATABASE_URL="postgresql://postgres:postgres@127.0.0.1:${PG_PORT}/postgres"
export DATABASE_URL

echo "Ensuring monoscope schema in PG..."
# Server runs migrations on startup; we just need the project + API key seeded
# after it comes up. Below we do that once the server is reachable.

# 4. monoscope-server. Kill any stale LISTENER on the bench ports (a previous
# aborted bench run or a leftover dev server). Crucially, scope to LISTEN
# sockets only — `lsof -i:PORT` also returns *clients* of that port (TF
# exports OTel traces to monoscope's OTLP endpoint on 4317, so it shows up
# here as a client), and killing those takes the rest of the stack down.
for port in "$OTLP_PORT" "$MONO_HTTP_PORT"; do
  if lsof -ti:"$port" -sTCP:LISTEN >/dev/null 2>&1; then
    echo "Port :$port already has a listener — killing it"
    lsof -ti:"$port" -sTCP:LISTEN | xargs -I{} kill {} 2>/dev/null || true
    sleep 1
  fi
done

SERVER_BIN="$(cabal list-bin monoscope-server 2>/dev/null || echo)"
[[ -z "$SERVER_BIN" ]] && { echo "monoscope-server not built - run: cabal build monoscope-server"; exit 2; }

echo "Starting monoscope-server..."
SERVER_LOG=$(mktemp -t monoscope-bench-server.XXXXXX.log)
BENCH_DIR=$(mktemp -d -t monoscope-bench.XXXXXX)
# Stage migrations dir in BENCH_DIR so server can find them; nothing else (no
# .env to leak prod creds into the process).
ln -s "$ROOT/static" "$BENCH_DIR/static"
(
  cd "$BENCH_DIR"
  # Use `env` (not env -i) so HOME/USER/LANG etc. are preserved; just clobber
  # the variables the server cares about so .env (in any cwd) can't leak prod creds.
  DATABASE_URL="$DATABASE_URL" \
  TIMEFUSION_PG_URL="postgresql://postgres:postgres@127.0.0.1:${TF_PORT}/postgres" \
  PORT="$MONO_HTTP_PORT" \
  GRPC_PORT="$OTLP_PORT" \
  MIGRATIONS_DIR="./static/migrations/" \
  LOG_LEVEL="${LOG_LEVEL:-info}" \
  ENVIRONMENT=DEV \
  ENABLE_BACKGROUND_JOBS=True \
  ENABLE_PUBSUB_SERVICE=False \
  ENABLE_DAILY_JOB_SCHEDULING=False \
  ENABLE_KAFKA_SERVICE=False \
  ENABLE_TIMEFUSION_WRITES=True \
  ENABLE_TIMEFUSION_READS=True \
  KAFKA_BROKERS="" \
  KAFKA_TOPICS="" \
  API_KEY_ENCRYPTION_SECRET_KEY="monoscope123456123456monoscope12" \
  GOOGLE_SERVICE_ACCOUNT_B64="" \
  GOOGLE_APPLICATION_CREDENTIALS="" \
  exec "$SERVER_BIN" >"$SERVER_LOG" 2>&1
) &
SERVER_PID=$!
PIDS+=("$SERVER_PID")
echo "  pid=$SERVER_PID, log=$SERVER_LOG"

# Wait for OUR server to take port 4317 (not some stale listener) -- check that
# the pid holding the port belongs to our subtree of monoscope-server.
echo "Waiting for OTLP gRPC :$OTLP_PORT (owned by our server)..."
for _ in $(seq 1 90); do
  holder=$(lsof -ti:"$OTLP_PORT" 2>/dev/null | head -1)
  if [[ -n "$holder" ]]; then
    # Walk up parents until we find SERVER_PID or hit init
    p=$holder
    while [[ "$p" != "1" && -n "$p" ]]; do
      if [[ "$p" == "$SERVER_PID" ]]; then
        echo "  OTLP ready (server pid $SERVER_PID owns :$OTLP_PORT)"
        break 2
      fi
      p=$(ps -o ppid= -p "$p" 2>/dev/null | tr -d ' ')
      [[ -z "$p" ]] && break
    done
  fi
  kill -0 "$SERVER_PID" 2>/dev/null || { echo "server died:"; tail -50 "$SERVER_LOG"; exit 1; }
  sleep 1
done
holder=$(lsof -ti:"$OTLP_PORT" 2>/dev/null | head -1)
[[ -z "$holder" ]] && { echo "OTLP port never opened:"; tail -80 "$SERVER_LOG"; exit 1; }

# 5. Seed project + API key directly in PG. monoscope stores the raw API key
# in `key_prefix` (no hashing) and authenticates by literal lookup.
echo "Seeding bench project + API key..."
# Unique per-run project so the row count reflects only this bench.
PROJECT_ID="$(uuidgen | tr '[:upper:]' '[:lower:]')"
API_KEY="bench_api_key_$(date +%s%N)"
PGPASSWORD=postgres psql -h 127.0.0.1 -p "$PG_PORT" -U postgres -d postgres -v ON_ERROR_STOP=1 -q <<SQL
  INSERT INTO projects.projects (id, title, payment_plan, active)
  VALUES ('${PROJECT_ID}', 'bench', 'Startup', true)
  ON CONFLICT (id) DO UPDATE SET active = true;

  INSERT INTO projects.project_api_keys (active, project_id, title, key_prefix)
  SELECT true, '${PROJECT_ID}', 'bench', '${API_KEY}'
  WHERE NOT EXISTS (SELECT 1 FROM projects.project_api_keys WHERE key_prefix = '${API_KEY}');
SQL
echo "  seeded project=${PROJECT_ID} key=${API_KEY}"
# Give monoscope-server a moment to refresh the api-key cache (it's a TTL'd LRU).
sleep 2

# 6. Run the load generator.
echo
echo "=== telemetrygen ==="
echo "  workers=$BENCH_WORKERS duration=$BENCH_DURATION spans/trace=$BENCH_SPANS_PER_TRACE rate=$BENCH_RATE/worker"
TG_OUT=$(mktemp -t monoscope-bench-tg.XXXXXX.log)
START=$(date +%s)
telemetrygen traces \
  --otlp-endpoint "127.0.0.1:${OTLP_PORT}" \
  --otlp-insecure \
  --otlp-header "authorization=\"Bearer ${API_KEY}\"" \
  --workers "$BENCH_WORKERS" \
  --duration "$BENCH_DURATION" \
  --rate "$BENCH_RATE" \
  --child-spans "$BENCH_SPANS_PER_TRACE" \
  --service bench-load \
  --batch \
  --status-code Ok 2>&1 | tee "$TG_OUT"
END=$(date +%s)
ELAPSED=$((END - START))
echo

# 7. Tally what landed where.
PG_ROWS=$(PGPASSWORD=postgres psql -h 127.0.0.1 -p "$PG_PORT" -U postgres -d postgres -tAc \
  "SELECT count(*) FROM otel_logs_and_spans WHERE project_id = '${PROJECT_ID}'")
TF_ROWS=$(PGPASSWORD=postgres psql -h 127.0.0.1 -p "$TF_PORT" -U postgres -d postgres -tAc \
  "SELECT count(*) FROM otel_logs_and_spans WHERE project_id = '${PROJECT_ID}'" 2>/dev/null || echo "?")

# Extract telemetrygen-reported totals (it logs "traces" sent).
TG_TRACES=$(grep -oE 'traces=[0-9]+' "$TG_OUT" | tail -1 | cut -d= -f2 || echo "?")
TG_SPANS=$(grep -oE 'spans=[0-9]+' "$TG_OUT" | tail -1 | cut -d= -f2 || echo "?")

echo "=== Results ==="
echo "  Wall time:       ${ELAPSED}s"
echo "  telemetrygen -> traces=${TG_TRACES} spans=${TG_SPANS}"
if [[ "$TG_SPANS" =~ ^[0-9]+$ ]] && (( ELAPSED > 0 )); then
  echo "  spans/sec sent:  $(( TG_SPANS / ELAPSED ))"
fi
echo "  PG row count:    ${PG_ROWS}"
echo "  TF row count:    ${TF_ROWS}"
if [[ "$PG_ROWS" =~ ^[0-9]+$ ]] && (( ELAPSED > 0 )); then
  echo "  spans/sec stored (PG): $(( PG_ROWS / ELAPSED ))"
fi
echo
echo "(server log: $SERVER_LOG)"
echo
echo "Last 20 lines of server log:"
tail -20 "$SERVER_LOG"
echo
echo "TF + timefusion mentions in server log:"
grep -iE "timefusion|enableTimefusion|TIMEFUSION_WRITE" "$SERVER_LOG" | head -20 || echo "(none)"
