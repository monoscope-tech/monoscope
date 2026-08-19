#!/usr/bin/env bash
# Run the Playwright e2e suite against a throwaway server.
#
# The point of this script is that it CANNOT reach production. The app loads `.env`
# relative to its working directory, so the server is started from a temp directory
# holding an env built from .env.example (which carries no secrets) — the real .env is
# never on its path. Port and database are e2e-only, and every external integration is
# off, so a stray consumer cannot join a production topic.
#
#   scripts/e2e.sh                      # whole suite
#   scripts/e2e.sh tests/live-tail.spec.ts
#   E2E_KEEP=1 scripts/e2e.sh           # leave the server up for debugging
set -euo pipefail
cd "$(dirname "$0")/.."
ROOT=$(pwd)

PORT=${E2E_PORT:-8081}
DB=${E2E_DB:-monoscope_e2e}
# DB_HOST/DB_PORT are what CI sets and what its `pg` capability probes.
PGHOST=${E2E_PGHOST:-${DB_HOST:-localhost}}
PGPORT=${E2E_PGPORT:-${DB_PORT:-5432}}
export PGPASSWORD=${E2E_PGPASSWORD:-postgres}

# Refuse to reuse a port someone else holds. macOS will happily let a second server bind
# :8081 on IPv6 while an older one holds IPv4, and `localhost` resolves to IPv4 first — so
# without this the suite silently tests whatever stale server E2E_KEEP left behind.
if lsof -nP -iTCP:"$PORT" -sTCP:LISTEN >/dev/null 2>&1; then
  echo "port $PORT is already in use — stop that server first:" >&2
  lsof -nP -iTCP:"$PORT" -sTCP:LISTEN | tail -n +2 >&2
  exit 1
fi

psql -h "$PGHOST" -p "$PGPORT" -U postgres -tAc 'SELECT 1' >/dev/null || {
  echo "No postgres on $PGHOST:$PGPORT. Start it with: make timescaledb-docker" >&2; exit 1; }

psql -h "$PGHOST" -p "$PGPORT" -U postgres -tAc \
  "SELECT 1 FROM pg_database WHERE datname='$DB'" | grep -q 1 ||
  psql -h "$PGHOST" -p "$PGPORT" -U postgres -q -c "CREATE DATABASE \"$DB\""

BIN=$(cabal list-bin monoscope-server 2>/dev/null || true)
[ -x "$BIN" ] || { echo "monoscope-server not built. Run: cabal build monoscope-server" >&2; exit 1; }

# The server's working directory. Built from .env.example so no real credential is ever
# copied here; the overrides below are what make it an e2e box rather than a dev one.
RUNDIR=$(mktemp -d)
trap 'kill "${SRV:-}" 2>/dev/null || true; rm -rf "$RUNDIR"' EXIT
ln -s "$ROOT/static" "$RUNDIR/static"
{
  grep -vE '^(DATABASE_URL|PORT|GRPC_PORT|ENABLE_PUBSUB_SERVICE|ENABLE_BACKGROUND_JOBS|KAFKA_BROKERS|BASIC_AUTH_ENABLED)=' .env.example
  # .env.example turns basic auth on, which 401s every request. The specs drive the demo
  # project, which is reachable without a login by design.
  echo "BASIC_AUTH_ENABLED=False"
  # Without this the free tier card is not rendered at all, so the plan picker shows two
  # cards instead of three and the billing/onboarding specs cannot see #freePricing.
  echo "ENABLE_FREETIER=True"
  echo "DATABASE_URL=\"host=$PGHOST user=postgres password=$PGPASSWORD dbname=$DB port=$PGPORT sslmode=disable\""
  echo "PORT=$PORT"
  echo "GRPC_PORT=${E2E_GRPC_PORT:-4318}"     # not 4317: the dev server may hold it
  echo "ENABLE_PUBSUB_SERVICE=False"
  echo "ENABLE_BACKGROUND_JOBS=False"
  echo "MIGRATE_AND_INITIALIZE_ON_START=True" # builds the schema, and migration 0001
} > "$RUNDIR/.env"                            # seeds the demo project the specs use

echo "starting monoscope-server on :$PORT against $DB"
# `exec` so $! is the server itself, not the subshell — killing the subshell would leave
# the server orphaned on the port, and the next run would test against it.
(cd "$RUNDIR" && exec "$BIN") > "$ROOT/e2e-server.log" 2>&1 &
SRV=$!

for _ in $(seq 1 120); do
  curl -fsS "http://localhost:$PORT/ping" >/dev/null 2>&1 && break
  kill -0 "$SRV" 2>/dev/null || { echo "server died — see e2e-server.log" >&2; tail -20 "$ROOT/e2e-server.log" >&2; exit 1; }
  sleep 1
done
curl -fsS "http://localhost:$PORT/ping" >/dev/null || { echo "server never became ready" >&2; exit 1; }

# `|| STATUS=$?` rather than a bare call: under `set -e` a failing suite would abort the
# script here, so E2E_KEEP would never fire — exactly when you want the server left up.
STATUS=0
(cd e2e && E2E_BASE_URL="http://localhost:$PORT" npx playwright test "$@") || STATUS=$?
if [ -n "${E2E_KEEP:-}" ]; then
  echo "server left running on :$PORT (pid $SRV); rundir $RUNDIR — kill it when done"
  trap - EXIT
fi
exit $STATUS
