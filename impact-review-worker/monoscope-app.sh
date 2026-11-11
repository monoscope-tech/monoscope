#!/bin/bash
# Serves Monoscope offline with this checkout's frontend; see .monoscope/PROOF.md. With --server it also
# builds this checkout's server from the workspace's warm build and runs that instead of the guest's.
set -euo pipefail
trap 'tail -qn 30 /tmp/{tailwind,vite,cabal,postgres,monoscope,traffic}.log >&2 2>/dev/null' ERR
app=/opt/monoscope
ln -sfn /opt/proof/node_modules node_modules
ln -sfn /opt/proof/web-components/node_modules web-components/node_modules
if [ "${1:-}" = --server ]; then
  ./node_modules/.bin/tailwindcss -i static/public/assets/css/tailwind.css -o static/public/assets/css/tailwind.min.css >/tmp/tailwind.log 2>&1
  (cd web-components && ./node_modules/.bin/vite build >/tmp/vite.log 2>&1)
  mkdir -p dist-newstyle/src && cp -Rn /opt/haskell/dist-src/. dist-newstyle/src/
  # The same flags as the workspace build, or cabal reconfigures and rebuilds from scratch.
  cabal build exe:monoscope-server --offline --ghc-options="-O0 -j2" >/tmp/cabal.log 2>&1
  server=$(cabal list-bin exe:monoscope-server --offline | tail -n 1) root=$PWD migrate=True
else
  cp -r static/public/. $app/static/public/
  ./node_modules/.bin/tailwindcss -i static/public/assets/css/tailwind.css -o $app/static/public/assets/css/tailwind.min.css >/tmp/tailwind.log 2>&1
  dist=$app/static/public/assets/web-components/dist entry=$(cat $app/entry)
  (cd web-components && ./node_modules/.bin/vite build --outDir $dist --emptyOutDir >/tmp/vite.log 2>&1)
  # The guest's server names its bundle entry at compile time, so the new entry takes the old name.
  built=$(node -p "require('$dist/manifest.json')['index.html'].file")
  [ "$built" = "$entry" ] || { mv "$dist/$built" "$dist/$entry" && sed -i "s/${built#js/}/${entry#js/}/g" "$dist"/js/*.js; }
  server=$app/monoscope-server root=$app migrate=False
fi
/usr/lib/postgresql/18/bin/pg_ctl -D $app/pgdata -l /tmp/postgres.log -w start >/dev/null
# The guest's database is migrated to its own build; a pull request's new migrations apply on top.
(cd "$root" && set -a && . $app/app.env && MIGRATIONS_DIR=$root/static/migrations/ MIGRATE_AND_INITIALIZE_ON_START=$migrate exec "$server" >/tmp/monoscope.log 2>&1 &)
curl -sf -o /dev/null --retry 120 --retry-delay 1 --retry-connrefused http://127.0.0.1:8080/ping
(node /opt/traffic/monoscope-traffic.mjs >/tmp/traffic.log 2>&1 &)
for _ in $(seq 1 60); do grep -q streaming /tmp/traffic.log && exit 0; sleep 0.5; done
false
