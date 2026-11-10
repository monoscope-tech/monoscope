#!/bin/bash
# Serves the guest's production server offline with this checkout's frontend swapped in; see .monoscope/PROOF.md.
set -euo pipefail
trap 'tail -qn 30 /tmp/{tailwind,vite,postgres,monoscope,traffic}.log >&2 2>/dev/null' ERR
app=/opt/monoscope
if [ -f web-components/vite.config.mjs ]; then
  ln -sfn /opt/proof/node_modules node_modules
  ln -sfn /opt/proof/web-components/node_modules web-components/node_modules
  cp -r static/public/. $app/static/public/
  ./node_modules/.bin/tailwindcss -i static/public/assets/css/tailwind.css -o $app/static/public/assets/css/tailwind.min.css >/tmp/tailwind.log 2>&1
  dist=$app/static/public/assets/web-components/dist entry=$(cat $app/entry)
  (cd web-components && ./node_modules/.bin/vite build --outDir $dist --emptyOutDir >/tmp/vite.log 2>&1)
  # The server names its bundle entry at compile time, so the new entry takes the old name.
  built=$(node -p "require('$dist/manifest.json')['index.html'].file")
  [ "$built" = "$entry" ] || { mv "$dist/$built" "$dist/$entry" && sed -i "s/${built#js/}/${entry#js/}/g" "$dist"/js/*.js; }
fi
/usr/lib/postgresql/18/bin/pg_ctl -D $app/pgdata -l /tmp/postgres.log -w start >/dev/null
(cd $app && set -a && . ./app.env && exec ./monoscope-server >/tmp/monoscope.log 2>&1 &)
curl -sf -o /dev/null --retry 60 --retry-delay 1 --retry-connrefused http://127.0.0.1:8080/ping
(node /opt/traffic/monoscope-traffic.mjs >/tmp/traffic.log 2>&1 &)
for _ in $(seq 1 60); do grep -q streaming /tmp/traffic.log && exit 0; sleep 0.5; done
false
