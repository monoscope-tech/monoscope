# Signed query-failure PNGs

These images are actual responses from `widgetPngGetH`, captured by the
`does not cache query failures as successful empty PNGs` integration test in
`test/integration/Pages/DashboardsSpec.hs`. They were visually inspected at
960 × 320. They use the nil test project and the fixed 2025-06-01 11:45–12:00 UTC
window; they are not production screenshot captures.

The signed widget uses the Slack profile. The failing KQL is `name ==`; the empty
comparison uses `name == "no-such-png-fixture-operation"`. `failure-dark.png` adds
`appearance=dark`; the other requests use light appearance. Failed requests return
`Cache-Control: no-store`. The valid empty result retains the successful bounded
query cache policy. The empty result now labels the absence of returned observations and hides its
meaningless numeric axis. The requested time bounds remain visible.

Reproduce with Node/Bun dependencies installed and the local test services running:

```sh
mkdir -p /tmp/slack-handler-png
SLACK_PNG_CAPTURE_DIR=/tmp/slack-handler-png \
  DB_HOST=127.0.0.1 MINIO_ENDPOINT=http://127.0.0.1:19000 \
  TEST_MATCH=PNG make live-test-dev
```

The watcher builds the real PNG renderer first. This test covers signature
verification, malformed-query handling, image output, and cache headers. It does
not establish live Slack image-proxy behavior or backend-outage recovery. The
request parameters are specified above and in the test; this capture does not
include a serialized query result or final ECharts options.
