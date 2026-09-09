# Slack monitor chart evidence

This controlled integration fixture comes from the scalar-monitor notification test
in `test/integration/MonitoringSpec.hs`. It records 84 at 11:55 UTC and 100 at
12:00 UTC on June 1, 2025. A null at 11:56 breaks the line across missing checks.
The requested window remains 11:45–12:00 UTC. No readings are invented before 11:55.

`request.json` contains the notification-derived widget, its dataset, renderer
input, and a URL re-signed with the public key `slack-chart-fixture-key` for the nil
fixture project. It contains no installation secret. This is not a captured
production request or evidence from the original screenshots.

`options.json` and `chart.png` are the actual CLI output. The `-dark` variants use
the same renderer input with `darkMode: true`. Formatter functions are retained as
strings in the options. Both PNGs were visually inspected at 960 × 320.

To recapture the producer input with local test services:

```sh
SLACK_CHART_FIXTURE_PATH=/tmp/monitor-chart-fixture.json \
  DB_HOST=127.0.0.1 MINIO_ENDPOINT=http://127.0.0.1:19000 \
  TEST_MATCH=Monitoring make live-test-dev
```

After the monitoring tests pass, copy the capture to `request.json`. To replay one
image from the repository root (Bun and the web-components dependencies required):

```sh
python3 -c 'import json,sys; json.dump(json.load(open("web-components/test/fixtures/slack-monitor-gap/request.json"))["renderer_input"],sys.stdout)' |
  DEBUG_CHART=1 bun web-components/src/chart-cli.ts > /tmp/monitor-chart.png 2> /tmp/monitor-chart-options.log
```

The log contains `INPUT:` and `FINAL OPTIONS:` JSON sections. The renderer tests
also reuse these captured options with negative readings and an above-range
threshold. Synthetic sparse bar tests check the time mapping in ECharts itself.
The installed ECharts 6.1 time-axis shape containment otherwise expands the range
by a data band, despite explicit min/max. The Slack profile disables that expansion
and reserves margins for boundary bars.

Still outstanding: production screenshot captures, explicit coverage labels,
empty and failed-query states, measurement units, and desktop/mobile Slack checks.
This fixture does not establish those acceptance criteria.
