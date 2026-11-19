# Anonymous usage telemetry

Self-hosted Monoscope sends one anonymous usage report a day to Monoscope. We use it to learn which
versions, deployment types and features are in use, so we know what to support and what to fix first.
It never decides whether a feature works: turning it off changes nothing else.

## Turning it off

Set either variable on the Monoscope server, then restart it:

```bash
MONOSCOPE_TELEMETRY_DISABLED=1
DO_NOT_TRACK=1    # the cross-tool convention; any value but empty, 0 or false
```

`MONOSCOPE_TELEMETRY_DEBUG=1` keeps reports on this machine: each one is written to the log and to
`apis.instance_telemetry.last_payload` instead of being sent. The server logs at startup whether telemetry is on, off or in debug mode.

## What a report contains

```json
{
  "schema": 1,
  "instance_id": "6f1c2b8e-3a4d-4e5f-9a7b-1c2d3e4f5a6b",
  "instance_created_at": "2026-11-19",
  "version": "08f22a8",
  "environment": "prod",
  "deployment": "docker",
  "os": "linux",
  "arch": "x86_64",
  "counts": {"projects": "1-5", "users": "6-25", "monitors": "6-25", "dashboards": "1-5"},
  "usage24h": {"events": "100k-1M", "metrics": "0"},
  "features": {"ai": true, "slack": false, "github_app": true, "timefusion": true}
}
```

| Field | Meaning |
| --- | --- |
| `instance_id` | A random UUID generated once, in this instance's database. It identifies the install, not a person or machine. |
| `instance_created_at` | The day that id was generated. |
| `version` | The build's git commit (`GIT_HASH`), or `dev`. |
| `environment` | `ci` when CI variables are present, `dev` in development mode, otherwise `prod`. |
| `deployment` | `kubernetes`, `docker` or `binary`. |
| `os`, `arch` | The server's operating system and CPU architecture. |
| `counts`, `usage24h` | Projects, users, monitors and dashboards, and the last day's events and metrics, as ranges such as `1-5` or `100k-1M`, never exact numbers. |
| `features` | Whether AI, Slack, the GitHub App integration and TimeFusion are configured. |

**Never sent:** hostnames, URLs, IP addresses, emails, user or project names, queries, attribute keys or
values, and any telemetry your applications send to Monoscope.

## How it is sent and stored

- One report a day, at a random time in the first six hours after the daily job runs, plus one a few
  minutes after the server starts if none went out in the last 20 hours. Only one report a day is sent,
  however many replicas run.
- It is an HTTPS `POST` to `https://app.monoscope.tech/telemetry/v1/report` (`TELEMETRY_ENDPOINT`), with a
  five-second timeout. A failure is logged at info level and not retried, so air-gapped installs are
  unaffected.
- Monoscope stores each report as one event in its own project. The sender's IP address is used only to
  look up its country and is never stored. No third-party analytics service receives reports.

## Seeing what was sent

The last report built and when one was last sent (or attempted) are kept in this instance's database:

```sql
SELECT last_sent_at, jsonb_pretty(last_payload) FROM apis.instance_telemetry;
```

Every report is also logged at info level (`Usage report sent`) as it goes out.

## Changes

New fields are added only with a new `schema` number and an entry here.

- **Schema 1:** first version.
