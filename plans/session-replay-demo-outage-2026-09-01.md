# Session replays unplayable on the demo project — 2026-09-01

Reported: "even 5min old screen replays are not playing. saying cant play this
session", on
`app.monoscope.tech/p/00000000-…/log_explorer?session_replay=fb42636e-…`.

Two independent defects. The replay pipeline itself is healthy.

## 1. The demo project has recorded nothing since 2026-08-31 00:19 UTC

`projects.replay_sessions`, sessions per hour:

| hour (UTC) | demo `00000000` | `6297304f` | self `87576849` |
|---|---|---|---|
| 08-30 07:00 … 08-30 23:00 | 143–154 each hour | 1–18 | 0–3 |
| 08-31 00:00 | **46** | 2 | 6 |
| 08-31 01:00 → 09-01 06:00 | **0 every hour** | 1–81 | 0–12 |

A hard stop at `2026-08-31 00:19:38 UTC`, demo-only. Other projects never
stopped, so nothing in ingest, Kafka, MinIO/R2 or the merge cron is at fault.

Meanwhile the demo *is* still producing sessions — 140 distinct
`attributes.session.id` in the last hour, service `frontend-web` (the browser
SDK) at ~130/hr, which matches the old recording rate almost exactly. So the
browser is loading pages and tracing them; only the rrweb upload stopped.

### Root cause: the demo frontend's API key is not in the database

`otel-demo` helm values, `components.frontend.envOverrides`:

    MONOSCOPE_API_KEY   = xvNPecJKbi4z…   (48 chars)
    MONOSCOPE_APP_URL   = https://app.monoscope.tech
    MONOSCOPE_REPLAY_SAMPLE_RATE = 1

That value matches **no row** in `projects.project_api_keys` — not an inactive
one, not a soft-deleted one, none. `getProjectIdByApiKey` is an exact
`key_prefix =` match, so the replay upload gets 401:

    POST https://app.monoscope.tech/api/v1/rrweb
      Authorization: Bearer xvNPecJKbi4z…   ->  401 "Invalid API key"
      X-Project-Id: 00000000-…              ->  200 {"status":"ok"}   (demo bypass)
      Authorization: Bearer <demo project's own key>  ->  200

Traces are unaffected because they take a different path: the browser exports to
`PUBLIC_OTEL_EXPORTER_OTLP_TRACES_ENDPOINT=http://localhost:8080/otlp-http/v1/traces`
(frontend-proxy → cluster collector → monoscope agent), which never presents
`MONOSCOPE_API_KEY`. Only the replay POST goes direct to `MONOSCOPE_APP_URL`
with that key. Hence: traces flow, recordings 401 — and the failure is
invisible in the product.

### Pipeline verified healthy end to end

A hand-made two-event POST with the demo bypass landed a row in
`projects.replay_sessions` with one `file_key` within seconds
(`80b01381-fa85-4e33-a337-2d0b4a81a836`). Ingest → Kafka → S3 → DB works.

Old demo recordings are also intact: `1c7b1c0a-…` (the newest, 08-31 00:19) has
its four `file_keys` present in the bucket at the expected sizes. Retention is
30 days; the demo's oldest row is 08-27, i.e. the recorder ran for about four
days and then stopped.

### Fix

Point `MONOSCOPE_API_KEY` at a key that exists for project `00000000-…`. Both of
that project's keys are active and both authenticate.

Done in two places, so a redeploy can't undo it:

1. **Live** — `helm upgrade otel-demo open-telemetry/opentelemetry-demo --version 0.41.0`,
   now at revision 40. New demo replay sessions appeared within a minute.
2. **Source of truth** — `monoscope-k8s/otel-demo-images.yaml` in
   `monoscope-tech/opentelemetry-demo`, which is where the deploy command in
   [[otel-demo-fork-deploy-pipeline]] reads the frontend `envOverrides` from.
   That file is gitignored (the tracked `…images.example.yaml` carries
   `REPLACE_WITH_RUM_KEY`), so the key stays out of git.

Verified the documented command reproduces what is live:
`helm template … -f otel-demo-overlay.yaml -f otel-demo-images.yaml` renders the
working key, zero occurrences of the dead one, and 25 frontend env entries — i.e.
trap 2 (`useDefault.env` not merging) did not bite.

Still unexplained: why it worked until 08-31 00:19 with a key that is absent
today. The key was not rotated (both demo keys' `updated_at` is 2025-06-30) and
the frontend pod predates the stop, so the credential changed *underneath* a
running pod. Most likely the row was hard-deleted some time earlier — the row is
gone entirely, not soft-deleted, and `project_api_keys` cascades on project
delete — while `getProjectIdByApiKey`'s `projectKeyCache` kept serving the
cached hit; 00:19 would then be a server restart flushing the cache rather than
the deletion itself. Checkable against monoscope's own service-start logs.
Whatever removed that row can remove the one just installed, so a recurrence
looks like this: traces keep flowing, recordings stop dead, nothing errors.

## 2. The log explorer offers Replay for sessions that were never recorded

This is the bug the report actually hit: `fb42636e-…` is a live demo session
(4,577 spans, five minutes old) with no recording, and the UI still handed the
user a Replay button into an empty player.

`Telemetry.identitySummary` stamps a `session;…⇒<id>` tag on **every** row whose
attributes carry `session.id`, and `log-list.ts` turns that tag into the Replay
button. But `generateSummary` runs at **ingest** time — before a recording can
possibly exist, and for every SDK that sets `session.id` whether it records or
not. So the affordance was never gated.

The Sessions tab was gated (`cff87aeb7`, 2026-08-13,
`hasReplay` ← `projects.replay_sessions`) and RUM gates via `mergeSessions`. Two
call sites were not:

- the plain logs/spans list (fixed below);
- **`Anomalies.hs:265`** — the issue detail page reads
  `attributes___session___id` straight out of telemetry and `whenJust
  replaySession` renders the player unconditionally. Still ungated; needs the
  same existence check before the panel renders.

Fixed in `selectLogTable` (`gateReplayTags`): after the page is materialised, the
session ids in its `summary` column are looked up once against
`projects.replay_sessions`, and the tag is dropped from rows that have no
recording. One extra Postgres query per page, only when session tags are
present. Non-UUID session keys (backend SDKs without `setSession`, where the
session is derived from user id/email) can never have a recording, so they are
never asked for and never touched.

The detail panel is unaffected: it regenerates the summary from the row and
already skips `session;` (`Utils.summaryForDetailView`).

Known gap: live-tail rows arrive with their ingest-time summary and bypass
`selectLogTable`, so a streamed row still carries an ungated button until the
next fetch. Harmless while the demo records every session (sample rate 1), but
it is the reason the gate cannot live purely at read time forever.

## Not changed

The player's error copy. "No recorded events found for this session" is
accurate; the headline "Can't play this session" reads as a platform failure
rather than an absence, but with the affordance gated the user should not reach
that screen from the log explorer any more.
