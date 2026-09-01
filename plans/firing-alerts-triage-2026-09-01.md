# Triage of the alerts firing into Slack — 2026-09-01

Two alerts were named. They have nothing in common: one is a real product defect
affecting every user whose session expires with a tab open, the other is a
developer's laptop paging the team.

## 1. `SyntaxError` — real, fixed

> Failed to fetch new data: Unexpected token '<', "<!DOCTYPE "... is not valid JSON
> Service: monoscope-ui · 133 days · 2,446 events in 3 days

**Root cause: the auth wall answers a `fetch` with a redirect to HTML.**

    GET /chart_data?…            (no session)
      -> 302 Location: /login?redirect_to=…
      -> 302 -> https://apitoolkit.eu.auth0.com/authorize?…

`fetch()` follows redirects transparently, so JavaScript is handed a **200 HTML
login page**. `res.ok` is true, `res.json()` hits `<!DOCTYPE` and throws. The
existing `res.ok` guard in `widgets.ts` cannot help: nothing about the response
is an error status.

A tab left open past session expiry then repaints "Couldn't load this chart" on
every refresh tick — the breadcrumb trail on one event shows the same message
every 4–5 minutes for hours — and never recovers, because nothing tells the page
to re-authenticate.

**Fix** (`Web.Auth.challengeFor`): classify the request and answer it in the
protocol it speaks.

| request | challenge | response |
|---|---|---|
| basic auth enabled | `ChallengeBasic` | 401 `WWW-Authenticate` (unchanged) |
| `HX-Request` | `ChallengeHtmx` | 401 + `HX-Redirect` (htmx navigates; a 302 makes it swap the login page into the fragment) |
| `Sec-Fetch-Mode` ≠ `navigate`, or `Accept: application/json` | `ChallengeJson` | 401 JSON |
| navigation, or a client too old to say | `ChallengeRedirect` | 302 (unchanged) |

`Sec-Fetch-Mode` is set by the browser on every request and cannot be forged by
script, so it is the reliable signal; the `Accept` check is a belt-and-braces for
clients that predate it. `widgets.ts` now also sends `Accept: application/json`
and **reloads on 401**, so an expired session lands on the login page instead of
painting an error forever.

Fixed alongside, visible in the same curl: **`redirect_to` was not
value-escaped.** `escapedQueryPartial` leaves `&` alone, so
`redirect_to=/chart_data?pid=…&since=1H` truncated at the `&` and post-login
landed on a URL missing every parameter after the first. Now `urlEncode True`.

Three regression tests in `Web.AuthSpec`, all verified red against the pre-fix
behaviour (`expected: 401, but got: 302`), plus doctests on `challengeFor`.

**How to judge the fix — not "the alert goes quiet".** The old message comes from
bundles already loaded in open tabs, which will keep failing the old way for days.
The metric is: no `Unexpected token '<'` from sessions that loaded the new bundle.
New-bundle failures say `widget request failed: <status>` instead.

Unverified: whether htmx's hover-preload extension acts on `HX-Redirect`. It
stores `responseText` without running htmx's header processing, so it should not
navigate on hover — worth one browser check with an expired cookie.

## 2. `HasqlException` — not a production defect

> Hasql session error: Server error … `WITH session_sources AS (…)`
> 90 days · fired 08:21 today

**Every occurrence comes from `service.name = monoscope-dev`** — a local
development server. 24 events in 3 days, zero from `srv-captain--monoscope`.

The SQL in the alert does not exist in the tree: `backfillSessionAttributes` was
rewritten to be per-project (the 57014 statement-timeout fix). The alert shows
the *first-seen* sample, frozen at 2026-06-02, and a dev checkout at an older
commit keeps re-firing the same fingerprint.

This is not one stray laptop. Over 24 hours, project `87576849` (monoscope's own
telemetry) received:

    srv-captain--monoscope   1,100,043
    monoscope-dev              475,061   <-- local dev servers
    timefusion-dev              37,137   <-- local TimeFusion

`.env` ships `OTEL_SERVICE_NAME=monoscope-dev` with the production collector
endpoint and the production project key, so **every developer's local server
writes half a million spans a day into the production self-telemetry project and
can page the team for errors that exist only on their machine.** Two ghcid
servers are holding :8080 on this box right now.

No code change made — the lever is `.env` (gitignored, per-developer), and which
way to jump is a call for the team:

- point dev at a separate project, keeping the dogfooding; or
- drop the OTLP endpoint from `.env.example` so dev exports nowhere by default; or
- keep it and accept that dev errors page.

The same shape explains `9c54680c` ("No files in log segment", 342 events, all
`timefusion-dev`).

## What the fix to #1 uncovered

Guarding `res.ok` turned a class of silent HTML-parse failures into real statuses,
and two new issues appeared today with the true cause:

- `eb59cb66` — `widget request failed: 504`, **84 events in 3 days**. A gateway
  timeout on `/chart_data`: some chart query is exceeding the proxy's limit. This
  was previously indistinguishable from the auth case. Worth its own pass —
  related to the sessions/infra query costs in
  `plans/infra-sessions-rum-performance.md`.
- `fe4bcd2e` — `widget request failed: 500`, 3 events.

## Firing but one-off, from prod TimeFusion — no action

One event each, all on 2026-08-31, all matching queries hand-run against prod TF
during the sessions-rewrite investigation the night before. Not recurring, and
each is a known DataFusion limitation already documented:

- `a89bdc54` — `No field named sv … sv."UNNEST(outer_ref(sb.svcs))"` — no lateral
  `unnest(t.col)`.
- `6dcbee7e` — `Conflicting ordering requirements in aggregate functions` — one
  `GROUP BY` cannot carry aggregates with differing `ORDER BY`.
- `f077c73c` — `time_bucket does not support month or year intervals`. This one
  is genuinely user-facing if anyone writes a monthly `bin()`; the error message
  is good, but the KQL layer should reject it before TimeFusion does.
