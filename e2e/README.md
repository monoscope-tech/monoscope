# End-to-end tests

Playwright specs that drive the real server in a real browser. They exist to catch the
class of bug the unit suites structurally cannot see — a component reading a response key
the server never sends, a renamed element id, a suggestion path that works in isolation and
not through the live editor. Every one of those has actually shipped here.

## Do not point these at port 8080

`playwright.config.ts` defaults to **`http://localhost:8081`**, deliberately.

Port 8080 is what `make live-reload` serves, and the dev server reads `.env` — whose
`DATABASE_URL` points at `monoscope-prod-eu-pg` with `MIGRATE_AND_INITIALIZE_ON_START=True`.
These specs create dashboards, drag and resize widgets, and POST to `stripe_checkout`. Run
them against 8080 with the watcher up and that goes into **production**.

If a bare `npx playwright test` fails with connection refused, the fix is to start an e2e
server (below) — *not* to point `E2E_BASE_URL` back at 8080.

## Running them

```bash
make test-e2e                       # whole suite
scripts/e2e.sh tests/live-tail.spec.ts
scripts/e2e.sh -g "suggests"        # by title
E2E_KEEP=1 scripts/e2e.sh           # leave the server up to poke at
```

`scripts/e2e.sh` creates `monoscope_e2e` if absent, starts the server on 8081 from a temp
directory whose `.env` is built from `.env.example` (so no real credential is ever copied
and the production `.env` is not on its path), waits for `/ping`, runs Playwright, and tears
everything down. Verified from a dropped database and from a warm one — both green.

Three settings in that generated env are load-bearing, and all three were found the hard
way: `BASIC_AUTH_ENABLED=False` (otherwise every request 401s), `ENABLE_FREETIER=True`
(otherwise the plan picker renders two cards and `#freePricing` never appears), and
Pub/Sub + background jobs off (so a test server cannot join production topics).

No manual seeding is needed. Migration `0001` creates the demo project the specs drive
(`DEMO_PROJECT` = `00000000-0000-0000-0000-000000000000`, see `tests/helpers.ts`), the
schema endpoint derives its fields without telemetry, and specs that need richer state build
it themselves — `dashboard-grid.spec.ts` creates its own dashboard from a template rather
than opening "whichever dashboard sorted first", which is what previously made it pass only
on a database an earlier run had already populated.

## Rebuild the frontend before running

`BodyWrapper` Template-Haskell-splices the Vite manifest's content hash at **compile time**,
so a `vite build` without rebuilding the server leaves the page requesting the previous
bundle. If the browser 404s an asset or runs stale JS, rebuild the server binary too —
`scripts/e2e.sh` runs whatever `cabal list-bin monoscope-server` points at, so a server-side
fix is not under test until that binary is rebuilt.
