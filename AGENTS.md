# AGENTS.md

This file provides guidance to coding agents working in this repository.

## Working Principles

1. **Think before coding.** State assumptions; if multiple interpretations exist, present them — don't pick silently. If a simpler approach exists, say so. If something is unclear, stop and ask.
2. **Simplicity first.** Minimum code that solves the problem: no speculative features, no abstractions for single-use code, no unrequested configurability, no error handling for impossible scenarios. If 200 lines could be 50, rewrite.
3. **Surgical changes.** Don't "improve" adjacent code, comments, or formatting; don't refactor what isn't broken; match existing style. Remove only orphans YOUR changes created; mention (don't delete) pre-existing dead code. Every changed line should trace to the user's request.
4. **Goal-driven execution.** Turn tasks into verifiable goals ("fix the bug" → "write a failing test, make it pass"); for multi-step work state a brief plan with a verify step per item, then loop until verified.

## Project Overview

Monoscope is an open-source observability platform (Haskell, GHC 9.12.2) that ingests logs, traces, and metrics via OpenTelemetry and stores them in S3-compatible storage via TimeFusion (custom time-series DB backed by PostgreSQL/TimescaleDB). Frontend is server-side rendered HTML (Lucid + HTMX) with TypeScript web components (Vite).

### Storage migration: PG + TF dual-write is temporary

Telemetry is currently dual-written to both Postgres (`otel_logs_and_spans`) and TimeFusion. **The end-state is TimeFusion only** — Postgres is the legacy store and is being phased out as TF features reach parity. When designing changes that touch the dual-write path (e.g. `bulkInsertOtelLogsAndSpansTF`, ingestion durability, idempotency), bias decisions toward TF correctness and treat PG as best-effort. Don't invest in PG-side complexity (e.g. `ON CONFLICT` ceremony, PG-specific reconciliation jobs) that will be deleted with the migration.

## File Structure

The repo is three cabal packages (see `cabal.project`):

- **`monoscope`** (root) — the server: library + `monoscope-server` exe + test suites.
- **`monoscope-shared`** (`shared/`) — DB-free modules the server and CLI both
  need: wire types, the KQL grammar, the HTTP effect, CLI formatting. **Nothing
  here may import postgresql-simple, hasql, librdkafka or grpc** —
  the CLI links it and must stay free of native dependencies.
- **`monoscope-cli`** (`cli/`) — the `monoscope` binary. Depends on
  `monoscope-shared` only, **never on `lib:monoscope`**. `scripts/check-cli-linkage.sh`
  fails the release build if that slips.

`monoscope-cli` appears in `lib-deps.yaml` purely so the `test-dev` target (which
compiles `src/` and `test/integration/` as one GHCi unit) can build the CLI
specs — the same convention the other test-only deps there follow. **Never
`import CLI.*` from `src/`**: it would compile, and it would put the server's
Brick/Vty-laden CLI back inside the server.

Both non-root packages have hand-written `.cabal` files (hpack only generates
the root one); add new modules to their `exposed-modules` by hand.

**Build the CLI with `make cli-build`, not a bare `cabal build … --ghc-options=…`.**
`monoscope-shared` is also loaded by the ghcid session behind `make live-reload`;
rebuilding it at a different optimisation level swaps the dylib underneath GHCi
and the next reload dies with `symbol not found in flat namespace` (restart the
pane with `make tmux-live-reload` if you hit it).

```
monoscope/
├── app/Main.hs                # Entry point — calls Start.startApp
├── shared/src/                # monoscope-shared: DB-free, shared with the CLI
│   ├── Pkg/Deriving.hs        # pure DerivingVia wrappers + ToSchema (no DB)
│   ├── Pkg/Parser/{Expr,Stats}.hs  # KQL grammar
│   ├── Pkg/CLIFormat.hs, Data/Effectful/Wreq.hs
│   ├── Pages/Charts/Types.hs, Models/Telemetry/Schema.hs, Web/Wire.hs
├── cli/                       # monoscope-cli
│   ├── CLI/{Main,Commands,Core,Config,Resource,Validate}.hs
│   ├── CLI/{Chart,Dashboard,LogView,Table,UI}.hs  # terminal rendering
│   ├── exe/Main.hs            # thin wrapper passing Paths_monoscope_cli version
│   └── test/Main.hs           # renderer unit tests (make cli-test)
├── src/
│   ├── Start.hs               # App bootstrap, OTel init, server startup
│   ├── BackgroundJobs.hs      # odd-jobs background job definitions
│   ├── ProcessMessage.hs      # Pub/Sub & Kafka message processing pipeline
│   ├── Utils.hs               # Shared utilities
│   ├── System/                # Config.hs (EnvConfig), Server.hs (Warp), Types.hs (effect aliases), Logging.hs, Tracing.hs
│   ├── Web/                   # Routes.hs (Servant wiring), Auth.hs (auth/JWT/error rendering)
│   ├── Pages/                 # Server-rendered HTML (Lucid + HTMX): BodyWrapper (shell), Components, LogExplorer/, Charts/, Monitors/, Bots/, Endpoints/, Onboarding/, Dashboards, Settings, Reports, Replay, Anomalies, …
│   ├── Models/                # DB models & queries: Projects/ (Projects.hs = User/Project/Session/billing, ProjectApiKeys, ProjectMembers, Dashboards, GitSync), Apis/ (Endpoints, Monitors, Anomalies, LogPatterns, Fields, Integrations, Issues/), Telemetry/ (Telemetry.hs queries, Schema, SummaryGenerator), Users/Sessions.hs (re-exports)
│   ├── Pkg/                   # Shared libs: DeriveUtils (DB types/newtype/TH; re-exports Pkg.Deriving), Parser (KQL, Megaparsec), AI, Mail, Queue (Pub/Sub+Kafka), QueryCache, TestUtils, Drain, ErrorFingerprint, PatternMerge, GitHub, EmailTemplates, Components/, Parser/
│   ├── Opentelemetry/         # OtlpServer.hs (gRPC OTLP, port 4317), OtlpMockValues.hs
│   └── Data/Effectful/        # Custom effects: LLM, Notify, UUID, Wreq
├── test/                      # doctests/, unit/ (no DB), integration/ (needs DB), bench/
├── tests/golden/              # Golden files for external API mocking
├── static/                    # migrations/ (numbered SQL), public/ (CSS/JS/images/fonts)
├── web-components/src/        # TypeScript frontend (Vite): web components, chart-cli, workers
├── proto/opentelemetry/       # OpenTelemetry .proto files
├── package.yaml               # Hpack build definition (→ monoscope.cabal)
├── Makefile                   # Dev commands (build, test, lint, fmt, …)
└── docker-compose.yml         # TimescaleDB + app services
```

## Development Workflow

A `make live-reload` process is always running in a tmux pane, logging to `build.log`. Web component build logs are in `web-components.log` and CSS build logs are in `css.log`.

### Pull requests only

All Monoscope changes must go through a pull request, including documentation,
configuration, hotfixes, and deploy-related changes. Work on a non-`master`
branch, push that branch, and iterate by updating the pull request until its
review and required checks pass. Merge the pull request through GitHub; **never
push commits directly to `origin/master`**.

When a production deployment is needed, merge the pull request first and deploy
the resulting commit already present on `origin/master`. Do not use the deploy
workflow to bypass pull-request review.

### DO NOT run `cabal build` to verify your changes

**To check your changes compile, read `build.log`. Do not run `cabal build`, `cabal repl`, `ghc`, or any equivalent yourself.** ghcid recompiles automatically on every file save and streams the result into `build.log`; running `cabal build` in parallel races it, warms a second build cache, and wastes minutes of your time (and the user's) for information that is already sitting in the log file.

This applies to **every** edit, including the last one you just made. If you caught yourself about to run `cabal build`, stop and `Read` `build.log` instead.

**If `build.log` shows ghcid has crashed** (e.g. `cabal: semWait: invalid argument`, an empty tail, or a hang), restart the tmux pane — do **not** fall back to `cabal build`:

```bash
make tmux-live-reload
```

Then re-read `build.log` after a few seconds. The only times it's appropriate to invoke the compiler directly are: running the test suite (`make test-*`), running doctests, or running `hpack` after adding/removing `.hs` files.

## Build Commands

```bash
# Build (fast, dev)
cabal build lib:monoscope --ghc-options="-O0 -j8"

# Build all (lib + exe + tests)
cabal build all -j --ghc-options="-O0 +RTS -A64m -n2m -RTS"

# Run server
cabal run

# Live reload (ghcid)
make live-reload

# Type-check watch (ghciwatch)
make watch

# Format
make fmt          # fourmolu on src/**/*.hs

# Lint
make lint         # hlint src
make fix-lint     # hlint auto-refactor

# CSS (Tailwind v4 + DaisyUI v5)
make post-css     # one-shot build
make css-start    # watch mode (logs to css.log)

# Web components (Vite)
make web-components-watch  # watch mode (logs to web-components.log)
```

**NEVER run `cabal clean`** — it destroys all cached deps and dramatically increases compile time.

**Never put a `package *` stanza in `cabal.project.local`.** Its `ghc-options` feed
every dependency's `~/.cabal/store` hash, forking a private copy of the whole
dependency set per checkout — that is what used to make each worktree rebuild
everything from scratch. Scope dev flags to the named local packages
(`package monoscope` / `monoscope-shared` / `monoscope-cli`) instead. New
worktrees: `make worktree NAME=my-feature` (see the Makefile comment for what it
does and does not seed).

After adding/removing `.hs` files, always run `hpack` to regenerate `monoscope.cabal` from `package.yaml`.

### Dead Code Detection (Weeder)

Run `weeder` regularly to find unused functions and remove them. A `weeder.toml` config is in the repo root. Ignore results from these files (they contain intentionally unused/future-use code):

- `src/Data/Effectful/Wreq.hs` — HTTP method effect wrappers (used as needed)
- `src/System/Logging.hs` — logging helpers
- `src/System/Tracing.hs` — tracing helpers
- `src/Devel.hs` — dev-mode REPL helpers
- Auto-generated proto-lens files (`dist-newstyle/build/autogen/Proto/`)
- `cli/CLI/` — CLI tool (separate executable, weeder may not trace its root)

## CI, and running it locally

Use standard GitHub-hosted runners for remote jobs. Do not add Blacksmith runners or actions without an explicit user request.

### Run the reviews before updating the pull request

Before pushing a pull-request branch, run all three over the change and
**implement what they raise**:

```
/hs-distill          # size, reuse, algebraic style
/hs-evasion-review   # constraint-evading compromises
/hs-lob-review       # client-side tier & locality of behavior
```

Then **run them again on the result**, and keep looping until a pass comes back
with nothing material left — a fix routinely opens the next finding, so one
round is not a sign-off.

Once that loop has settled, run `/simplify` as the last pass over the final
diff, and apply what it finds. Then `make ci-signoff`, push the branch, and
update the pull request.

Not every finding has to be taken: if one is wrong or the cost outweighs it, say
so explicitly and move on. What is not acceptable is updating the pull request
without having run them.

### Sign off locally before updating the pull request — never push and wait on CI

**A push to `master` IS a deploy, and remote CI is far too slow to be a feedback
loop: an image build alone runs ~85 minutes.** So `make ci-signoff` must be green
*before* you push the branch. It runs the checks locally and publishes
attestations, which CI's gate then reuses — a signed-off pull-request update can
reuse those results instead of re-running everything.

```bash
make ci-signoff                     # everything, attesting each pass
make ci-signoff CHECKS="fmt lint"   # scoped to a limited change
```

This is a rule, not a preference: on 2026-09-08 five consecutive deploys failed,
each burning ~85 minutes of wall clock before reporting an e2e expectation that a
local run would have shown in minutes. Never treat "push it and see what CI says"
as a way to test a change.

**`make ship` is the whole deploy from here** — checks, image, push, CapRover —
so the slow half stops being a queue you wait on:

```bash
make ship          # checks → image → push → deploy, stopping at the first failure
make deploy-status # what production is running right now
```

It refuses to deploy a commit `origin/master` does not contain, and refuses when
any deploy-path check (`build doctests unit-tests cli-tests integration-tests
e2e`) lacks an attestation for that exact tree. CI still runs, but the gate skips
every attested check, the image is already in the registry, and the deploy job
asks `ci.sh deployed <sha>` and skips too. Do this **once per machine** so the
amd64 image build is native instead of emulated:

```bash
BUILD_HOST=ubuntu@your-amd64-box make builder-setup
```

Two things this does **not** cover, so check them yourself:

- **The image build is outside the fingerprinted checks** (it produces an
  artifact, not a verdict), so `ci-signoff` cannot prove it. When a change
  touches the `Dockerfile`, `static/`, or the web-components build, run
  `make deploy-image` locally first — that is the only local proof of the stage
  that has failed most often.
- **If a check genuinely cannot run here** (missing service or tool), say so in
  the PR/commit and let CI run that one. Never publish an attestation for a check
  that did not pass.

`ci/checks.tsv` is the single definition of what CI is; `.github/workflows/*.yml`
and `scripts/ci/ci.sh` both read it, so a check cannot exist in one and not the
other. Each check is fingerprinted over the content it depends on, and a pass
publishes an attestation under `refs/ci-attest/v1/*`. CI's gate job skips any
check already proven for the exact tree it is about to test — by an earlier run,
another branch, or a developer's `make ci`.

```bash
make ci          # run CI's checks in CI's own containers, attesting each pass
make ci-status   # what CI would run right now, without running any of it
make ci-selftest # fingerprint/capability logic + checks.tsv <-> run_body agreement
```

**Adding or changing a check means editing `ci/checks.tsv` and `run_body` in
`scripts/ci/ci.sh` together** — `make ci-selftest` enforces they agree, and also
that every declared input path exists (a typo silently narrows what a check
depends on, which is how an untested change ships).

Things that are load-bearing and easy to break:

- **Narrow a check's `inputs` only where it is provably sound.** Too wide costs a
  rerun; too narrow ships an untested change. The frontend bundles are in the
  Haskell pathset on purpose — BodyWrapper TH-splices their content hash.
- **Capabilities are the safety property.** A check declares what it needs
  (`ghc`, `pg`, `minio`, `tf-real`); an attestation records what the environment
  had. Reuse requires provided ⊇ required, so a stub-service run can never
  satisfy a check needing the real one. Never widen a `caps` string to make
  something pass.
- **Fingerprints are pinned once per run**, because steps rewrite the tree as
  they go (`hpack`, `npm ci`, `hlint --refactor`). Recomputing after those would
  attest a fingerprint no gate ever asked for.
- **Never let bookkeeping fail a green check.** Attestation publishing is
  best-effort; a network or git failure must not turn a passing suite red. This
  already happened once — `set -e` plus git's container-job "dubious ownership"
  reported a successful 16-minute build as a failure.
- **`docs/local-ci.md` is the reference** — every knob, the fidelity caveats, and
  the kill switches (`CI_ATTEST_DISABLED` repo variable, `EPOCH` in `ci.sh`).

The deploy image is deliberately **not** fingerprinted: it produces an artifact
rather than a verdict, so its cache is the registry and its identity is the
commit SHA. `build-image` skips when `ghcr.io/…:<sha>` already exists, and
`make deploy-image` lets a big machine pre-build it. Because an image cannot be
re-derived from source to check (Haskell builds are not bit-reproducible), that
path refuses a dirty tree and records who built it; the deploy job prints
`built by: …` every time.

## Testing

### NEVER run `cabal test` / `stack test` to verify tests

**Read `build-test-dev.log`.** `make live-test-dev` is the way to run and iterate on the test suite in this repo. It runs as a ghcid watcher (in a tmux pane or as a backgrounded process), compiles `src/` + `test/integration/` as a single GHCi target (no library re-link between edits), and re-runs the suite automatically on every file save, streaming results into `build-test-dev.log`.

This is the **same** rule as `cabal build` vs `build.log` — running `cabal test` in parallel re-links the whole library, races the watcher, warms a second build cache, and wastes minutes of your time and the user's for information that's already in the log file.

```bash
make live-test-dev                              # full integration suite, auto-reruns on save
TEST_MATCH=/MonitoringSpec/ make live-test-dev  # filter by spec name
```

If you catch yourself about to run `cabal test integration-tests` or `cabal test --test-options=...`, **stop and read `build-test-dev.log` instead.** It applies to every test verification, including the one you just decided was "quick" or "scoped." If the watcher isn't running, start it (`make live-test-dev` in the background or `make live-test-dev | tee build-test-dev.log` in a tmux pane); do **not** fall back to a one-shot `cabal test`. If the watcher seems stuck or crashed, restart it the same way — never invoke the compiler directly.

The one acceptable exception is a CI-style reproduction with a fixed seed (e.g. chasing a flake) — and even then, prefer rerunning under live-test-dev with `TEST_MATCH` first.

### Other test commands (rarely needed)

```bash
make test              # all tests (requires DB) — heavy; only for full CI parity
make cli-test          # CLI renderer tests (no DB, milliseconds)
make test-unit         # unit tests (no DB)
make test-doctests     # doctests
make test-integration  # integration tests (requires DB)

# Live watching for non-integration targets
make live-test-unit
make live-reload-doctests
```

The `test-dev` stanza in `package.yaml` shares its dependency list with the
library via `!include hpack-includes/lib-deps.yaml` — when you add a library
dep, edit that file and both targets pick it up. (hpack can't splice list-position
includes, so a few test-only deps — hspec, uuid-quasi, ki, pg-transact,
aeson-pretty, process — also live in `lib-deps.yaml`; the library declares
them unused, silenced by the global `-Wno-error=unused-packages`.)

**The container runtime on macOS is [OrbStack](https://docs.orbstack.dev/quick-start), not Docker Desktop.** It supplies the `docker`/`docker-compose` CLIs and its own `orbstack` docker context, so every `docker …` command and `make` target in this file works unchanged — but Docker Desktop is uninstalled, so anything that assumes `/Applications/Docker.app`, the `desktop-linux` context, or the `desktop` credential helper will fail. If `docker` can't reach a daemon, start OrbStack (`open -a OrbStack`) rather than reinstalling Docker Desktop.

Integration tests require PostgreSQL+TimescaleDB at localhost:5432 (postgres/postgres). Start with:

```bash
make timescaledb-docker      # persistent
make timescaledb-docker-tmp  # ephemeral (tmpfs)
```

**`USE_EXTERNAL_DB=true` is what points the suite at that container — never drop it.**
Every DB-touching make target sets it (`test`, `test-integration`, `test-integration-tf`,
`live-test-dev`). Without it, `Pkg.TestUtils.withSetup` takes the `withLocalSetup` branch
and boots **tmp-postgres** from whatever `postgres` binaries are on `PATH` — on a Mac that
is homebrew's, which ships `timescaledb` but **not** `timescaledb_toolkit`. Migration
`0001` then fails on `CREATE EXTENSION IF NOT EXISTS timescaledb_toolkit`, the template
database never builds, and **every** example dies in setup with
`uncaught exception: SomePostgreSqlException`.

That failure looks exactly like a broken change. Tells that it is the flag instead:

- the error names an `/opt/homebrew/...` extension control file, and the Postgres log lines
  say `host=[local]` (a Unix socket — the container is reached over TCP)
- a spec your change did not touch fails identically
- `DB_HOST` makes no difference; it only applies to the external-DB branch

So: run the make target. If you invoke `cabal test integration-tests` directly, pass
`USE_EXTERNAL_DB=true` yourself — and before blaming a change for integration failures,
run one spec it did not touch.

**Golden files** (`tests/golden/`): integration tests cache external API responses. Update with `UPDATE_GOLDEN=true`.

**Testing is critical — always write tests for new features and bug fixes.** Prefer high-level e2e integration tests that exercise handlers or real user actions end-to-end (from HTTP request through DB and back). Tests should assert on responses as users would see them. Only use doctests for isolated pure functions. Avoid low-level unit tests that mock internals — test real behavior through the actual stack. Keep tests concise: no redundant tests, prefer single tests that cover multiple cases implicitly or explicitly rather than many narrow tests.

When you doctest a smart constructor or refined type, **doctest the invariant, not just a happy path** — pin both the round-trip and the rejection (e.g. `mkX bad == Left …`) so the guarantee the type promises can't silently regress.

**Bug-fix workflow (mandatory):**

1. **Reproduce the bug as a failing test first** — at the level closest to where the bug manifests (integration if it's a handler/DB issue, doctest if it's a pure function). The test must fail in a way that exposes the bug, not just "something errored."
2. **Then write the fix** — and confirm the test now passes.
3. **Keep the test in the suite as a regression guard.** Name it after the bug (e.g. `splitReplayPayload_emptyStringFields_doesNotDropMessage`) so future readers see what it's preventing. Reference the incident/issue in the test if relevant.
4. Skipping the failing-test step is **not** acceptable just because the fix "looks obvious" — the day's experience is full of "obvious" fixes that didn't actually cover the real path (e.g. fixing simple-query ABORT while Hasql uses extended-query). A failing test forces you to exercise the actual code path.

Test helpers in `Pkg.TestUtils`: `withTestResources` (pool, caches, auth), `toServantResponse` (for handler responses), `runQueryEffect` (for DB effects), `ingestLog`/`ingestTrace`/`ingestMetric` (OTLP ingestion).

- <http://localhost:8080/p/00000000-0000-0000-0000-000000000000/> project is a demo project which can be accessed with no auth.

## Architecture

### Effect System

Uses **effectful** for algebraic effects. Key types in `System.Types`:

- `ATBaseCtx` — base effect stack (no auth)
- `ATAuthCtx` — authenticated request context
- `DB es` — DB access constraint (`WithConnection :> es, IOE :> es`)

### Key Modules

- `Start.hs` — app bootstrap + OpenTelemetry init
- `Web/Routes.hs` — Servant route definitions
- `Web/Auth.hs` — auth + cookie/JWT
- `Pages/` — server-side HTML pages (Lucid + HTMX)
- `Models/Projects/Projects.hs` — User, Project, Session models
- `Pkg/Parser.hs` — KQL query parser (Megaparsec)
- `Pkg/DeriveUtils.hs` — DB types, newtype helpers, TH utilities
- `Pkg/TestUtils.hs` — test infrastructure
- `Opentelemetry/OtlpServer.hs` — gRPC OTLP ingestion (port 4317)
- `ProcessMessage.hs` — message processing pipeline
- `BackgroundJobs.hs` — odd-jobs background jobs

### Ingestion Pipeline

Data enters via gRPC OTLP (port 4317), Google Pub/Sub, or Kafka → `ProcessMessage.processMessages`

### Database

- Primary: PostgreSQL + TimescaleDB (pg-entity + postgresql-simple)
- Secondary: TimeFusion (S3-backed, PostgreSQL wire protocol)
- Migrations: `static/migrations/` (numbered, run on startup when `MIGRATE_AND_INITIALIZE_ON_START=True`)
- **Migrations are append-only — never edit an applied migration.** The runner checksums each file; changing one that already ran causes a checksum mismatch that silently halts the runner mid-list, so every later migration never applies. To change schema, add a new numbered follow-up migration.
- **Ordering is the filename, not the number — check the highest number on `master` before claiming one.** Two branches that both add `0125_*` merge cleanly and both apply, in `sort` order of the full filename (`0125_git_hosts.sql` before `0125_issue_ack_window.sql`). `0125` is already taken twice for this reason. It is only a naming hazard, not a live bug, and it is **not** fixable after the fact: renaming an applied file changes the checksum and halts the runner.

### Frontend

- Lucid (type-safe HTML) + HTMX for interactivity
- TypeScript web components in `web-components/` (Vite)
- Static assets in `static/public/assets/`

## Type & Effect Design

The type system is the primary tool for correctness here. Lean on it before writing runtime checks or tests.

- **Make invalid states unrepresentable.** Model the business domain in types so illegal combinations can't be constructed. Prefer sum types over boolean/`Maybe` soup (`data PlanState = Trial UTCTime | Active SubId | Cancelled UTCTime` over `isTrial :: Bool` + `subId :: Maybe SubId` + `cancelledAt :: Maybe UTCTime`). Push invariants into constructors via smart constructors + hidden data constructors (export the type, not its constructor; expose a `mkX :: … -> Either Err X` that validates once). If a function has to defend against a state the caller "shouldn't" produce, that state shouldn't be representable — fix the type, delete the guard.
- **Parse, don't validate.** Validate at the boundary (request decode, env parse, DB read) into a precise type, then trust that type everywhere downstream. A `NonEmpty`, a `ProjectId` newtype, or a refined `Email` carries its guarantee through the whole call graph — no re-checking, no "this is always non-empty here" comments. Use `newtype` aggressively to stop primitive obsession (`UUIDId`-style typed IDs are the existing pattern — extend it: don't pass bare `Text`/`Int` when a domain meaning exists).
- **Encode invariants in types, not comments.** A comment saying "callers must hold the lock" / "list is sorted" / "only call after auth" is a type that wasn't written. Reach for phantom types, `data`-kind tags, GADTs, or a capability argument before a comment. Totality matters: avoid partial functions (`head`, `fromJust`, partial record fields, incomplete `case`) — `-Wincomplete-patterns` is on for a reason.
- **Prefer `Eff` effects over raw `IO`.** A concrete `IO a` says nothing about what a function does; an `Eff '[DB, LLM, Time, Log] a` signature documents and *constrains* its capabilities — a function with no `Notify` in its row provably can't send notifications. Program against the smallest effect row that does the job (`(DB :> es, Time :> es) => Eff es a`, not `ATAuthCtx`-everything). New side-effecting capabilities should become an effect in `Data/Effectful/` (see `LLM`, `Notify`, `UUID`, `Wreq`, and `Time`/`TestClock` from PR #329), not an `IO` call buried in a handler.
- **`bracket`/`withX` for every acquired resource.** Connections, file handles, pool checkouts, temp state — acquire and release inside `bracket`/`finally`/a `withX` wrapper so an exception can't leak the resource. Never manual acquire-then-release in sequence; the ingestion pipeline and DB pools make a leaked handle a production incident, not a tidiness issue.
- **One canonical interpreter pair per effect.** When you add an effect in `Data/Effectful/`, ship both interpreters in that same module: the production one and the test/fake one. "How do I fake this in a test?" should never require hunting — the deterministic interpreter lives next to the effect by construction.
- **Effects make tests reproducible.** The reason to define an effect is the test interpreter: non-determinism (clock, UUID, randomness, HTTP, LLM) goes behind an effect so tests run a pure/fake interpreter and assert deterministically (`TestClock` for `NOW()`, golden interpreters for external APIs). If you find yourself wanting to mock or stub something in a test, that something should be an effect. Never reach for `IO`-level seams (global `IORef`, `unsafePerformIO`, env-var toggles) where an effect interpreter is the right seam.

## Conventions

- **Client-side escalation ladder — lowest tier that stays clear wins.** Plain HTML → Tailwind CSS state variants (`group-has`, `peer-has`, `has()`, `:checked`) → HTML + concise declarative attributes (htmx, `hx-live` bindings) → hyperscript. **Hyperscript is a concise DSL that will always remain in this codebase where it is the clearest, most concise solution** — `send X to <sel/>`, `halt`, class add/remove one-liners, cross-element listeners, keyboard handlers, behaviors. Never translate a hyperscript one-liner 1:1 into an `hx-on:*` raw-JS string (`document.getElementById(...)` chains); that swaps a readable DSL for wordier JS and is a regression. Replace hyperscript only when the replacement is strictly simpler and more concise: a CSS variant, a declarative binding, or JS that collapses multi-line DSL into one clear line.
- **Locality of behavior.** Keep behavior at the site where it is used — inline hyperscript, event handlers, and short scripts directly on the element rather than naming them in a `where` clause or module-level binding. A reader should be able to read a button and immediately see what it does without jumping elsewhere. Named helpers are justified only when the same logic is reused across multiple call sites *and* the name adds clarity; single-use extractions trade readability for false DRY. This applies equally to HTML attributes, JS snippets, and HTMX directives: if it's only used once, keep it at the use site.
- **Lucid owns HTML; HTMX owns HTML arrival.** Render UI markup in Lucid whenever possible — never construct HTML strings in JavaScript. For async content, render a stable Lucid shell/loading state, then have HTMX swap a dedicated child target with the response. JavaScript may only coordinate state or interop; it must not become a second rendering system.
- **Conciseness is the top priority.** Minimize files and lines of code. Use advanced Haskell techniques (generics, `DerivingVia`, `DeriveAnyClass`, Template Haskell, type-level programming), GHC extensions, and libraries to eliminate boilerplate. Prefer editing existing code to cover new functionality over creating new files. Write general functions that solve multiple use cases. Advanced techniques are encouraged when they reduce code. Prefer fewer, larger files over many small ones.
- **Be succinct and concise — in code, in comments, in PR descriptions, and in replies.** Keeping code small is a priority on its own. Drop helpers used once, prefer point-free where it stays readable, skip restating what the code already shows, and don't add prose where a one-line note suffices. Same applies to commit messages and review responses.
- **Prefer external packages over in-house — always.** Reaching for a maintained library is the default, not the fallback: from one-line primitives (`BS.filter`, `bimap`, `ordNub`, `partitionEithers`, `eitherDecodeStrict'`) up to whole subsystems. If a one-line standard call replaces an in-house helper at the call site, inline it and delete the helper. Helpers should earn their keep across many call sites — single-use shims that rename or lightly wrap a library function belong nowhere. Before writing non-trivial logic, check Hackage/Stackage for an existing package; adding a dependency almost always beats hand-rolling and maintaining the equivalent. Fewer lines is a goal in itself — leaning on `base`/`relude`/`aeson`/`bytestring` and the wider ecosystem is how we get there.
- **Prefer concrete typed handler return types over opaque `Html ()`.** A handler that returns `RespHeaders MyPageGet` (a newtype carrying `PageCtx` + the data it renders) is testable — tests can inspect the payload structurally, golden-test the rendered HTML, and refactor the renderer independently. A handler that returns `RespHeaders (Html ())` collapses the data and view into one opaque blob you can only assert against as a string. **Do not "simplify" typed-newtype handlers (`ApiGet`, `ManageMembers`, `CreateProject`, …) down to `Html ()` for the sake of LOC.** When introducing new page handlers, give them a typed wrapper carrying the data the renderer needs, with a `ToHtml` instance that calls the render function. Helpers like `withSettingsPage` are fine for handlers that already return `Html ()`, but don't use them as a reason to flatten typed handlers.
- **No duplicate functions — extend, don't fork.** When you need a slightly different version of an existing function (e.g. one that returns more fields, takes an extra filter, or aggregates more columns), extend the existing function and update its callers. Do **not** add a parallel `fooWithBar` / `fooV2` / `getXAndY` next to `getX`. Two near-identical SQL queries / handlers / helpers will drift, and a future reader has to diff them to figure out which one to use. Widening a return type or adding an argument is almost always cheaper than maintaining the second copy.
- **Extend the shared component; never build a one-off beside it.** The same rule as "no duplicate functions", applied to components (`Widget`, `Table`, `TimePicker`, `emptyState_`, the page shells). When a component *almost* does what a page needs, the answer is to add the capability to the component so every caller gets it — **not** to hand-roll a bespoke HTMX fragment, a private renderer, or a page-local variant next to it. "It only renders that server-side" / "that path doesn't populate it client-side" is a description of a gap to close in the component, not a reason to reinvent it one page over. A capability added to `Widget` is one implementation, tested once and available everywhere; a page-local copy is a second implementation that will drift and that the next reader has to diff. Reach for a new surface only once you have established the shared one genuinely cannot be extended to cover the case, and say why in the commit.
- **Relude** as custom Prelude (`NoImplicitPrelude` + `import Relude`)
- **GHC2024** language standard (includes `DataKinds`, `DerivingStrategies`, `GADTs`, `LambdaCase`, `RoleAnnotations`, etc.) with additional extensions in `package.yaml`
- **Qualified imports** for most non-Prelude modules
- `-Weverything -Werror` with selective `-Wno-*` suppressions
- `UUIDId` newtype for typed IDs (e.g., `ProjectId`, `UserId`)
- **Comments: concise docs that carry information not in the code, or none at all.** A comment earns its lines only by stating what the code cannot — an invariant, a wire contract, why a surprising choice was made. Comments that restate the code, narrate a change, or pad an obvious helper are bloat; delete them rather than polish them.
- **Record dot syntax** — always use `foo.bar` (OverloadedRecordDot) instead of accessor functions `bar foo`
- **No record field prefixes** — use bare field names (`key`, `since`, `expression`) not prefixed names (`csKey`, `mqSince`, `mcExpression`). `DuplicateRecordFields` is enabled to allow the same field name in multiple records within a module.
- **Always derive instances** — never write manual `ToJSON`/`FromJSON`/`ToField`/`FromField` instances when deriving can work. Use `deriving-aeson` (`CustomJSON`, `FieldLabelModifier`, `StripPrefix`, `CamelToSnake`, `OmitNothingFields`) for JSON. Use `WrappedEnumSC` (from `Pkg.DeriveUtils`) for enum-like types needing `ToJSON`/`FromJSON`/`ToField`/`FromField` with snake_case constructor stripping. Prefer `DerivingVia`, `DeriveAnyClass`, and `deriving newtype` over manual instances.
- **SVG icons via sprite sheets** — render icons with `faSprite_ "icon-id" "solid" "w-4 h-4"` (or `"regular"`). The icon id must exist as a `<symbol>` in `static/public/assets/svgs/fa-sprites/solid.svg` or `regular.svg`; referencing a missing id silently renders nothing. We have a **FontAwesome Pro** subscription (kit token in `.env` as `FONTAWESOME_PRO_TOKEN`). To add a new icon:
  1. Find the icon name at fontawesome.com (e.g. `arrow-right`, `circle-check`). Check the style: `solid` or `regular`.
  2. Run `make fa-add ICON=icon-name STYLE=solid` (or `STYLE=regular`). This appends the `<symbol>` to the matching sprite file automatically.
  3. Use `faSprite_ "icon-name" "solid" "w-4 h-4"` in Haskell.

  `make fa-add` (→ `scripts/fa-add.py`) fetches from the **FontAwesome Pro** API first (using `FONTAWESOME_PRO_TOKEN`, read from env or `.env`), falling back to the FA 6.7.2 **free** CDN only if the token is absent or the icon isn't in Pro — so Pro-exclusive icon names download with no manual steps. The token is env-only; never hard-code it. Sprite files are cache-busted automatically via `hashAssetFile`.

  Only `solid` and `regular` sprite files are wired into `faSprite_` (Utils.hs) today. To use a new Pro *style* (`light`/`thin`/`duotone`), `make fa-add` will download it into `<style>.svg`, but you must also add a matching `hashFile` arm to `faSprite_`'s `case faType` for cache-busting to be correct.
- **`Text`/`ByteString`, never `String`.** Relude already hides the `String`-based Prelude — keep it that way: `Text` for human/textual data, `ByteString` for bytes/wire, and use `OverloadedStrings` literals. Don't reintroduce `String` in signatures or `++`/`String`-returning helpers; if a library hands you `String`, convert at the boundary (`toText`/`encodeUtf8`).
- **Let the signature carry the cardinality invariant.** When a function genuinely requires ≥1 element, take `NonEmpty a`, not `[a]` + a runtime "shouldn't be empty" guard. Same for other shape constraints — push them into the argument/return type (`NonEmpty`, a newtype, a refined value) so the caller proves the invariant once and downstream code stops re-checking. This is "parse, don't validate" applied at every signature, not just the request boundary.
- **Exhaustive `case`, no catch-all `_ ->` on your own sum types.** Spell out every constructor. A wildcard silently swallows variants you add later; explicit arms turn "added a constructor" into a compile error that points you at every site that must handle it. This is what makes invalid-states-unrepresentable hold up over time. (`_ ->` is fine on truly open/foreign enums where you genuinely don't care about the tail.)
- **Derive traversals; don't hand-roll them.** You already mandate deriving for JSON/DB — extend it: if you're pattern-matching a structure just to map/fold/collect over it, derive `Functor`/`Foldable`/`Traversable` (or go via `Generic`) instead of writing the recursion by hand. Less code, and it can't drift out of sync with the type.
- **`ki` structured concurrency over raw `forkIO`.** We already depend on `ki`; use it. Scoped threads don't leak on exception and propagate failures to the parent — raw `forkIO` orphans threads and swallows errors (we've had shutdown incidents from exactly this). No bare `forkIO`/`async` where a `ki` scope fits.
- **`fourmolu` + `hlint` before every commit.** `make fmt` and `make lint` are not optional — run them (or rely on the watcher) so no commit introduces formatting churn or lint regressions.
- **`generic-lens` for nested record updates.** `generic-lens` is already a dep — use it (`#field` / `field @"x"`) for deep updates on `PageCtx`, config, and other nested records instead of verbose record-wildcard rebuilds. Shorter and it can't silently desync from the type.
- **`witch` (`From`/`into`) for boundary conversions** (add the dep) — one consistent conversion vocabulary instead of ad-hoc `toText`/`fromIntegral`/`pack`/`unpack` scattered across call sites. Define a `From` instance once at the boundary; convert with `into`/`from` everywhere after.
- **Secrets are env-only — never in code, logs, or commits.** API keys, tokens, encryption keys, DB credentials come from env (`System.Config.EnvConfig`) and nowhere else. Never hard-code them, never commit a real `.env`, and never log a secret value (see the observability "no secrets" rule).
- **Keep effect rows and the dep list lean.** `-Wredundant-constraints` is error-level — drop an effect/constraint from a signature the moment it's unused, don't let rows accrete. Run `weeder` as a standing habit (not just "occasionally") to delete dead functions, and prune unused entries from `lib-deps.yaml`/`package.yaml` as they fall out of use.
- **Typed errors over stringly/exception failures.** Model expected failure as a sum type returned via `Either`/the effectful `Error` effect, not `error`, `throwIO`-a-`userError`, or `Maybe` that loses the reason. Reserve exceptions for genuinely exceptional/`IO` faults; recoverable domain failures belong in the type.
- **Watch space leaks in hot paths.** Fields are lazy by default — add `!` to record fields on types that flow through the ingestion/processing pipeline, and force accumulators (`foldl'`, bang patterns). A lazy thunk retained per telemetry row is a memory incident, not a style nit.
- **HTMX tab/nav swap pattern** — all tab/nav links must use: `hxGet_` (full page URL), `hxTarget_` (content container), `hxSelect_` (same container), `term "hx-select-oob"` (`"#nav-container:outerMorph"`), `hxSwap_ "outerMorph"`, `hxPushUrl_ "true"`, and `[__|on click set my.preloadState to 'DONE'|]`. Add `term "preload" "mouseover"` on the parent nav container. Never use separate `/content` partial endpoints or hyperscript for active class management — morphing handles active state automatically. See `BodyWrapper.hs` `navTabAttrs` and settings nav for reference.
- **Tailwind state variants over hyperscript for presentational DOM state.** When a behavior is purely "show/hide/rotate/style X based on some UI state," express it in CSS via Tailwind variants — `peer`/`peer-checked:`, `group`/`group-has-[:checked]:`, `has-[]:`, `aria-expanded:`, and the `<details>`/hidden-checkbox hack — not hyperscript that imperatively `toggle`s classes. CSS-driven state survives HTMX morph swaps (no re-init), needs no JS, and can't desync. Concretely: collapse/expand rows, chevron rotation, dropdown/drawer/menu open-state, and "selected" highlighting should be a checkbox/`details` + variant, not `[__|on click toggle .hidden …|]`. **Reserve hyperscript for genuinely imperative behavior** that CSS can't express: keyboard shortcuts, clipboard, `htmx:*` lifecycle hooks, JS interop (chart/widget calls), auto-dismiss timers, custom event dispatch, and event-propagation control (`halt`). When you add or touch a hyperscript `toggle`/show-hide, check first whether a Tailwind variant does it.
- **Hyperscript over raw inline `<script>`/JS for small scripting logic.** For a handful of imperative lines (JS interop calls, event dispatch, URL/state tweaks tied to one element), prefer a hyperscript attribute at the use site over a `script_ [text|...|]` block of hand-written JS — it keeps the behavior inline (see locality of behavior above), needs no per-render re-serialization into the page, and avoids raw-JS-string pitfalls (untyped, unlinted, easy to typo inside a Lucid `text` block). Reserve embedded `script_` blocks (or the compiled TS bundle) for logic too large or reused for a one-line hyperscript attribute to carry cleanly.
- **Lucid concatenates duplicate `class_` attributes with NO space.** Lucid merges two `class_` attrs on one element via `<>` (`Lucid/Base.hs` `foldlMapWithKey`), so `class_ "checkbox checkbox-sm"` + `class_ "marker"` renders `class="checkbox checkbox-smmarker"` — silently corrupting both the last existing class and your addition. This bites hardest when adding a CSS-state hook (a `peer`/`group-has-[.marker:checked]` marker class) to a component helper that already sets `class_` (`formCheckbox_`, `formField_`, `primaryButton_`, …). Never pass a second `class_`: either fold your classes into the single `class_` the element already has, or — when the helper owns the `class_` and you only need a selector hook — attach the hook as `id_`/`data-*` and target `group-has-[#id:checked]` / `group-has-[[data-x]:checked]` instead. (Composing `<>` onto one `class_` value is fine; two separate `class_` attributes is the trap.)
- **`group-has`/`peer` reach by DOM relationship, not by `next <form/>` proximity.** A hyperscript `remove .hidden from next <form/>` finds its target by DOM search; the CSS equivalents do not. `peer-*` only styles **later siblings** of the `peer`; `group-*`/`group-has-*` only styles **descendants** of the `group` element. When the checkbox is nested inside a `label_` (so it is not a sibling of the element to reveal), put the named group (`group/foo`) on the nearest common ancestor and reveal with `group-has-[…:checked]/foo:flex` — `peer-checked:` will not reach across the label boundary. Verify the ancestor actually encloses both the toggle and the target before converting.

## Metrics & Observability

- **OpenTelemetry tracing** is used for distributed traces (spans, attributes via `System.Tracing`).
- **Custom metrics** should use the OpenTelemetry Metrics API (Counter, Gauge, Histogram) once `hs-opentelemetry-api` gains metrics support (see `ian/metrics-api-sdk` branch). Metric instrumentation points are marked with `TODO(otel-metrics)` throughout the codebase.
- **Never silently drop data** — when an operation fails or is skipped (queue full, cache miss, enqueue failure), always emit a metric or log at `logAttention`.
- **We dogfood our own product — instrument like a power user would want.** Monoscope ingests its own telemetry; every span/metric/log we emit is something we'll later debug an incident with. Hold our instrumentation to the same bar we sell.
- **Right signal for the job.** Metrics for aggregates and alerting (think RED — Rate, Errors, Duration — on every handler/consumer; USE for resources/pools). Traces for causality across a request. Logs for the detail a trace can't carry. Don't log-and-count what a metric should aggregate, and don't emit a metric where a span attribute belongs.
- **Span per logical unit of work, named by operation not implementation.** Wrap each meaningful step (DB query, external call, queue batch) in a span; name it for what it does (`ingest.batch`, `kql.parse`), not the function. Record failures on the span — set error status and attach the exception — so a failed span is visible in the trace, not just in logs.
- **Bounded cardinality on metric dimensions; rich attributes on spans.** Never put unbounded values (user id, trace id, raw URL, error message) on a metric label — that's a cardinality explosion. High-cardinality context belongs on span/log attributes instead. Use OTel semantic-convention attribute names (`http.*`, `db.*`, `messaging.*`) rather than inventing keys.
- **Structured logs, correlated, no secrets.** Log key/value context (via the structured logger in `System.Logging`), never string-interpolated blobs. Ensure trace/span IDs propagate so a log line links back to its trace — including across `ki` threads and effect boundaries, where context must be carried explicitly. Never log secrets, API keys, tokens, or PII.
- **Exemplars: link metrics back to traces.** Once the metrics API lands, attach trace IDs as exemplars on histograms/counters so a latency spike on a chart drills straight into a representative slow trace. This is exactly the cross-linked-context experience the product sells — wire it in our own instrumentation.
- **Log levels carry meaning.** `logAttention` (and above) is for things a human must look at — it should page-worthy, not routine. Don't drown real signal in info/debug noise on hot paths; a per-row debug log in the ingestion pipeline is a cost and a cardinality problem, not free.

## Configuration

All config via env vars (read by `System.Config.EnvConfig` using `envy`). Copy `.env.example` to `.env` for local dev. Key vars: `DATABASE_URL`, `TIMEFUSION_PG_URL`, `PORT` (8080), `GRPC_PORT` (4317), `API_KEY_ENCRYPTION_SECRET_KEY`.

## Operational Scripts

One-off audit, backfill, and reconciliation shell scripts live in `scripts/local/` (gitignored). **Always look there first** before writing a new ad-hoc script — `common.sh` already loads `DATABASE_URL` + LS API helpers, and existing scripts (`active-data-projects.sh`, `paying-customers.sh`, `paid-customers-3mo.sh`, `usage-audit.sh`, `backfill-subscription.sh`, `stripe-paying-customers.sh`) cover most investigation patterns.

**When writing a new script, put it in `scripts/local/`** (not `/tmp`, not elsewhere). Use `common.sh` for env loading and source it via `ENV_FILE=.../.env` if the script needs the prod monoscope DB (`.env`) rather than the LS defaults in `.env.prod`. Prefix write scripts with `write_` and keep read scripts idempotent. Make scripts executable and keep output as TSV / aligned text so they compose with `awk`, `jq`, `column`.

## Proto Generation

```bash
make gen-proto    # regenerate proto-lens bindings from proto/*.proto
# then: hpack && cabal build
```

## Design Context

### Users

Developers, SREs, and on-call engineers investigating production incidents, debugging traces, and understanding system behavior. Primary context is high-stakes and time-pressured (3 AM incident, shared trace link). Secondary context is exploratory (dashboards, endpoint discovery). A smaller audience (PMs, support) opens shared links without technical fluency — don't punish them, but prioritize the power user.

### Brand Personality

Precise, trustworthy, calm. Confident without theatrics, dense where density earns its keep. Never breezy, never decorative, never patronizing. Emotional goal during an incident is _relief_, not excitement.

### Aesthetic Direction

Light-mode-first, **dark-mode parity required**. OKLCH-based semantic tokens (`fillBrand`, `fillError`, etc.). Reference: **Datadog APM** — info-rich, color carries meaning, cross-linked context; but without Datadog's glossier edges. Waterfall/timeline are the heroes.

**Anti-references**: no playful illustrations/mascots, no decorative icons above headings, no neon-on-dark, no glassmorphism, no gradient text, no cartoony empty states.

### Design Principles

1. **Color is signal, not decoration.** Service colors identify; red = error; green = success; brand blue = clickable. No meaning → no color.
2. **Density is respect.** Experienced users scan; pack the waterfall. Vary spacing intentionally — tight within rows, generous between logical groups.
3. **Every error state needs a second signal beyond color** (shape, position, text, icon). **WCAG AA is the floor.**
4. **Progressive disclosure earns its keep during incidents.** Summary counts up front; detail one click away. Never bury the exception type two clicks deep in a 29-error trace.
5. **Dark mode is not an afterthought.** Every token verified in both themes.
6. **Shared views are first-class.** A share link may be opened by someone without an account, on a phone, during a panic. Readable standalone.
