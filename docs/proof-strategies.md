# Fast proofs, by stack

A proof records the merge base and the pull request in two offline VMs, at the same time, with the
same script. Every second of setup is paid twice and on every review, so the target is a proof
that starts in seconds. The review budget is 30 minutes, but a proof that needs minutes leaves
less of it for the investigation, and costs CPU on a shared host.

Three things make a proof fast, in order of leverage:

1. **Build nothing that production already built.** Ship the last production artifact in the
   guest image and rebuild only the layer the pull request touches.
2. **Start from a warm workspace.** Let the base's build output survive in a checkpoint, so the
   head compiles only the diff.
3. **Pre-pay state.** Migrate databases, install dependencies and download browsers when the guest
   image is built, never during a proof.

The repository tells the agent how through `.monoscope/PROOF.md`, read from the base revision.
The host stores the setup of every proof that succeeded and offers it to the next review, so a
good recipe is written once.

## The levers

| Lever | Where it lives | What it saves |
| :-- | :-- | :-- |
| Guest image per stack | `impact-review-worker/Dockerfile.*`, `build-guest.mjs` | Toolchains, dependencies, browsers, migrated databases |
| Production artifact in the guest | `COPY --from=<prod image>` in the guest Dockerfile | The whole server build, for frontend and script changes |
| Warm workspace checkpoint | `IMPACT_REVIEW_PREPARE_SCRIPT`, `IMPACT_REVIEW_CACHE_DIR` | The base build; the head becomes incremental |
| Dependency archive | `IMPACT_REVIEW_DEPENDENCY_ARCHIVE` | Offline package fetches (cargo, go, pip, npm) |
| Stored recipe | `projects.pr_review_proof_recipes` | The agent working out setup again |
| Narrow target | `PROOF.md` | Building and starting more than the claim needs |

A guest image is pinned by build id: a checkpoint built on one image is never reused on another,
so rebuild images on a schedule, not per pull request.

## Per stack

### TypeScript and JavaScript

Bake `node_modules` at the base lockfiles into the guest (`npm ci` in the image) and link them into
the checkout; a pull request that changes the lockfile is the only one that needs more. Vite,
esbuild and Tailwind build a frontend in seconds, so build the head every time and serve it.
Prefer a production build plus a static server or the real backend over a dev server: no
dependency pre-bundling on first load, and the recording shows what users get.

### Haskell

Never build the server in a proof. Copy the production binary into the guest, migrate its
database at image build time, and swap in the checkout's frontend before starting it. Monoscope
does this with `monoscope-app`: rebuilding the frontend, starting Postgres and the server, and
ingesting an hour of sample traffic take 7 seconds together, against 25 seconds of migrations
paid once per image. When the
change is in Haskell, prove it with a focused test or doctest from a warm `dist-newstyle`
checkpoint, at `-O0`, building one component, or through `cabal repl` with `:script`, which skips
linking.

### Rust

Keep `target/` in the warm checkpoint and build only the binary the claim needs
(`cargo build --bin <name>`), with `CARGO_INCREMENTAL=1`, a debug profile and `mold` as the linker.
Vendor dependencies into the dependency archive (`cargo vendor`), since the VM is offline. A crate
that is cheap to run in tests is usually better proved with `cargo test <name> -- --nocapture` than
by starting a service.

### Go

Go is fast enough to build cold, but keep `GOCACHE` and `GOMODCACHE` in the checkpoint anyway, and
`go build ./cmd/<name>` only what the claim needs. Offline, either build vendored modules with
`-mod=vendor`, or ship `GOMODCACHE` in the dependency archive and set `GOPROXY=off`.

### Python

Install the locked environment into the guest (`uv sync --frozen` into a fixed venv path) and
point the checkout at it. Python needs no build, so a proof is the start time of the app: use the
production server command with migrations already applied in the image, and SQLite or a
pre-initialised Postgres data directory instead of running migrations per proof.

## Databases

Run migrations once, at image build time, and ship the data directory. Postgres starts from it in
under a second with `fsync=off`, `synchronous_commit=off` and `full_page_writes=off`, which are
safe in a VM that is thrown away. Load sample data through the product's own ingestion or API when
it has derived state (Monoscope sends OTLP to itself so patterns and Live Tail are real); use SQL
only for plain tables. Seed relative to `now()`, so time-windowed views have data.

## Writing PROOF.md

Keep it to the recipe the agent can copy:

- one setup command, unrecorded, that leaves the app ready and returns;
- the URL or command that shows the change, and how to assert it so the base fails;
- what the setup cannot prove (in Monoscope, server-rendered markup), so the agent reports it as
  unverified instead of failing a proof.

See [Monoscope's own](../.monoscope/PROOF.md) for a browser proof against the real app.
