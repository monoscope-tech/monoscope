# Contributing to Monoscope

Thank you for your interest in contributing! This guide covers development setup, running the project locally, testing, code style, and the PR workflow.

## Development Setup

### Prerequisites

- **GHCup** — Haskell toolchain manager: `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`
- **Docker & Docker Compose** — For database and services
- **Node.js 22** — For frontend assets (web-components, Tailwind CSS)
- **LLVM** — Required for GHC compilation
- **libpq** — PostgreSQL development libraries

### Initial Setup

```bash
# Clone the repository (fork first if contributing)
git clone https://github.com/monoscope-tech/monoscope.git
cd monoscope

# Install Haskell dependencies
cabal update

# Activate the pre-commit hook for auto-formatting
git config core.hooksPath .githooks
```

### Running Locally with Docker Compose

The fastest way to get a running instance:

```bash
docker-compose up
```

Visit `http://localhost:8080` — default credentials are `admin` / `changeme`.

#### Optional Profiles

Start additional services with Docker Compose profiles:

```bash
# pgAdmin for database management (http://localhost:5050)
docker-compose --profile dev up

# Ephemeral test database (port 5433)
docker-compose --profile test up
```

#### Environment Variables

Copy `.env.example` to `.env` and adjust as needed. The defaults are sufficient for local development.

### Building from Source

```bash
# Build all targets
cabal build all -j

# Run the application
cabal run

# Or with live-reload using ghcid
make live-reload
```

## Running Tests

### Unit Tests (fast, no database required)

```bash
cabal test unit-tests --ghc-options="-O0" --test-show-details=direct
```

Or via Makefile:

```bash
make test-unit
```

### Doctests

```bash
cabal test doctests --ghc-options="-O0" --test-show-details=direct
```

Or via Makefile:

```bash
make test-doctests
```

### Integration Tests (requires external PostgreSQL)

```bash
USE_EXTERNAL_DB=true cabal test integration-tests --ghc-options="-O0" --test-show-details=direct
```

Or via Makefile:

```bash
make test-integration
```

### All Tests

```bash
make test
```

### Run Specific Tests

```bash
# Match by test name pattern
cabal test unit-tests --test-options='--match="/pattern/"'

# Run with live file-watching
make live-test-reload-unit
```

## Running CI Locally

Our GitHub runners are small. Your laptop probably isn't. `make ci` runs **the
same checks CI runs, in the same container images**, so you can find a failure in
minutes instead of waiting on a queue:

```bash
make ci                                # everything CI runs
make ci CHECKS="doctests unit-tests"   # just these
make ci-status                         # what CI would run right now, without running it
make ci-down                           # stop the containers (build caches kept)
```

`ci/checks.tsv` is the single definition of what CI is — the GitHub workflows and
`make ci` both read it, so the two can't drift apart.

### CI skips what you already proved

Each check is fingerprinted over the content it depends on. When one passes,
the result is published as a git ref, and CI's gate skips any check already
proven for the exact tree it's about to test. Run `make ci` before you push and
CI has little or nothing left to do.

Two things worth knowing:

- **The first run is a cold build** and takes as long as a cold CI run. Every run
  after that is incremental — the caches live in Docker volumes that survive
  `make ci-down`. Start it and go do something else.
- **Publishing a result requires push access**, so if you're contributing from a
  fork, `make ci` still gives you fast local feedback but CI will re-run the
  checks itself. That's deliberate: an attestation is only as trustworthy as the
  ability to push code in the first place.

Reuse is conservative by design. Each check declares the capabilities it needs
(`ghc`, `pg`, `minio`, `tf-real`) and each result records what the environment
actually had, so a run that fell back to a stub service can never stand in for
one that needs the real thing. On Apple Silicon, where TimeFusion has no image,
`integration-tests` is refused rather than weakly cached — run it with
`CI_ALLOW_DEGRADED=true` for feedback and CI will still verify it properly.

Full reference, including every knob and how to add or change a check:
[docs/local-ci.md](docs/local-ci.md).

## Code Style

### Formatting

**Fourmolu** is used for Haskell code formatting. It is automatically run via a GitHub Actions workflow on every push — any formatting changes are auto-committed to your branch. You don't need to format manually before opening a PR.

To format locally:

```bash
make fmt
```

To format on commit (pre-commit hook is in `.githooks/pre-commit`):

```bash
# Already activated via git config core.hooksPath .githooks
# To bypass for a single commit:
SKIP_FOURMOLU=1 git commit ...
```

### Linting

**HLint** checks for common Haskell code smells and style issues.

```bash
make lint
```

To auto-fix lint issues:

```bash
make fix-lint
```

### Frontend

JavaScript/TypeScript uses **Prettier** (configured in `.prettierrc`). Run it via:

```bash
npx prettier --write .
```

Tailwind CSS is processed during the build — run it manually with:

```bash
make post-css
```

## PR Workflow

### Fork & Branch

1. Fork the repository on GitHub.
2. Add your fork as a remote:

   ```bash
   git remote add fork https://github.com/<your-username>/monoscope.git
   ```

3. Create a feature branch from `master`:

   ```bash
   git checkout master
   git pull upstream master
   git checkout -b feat/your-feature-name
   ```

### Making Changes

1. Make your changes, writing tests as needed.
2. Ensure tests pass:

   ```bash
   cabal test unit-tests --ghc-options="-O0"
   cabal test doctests --ghc-options="-O0"
   ```

3. Run the linter:

   ```bash
   make lint
   ```

4. Commit your changes. The pre-commit hook will run fourmolu on staged `.hs` files. To bypass the hook for a specific commit (e.g., for documentation-only changes):

   ```bash
   SKIP_FOURMOLU=1 git commit -m "your message"
   ```

### Opening a Pull Request

1. Push your branch to your fork:

   ```bash
   git push fork feat/your-feature-name
   ```

2. Open a PR against `monoscope-tech/monoscope` on GitHub.
3. Fill in the PR template with a clear description of the changes.
4. A maintainer will review and request changes if needed.

### Auto-Formatting on Push

The `fourmolu.yml` GitHub Actions workflow runs on every push to the repository. It automatically formats any `.hs` files that don't match the fourmolu configuration and commits the changes back to your branch. This means:

- **Don't worry about formatting** — fourmolu will fix it for you after you push.
- **Check the diff** after pushing — the bot may have committed formatting fixes.
- **Amend or rebase** as needed if the bot's commit conflicts with your changes.

## Project Structure

```
monoscope/
├── src/              # Haskell application source
├── static/           # Static assets, migrations, public files
├── web-components/   # TypeScript frontend components
├── cli/              # CLI tool source
├── proto/            # Protocol Buffer definitions
├── test/             # Test scripts and helpers
├── tests/            # Integration test suites
├── app/              # Application entry points
├── npm/              # Node.js helper scripts
├── config/           # Application configuration
├── docs/             # Documentation
└── cabal.project     # Cabal build configuration
```

## Getting Help

- **Discord** — Join the community at https://discord.gg/BSFCaUHxt4
- **GitHub Issues** — For bugs and feature requests
- **Documentation** — https://docs.monoscope.tech