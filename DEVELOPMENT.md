# Development Guide

This guide covers setting up Monoscope for local development and testing.

## Prerequisites

### System Requirements

- **Haskell**: Install via GHCup
- **PostgreSQL with TimescaleDB**: For time-series data storage
- **LLVM**: Required for compilation
- **Google Cloud SDK**: For GCP integration (if using GCP)

### Installing Dependencies

**Install Haskell via GHCup**

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

**For macOS:**

```bash
# Install LLVM
brew install llvm

# Install PostgreSQL with TimescaleDB
brew install postgresql
brew install timescaledb

# Install libpq
brew install libpq
```

**For Linux (Ubuntu/Debian):**

```bash
# Install LLVM
sudo apt-get install llvm

# Install PostgreSQL and TimescaleDB
# Follow instructions at: https://docs.timescale.com/install/latest/

# Install libpq
sudo apt-get install libpq-dev
```

## Development Setup

### Database Setup with Docker

1. **Create a Docker volume for PostgreSQL data:**

```bash
docker volume create pgdata
```

2. **Run TimescaleDB in Docker:**

```bash
make timescaledb-docker
```

3. **Configure pg_cron extension:**

Add the following to your PostgreSQL configuration:

```sql
ALTER system SET cron.database_name = 'monoscope';
ALTER system SET shared_preload_libraries = 'pg_cron';
```

Then restart the TimescaleDB Docker container.

### Building from Source

```bash
# Clone the repository
git clone https://github.com/monoscope-tech/monoscope.git
cd monoscope

# Build with Stack
stack build

# Or build with Cabal
cabal build
```

### Running Locally

```bash
# Using Stack
stack run

# Or using Cabal
cabal run monoscope-server
```

### Development Tools

**Install code formatting and linting tools:**

```bash
# Code formatter
brew install ormolu

# Linter
brew install hlint
```

**Useful commands:**

```bash
# Format code
make fmt

# Run linter
make lint
```

### Automatic Recompilation with ghcid

The project uses ghcid for automatic recompilation:

```bash
# Install ghcid
stack install ghcid

# Run with automatic recompilation
make dev
# Or
ghcid --command="stack repl" --test=":main"
```

Build status is written to `build.log`. Check it with:

```bash
# Check last 50 lines of build log
tail -50 build.log

# Watch build log in real-time
tail -f build.log
```

### IDE Support

For better IDE support, compile Haskell Language Server locally to avoid crashes, especially on macOS. See [issue #2391](https://github.com/haskell/haskell-language-server/issues/2391).

## Testing

### Run All Tests

```bash
make test
# OR
stack test --ghc-options=-w
```

### Unit Tests

Unit tests don't require a database connection and run much faster. They include doctests and pure function tests.

```bash
make test-unit
# OR
stack test monoscope-server:unit-tests --ghc-options=-w
```

### Unit Tests with File Watching

```bash
make live-test-unit
# OR
stack test monoscope-server:unit-tests --ghc-options=-w --file-watch
```

### Integration Tests

Integration tests use an external PostgreSQL database for better isolation and performance.

```bash
# Run all integration tests
USE_EXTERNAL_DB=true cabal test integration-tests -j --ghc-options="-O0 -j8" --test-show-details=direct

# Run specific test suite
USE_EXTERNAL_DB=true cabal test integration-tests -j --ghc-options="-O0 -j8" --test-show-details=direct --test-option="--match=/gRPC Ingestion/"

# Monitor test progress
USE_EXTERNAL_DB=true cabal test integration-tests -j --ghc-options="-O0 -j8" --test-show-details=direct 2>&1 | tee tests.log
tail -f tests.log
```

### Running Specific Tests

```bash
stack test --test-arguments "--match=SeedingConfig" monoscope-server:tests
# OR
stack test --ta "--match=SeedingConfig" monoscope-server:tests
```

### Test Workflow When Iterating

When fixing failing tests:
1. Run tests in background with output to `tests.log`
2. Analyze compilation/test errors from the output
3. Fix issues (type errors, missing functions, etc.)
4. Re-run tests to verify fixes
5. Iterate until all tests pass

### Common Test Helpers

- `toServantResponse` - For handlers that return `RespHeaders a` (wrapped servant responses)
- `runQueryEffect` - For effects that return data directly (like `Charts.queryMetrics`)
- `runTestBg` - Run background jobs in test context
- `withTestResources` - Provides test resources (pool, caches, auth context, etc.)

## Service Worker

To build the service worker:

```bash
workbox generateSW workbox-config.js
```

## Google Cloud Configuration (Optional)

If using Google Cloud integration:

```bash
gcloud auth application-default login
```

## Project Structure

```
monoscope/
├── src/              # Haskell source code
├── static/           # Static assets and migrations
├── web-components/   # Frontend TypeScript components
├── tests/           # Test suites
├── build.log        # Auto-generated build status
└── build-doctests.log # Doctest results
```

## Database Access

For read-only database access during development:

```bash
# Read DATABASE_URL from .env file
source .env
psql $DATABASE_URL
```

**Important**: You're absolutely not allowed to perform any writes directly to the database. Use the application's API or migration system for any changes.

## Contributing

Before contributing:
1. Ensure all tests pass
2. Run code formatter: `make fmt`
3. Run linter: `make lint`
4. Update documentation if needed

See our [Contributing Guidelines](CONTRIBUTING.md) for more details.