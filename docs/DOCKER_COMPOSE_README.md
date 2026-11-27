# Docker Compose Setup for Monoscope

This Docker Compose configuration runs a complete Monoscope stack: the app, [TimeFusion](https://github.com/monoscope-tech/timefusion) for telemetry (logs, traces, metrics) on S3-compatible storage, and TimescaleDB for application state (projects, users, monitors, dashboards).

## Quick Start

### 1. Basic Usage

Start the stack:

```bash
# Pull the latest Monoscope image from GitHub Packages (if needed)
docker pull ghcr.io/monoscope-tech/monoscope:latest

# Start services
docker-compose up -d
```

This will:

- Start TimescaleDB on port 5432 (application state)
- Start a local S3-compatible store ([RustFS](https://github.com/rustfs/rustfs)) and create TimeFusion's bucket
- Start TimeFusion (telemetry store) on port 12345
- Pull and start Monoscope from GitHub Packages on port 8080 (HTTP) and 4317 (gRPC)
- Automatically run database migrations
- Set up with basic authentication (admin/changeme)

### 2. Access the Application

- **Monoscope Web UI**: <http://localhost:8080>
- **gRPC/OTLP Endpoint**: localhost:4317
- **PostgreSQL/TimescaleDB**: localhost:5432
  - Database: `monoscope`
  - User: `postgres`
  - Password: `postgres`
- **TimeFusion** (PostgreSQL wire protocol, query telemetry with any Postgres client): localhost:12345
  - `psql postgresql://postgres:postgres@localhost:12345/postgres -c 'select count(*) from otel_logs_and_spans'`

### 3. Testing with Telemetrygen

Send test telemetry to verify your setup:

```bash
monoscope telemetrygen --kind=trace --count=10
```

### 4. Configuration

#### Using Environment Variables

1. Copy `.env.example` to `.env`:

   ```bash
   cp .env.example .env
   ```

2. Edit `.env` with your specific configuration values

3. Docker Compose will automatically use the `.env` file

#### Using your own S3 bucket

TimeFusion stores telemetry as Delta Lake tables in the bucket named by `AWS_S3_BUCKET`. To use AWS S3, Cloudflare R2 or any other S3-compatible store, set the `timefusion` service's `AWS_*` variables to your bucket and credentials, and remove the `s3` and `s3-bucket` services. Keep the `timefusion_data` volume: it holds the write-ahead log for rows not yet flushed to the bucket.

#### Direct Database Connection (for local development)

If you want to run Monoscope locally but use the Docker databases:

```bash
# Start TimescaleDB and TimeFusion
docker-compose up -d timescaledb timefusion

# Point a local Monoscope at them
export DATABASE_URL="host=localhost user=postgres password=postgres dbname=monoscope port=5432 sslmode=disable"
export TIMEFUSION_PG_URL="postgresql://postgres:postgres@localhost:12345/postgres"

# Run Monoscope locally
cabal run
```

## Available Services

### Core Services (always start)

- **timescaledb**: PostgreSQL with the TimescaleDB extension, for application state
- **s3** / **s3-bucket**: local S3-compatible storage for TimeFusion, and a one-shot job that creates its bucket
- **timefusion**: the telemetry store every log, span and metric is written to and read from
- **monoscope**: The Monoscope application

### Optional Services

#### pgAdmin (Database Management UI)

Start with the dev profile:

```bash
docker-compose --profile dev up -d
```

Access at: <http://localhost:5050>

- Email: <admin@monoscope.tech>
- Password: admin

#### Test Database (Ephemeral)

Start with the test profile for integration testing:

```bash
docker-compose --profile test up -d
```

This starts a separate TimescaleDB instance on port 5433 with temporary storage.

## Building Locally for Development

By default, Docker Compose uses the pre-built Monoscope image from GitHub Packages. To build locally:

### Option 1: Using Override File (Recommended)

```bash
# Copy the override example
cp docker-compose.override.yml.example docker-compose.override.yml

# Docker Compose will automatically use both files
docker-compose up -d
```

### Option 2: Using docker-compose build

```bash
# Force local build
docker-compose build monoscope

# Or build and run
docker-compose up -d --build monoscope
```

### Option 3: Manual Override

```bash
# Build locally with a custom tag
docker build -t monoscope:local .

# Run with the local image
docker run -d \
  --name monoscope-local \
  --network monoscope-network \
  -p 8080:8080 -p 4317:4317 \
  -e DATABASE_URL="host=timescaledb user=postgres password=postgres dbname=monoscope port=5432 sslmode=disable" \
  -e TIMEFUSION_PG_URL="postgresql://postgres:postgres@timefusion:5432/postgres" \
  monoscope:local
```

## Common Commands

### View logs

```bash
# All services
docker-compose logs -f

# Specific service
docker-compose logs -f monoscope
docker-compose logs -f timefusion
docker-compose logs -f timescaledb
```

### Update to latest Monoscope image

```bash
docker pull ghcr.io/monoscope-tech/monoscope:latest
docker-compose up -d monoscope
```

### Stop all services

```bash
docker-compose down
```

### Stop and remove all data

```bash
docker-compose down -v
```

### Run integration tests with test database

```bash
# Start test database
docker-compose --profile test up -d timescaledb-test

# Run tests (adjust DATABASE_URL to use port 5433)
USE_EXTERNAL_DB=true DATABASE_URL="host=localhost user=postgres password=postgres dbname=apitoolkit_test_template port=5433 sslmode=disable" cabal test integration-tests
```

## Troubleshooting

### Port Conflicts

If you have PostgreSQL running locally on port 5432, you can change the port mapping in `docker-compose.yml`:

```yaml
ports:
  - '5433:5432' # Change host port to 5433
```

Then update your DATABASE_URL to use port 5433.

### Build Issues

If the Monoscope build fails, check:

1. Docker has enough memory allocated (at least 4GB)
2. Build cache might be stale: `docker-compose build --no-cache monoscope`

### Database Connection Issues

If Monoscope can't connect to TimescaleDB:

1. Ensure TimescaleDB is healthy: `docker-compose ps`
2. Check logs: `docker-compose logs timescaledb`
3. Verify network connectivity: `docker exec monoscope-app ping timescaledb`

### No telemetry showing up

1. Ensure TimeFusion is healthy: `docker-compose ps timefusion`
2. Check its logs for S3 errors: `docker-compose logs timefusion`
3. Check that rows arrive: `psql postgresql://postgres:postgres@localhost:12345/postgres -c 'select count(*) from otel_logs_and_spans'`

### Migration Issues

If migrations fail:

1. Check migration files exist: `ls ./static/migrations/`
2. Verify database is ready: `docker-compose exec timescaledb pg_isready`
3. Check Monoscope logs: `docker-compose logs monoscope`

## Performance Tuning

### For Development

The current configuration is optimized for development with:

- Full query logging enabled
- Max connections set to 200
- Shared memory optimized for TimescaleDB

### For Production

Consider adjusting in `docker-compose.yml`:

- Point TimeFusion at a real S3 bucket (see "Using your own S3 bucket") and change the `RUSTFS_*`/`AWS_*` and `PGWIRE_*` credentials
- Remove query logging: Remove `-c log_statement='all'`
- Increase shared_buffers
- Adjust max_connections based on load
- Use Docker secrets for sensitive environment variables

