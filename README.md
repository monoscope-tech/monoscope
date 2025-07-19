<div align="center">

# Monoscope

## Advanced Monitoring & Observability Platform


[![Haskell](https://img.shields.io/badge/Built%20with-Haskell-5e5086?logo=haskell)](https://www.haskell.org/) [![S3 Compatible](https://img.shields.io/badge/Storage-S3%20Compatible-EC912E?logo=amazons3)](https://aws.amazon.com/s3/) [![LLM Powered](https://img.shields.io/badge/AI-LLM%20Anomaly%20Detection-412991)](https://github.com/apitoolkit/apitoolkit-server)

<img width="2732" height="2048" alt="app apitoolkit io_p_00000000-0000-0000-0000-000000000000_log_explorer(ipad but pc) (1)" src="https://github.com/user-attachments/assets/6175c23b-f3ac-450a-9ae3-4b86371f4f54" />


[Monoscope](https://github.com/apitoolkit/apitoolkit-server) is a powerful monitoring and observability platform that leverages LLMs to intelligently identify anomalies in your systems. With efficient data storage to S3-compatible buckets and real-time analytics powered by our custom timefusion storage engine, Monoscope provides deep insights into your infrastructure and applications.

</div>


<img width="2732" height="2048" alt="app apitoolkit io_p_00000000-0000-0000-0000-000000000000_log_explorer(ipad but pc)" src="https://github.com/user-attachments/assets/baf78e6f-2b75-4b6f-be56-94a0b3da9f31" />
<img width="3034" height="2274" alt="app apitoolkit io_p_00000000-0000-0000-0000-000000000000_dashboards_b6b33e15-100b-4c3d-8764-a7b60d5cfd87_(ipad but pc)" src="https://github.com/user-attachments/assets/3dae136a-e627-4278-91a1-81c5af5f8cd6" />

---

## Table of Contents

- [Features](#features)
- [Getting Started](#getting-started)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Development Setup](#development-setup)
- [Testing](#testing)
- [Useful Links](#useful-links)
- [Contributing](#contributing)
- [License](#license)

---

## Features

- 🤖 **AI-Powered Anomaly Detection**: Leverages LLMs to automatically identify and alert on unusual patterns
- ☁️ **S3-Compatible Storage**: Store logs, metrics and traces in any S3-compatible object storage
- 🚀 **High Performance**: Written in Haskell and rust for reliability and performance
- 📈 **Real-Time Analytics**: Monitor your systems with minimal latency
- 🔌 **Extensible**: Easy to integrate with existing monitoring infrastructure

## Getting Started

### Prerequisites

Before installing Monoscope, ensure you have the following dependencies:

- **Haskell**: Install via GHCup
- **PostgreSQL with TimescaleDB**: For time-series data storage
- **LLVM**: Required for compilation
- **Google Cloud SDK**: For GCP integration (if using GCP)

### Installation

1. **Install Haskell via GHCup**

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

2. **Clone the Repository**

```bash
git clone https://github.com/apitoolkit/apitoolkit-server.git monoscope
cd monoscope
```

3. **Install System Dependencies**

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

4. **Configure Google Cloud (Optional)**

If using Google Cloud integration:

```bash
gcloud auth application-default login
```

5. **Run Monoscope**

```bash
stack run
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
ALTER system SET cron.database_name = 'apitoolkit';
ALTER system SET shared_preload_libraries = 'pg_cron';
```

Then restart the TimescaleDB Docker container.

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

💡 **Tip**: For better IDE support, compile Haskell Language Server locally to avoid crashes, especially on macOS. See [issue #2391](https://github.com/haskell/haskell-language-server/issues/2391).

### Service Worker

To build the service worker:

```bash
workbox generateSW workbox-config.js
```

## Testing

### Run all tests

```haskell
make test
# OR
stack test --ghc-options=-w
```

### Run only unit tests

Unit tests don't require a database connection and run much faster. They include doctests and pure function tests.

```haskell
make test-unit
# OR
stack test apitoolkit-server:unit-tests --ghc-options=-w
```

### Run unit tests with file watching for development

```haskell
make live-test-unit
# OR
stack test apitoolkit-server:unit-tests --ghc-options=-w --file-watch
```

### Run a specific individual test

```haskell
stack test --test-arguments "--match=SeedingConfig" apitoolkit-server:tests
# OR
stack test --ta "--match=SeedingConfig" apitoolkit-server:tests
```

## Useful Links

- **Manual Data Ingestion**: Available at `/p/<project-id>/manual_ingest` (development only)
- **Inspiration**: [Lightstep](https://lightstep.com/), [Datadog](https://www.datadoghq.com/), [Instana](https://www.instana.com/)
- **Useful Reading**: [Build reload feedback cycle in Haskell](https://functor.tokyo/blog/2019-04-07-ghcid-for-web-app-dev)

## Contributing

We welcome contributions to Monoscope! Please feel free to:

- Report bugs and request features via [GitHub Issues](https://github.com/apitoolkit/apitoolkit-server/issues)
- Submit pull requests for bug fixes and new features
- Improve documentation and examples
- Share your use cases and feedback

Before contributing, please read our contributing guidelines and ensure your code passes all tests and linting checks.

## License

Monoscope is open source software. Please see the LICENSE file for details.

---

<div align="center">

<a href="https://github.com/apitoolkit/apitoolkit-server" target="_blank" rel="noopener noreferrer">⭐ Star us on GitHub</a>

</div>
