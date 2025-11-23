# Getting Started Guide

This guide will help you get Monoscope running and receive your first telemetry data in 5 minutes!

## Quick Start (2 minutes)

### Step 1: Start Monoscope with Docker Compose

Docker Compose is the recommended way to install Monoscope as it includes all required dependencies.

```bash
# Clone and start Monoscope
git clone https://github.com/monoscope-tech/monoscope.git
cd monoscope
docker-compose up -d

# Verify services are running
docker-compose ps
```

### Step 2: Access the UI

1. Open your browser and go to `http://localhost:8080`
2. Log in with default credentials:
   - Username: `admin`
   - Password: `changeme`

### Step 3: Create Your First Project

1. Click "New Project" in the dashboard
2. Enter project details:
   - Name: "My First Project"
   - Description: "Testing Monoscope"
3. Copy the API key that's generated - you'll need it next

## Send Your First Telemetry Data

### Option A: Using telemetrygen (Recommended for testing)

```bash
# Install telemetrygen if you haven't
go install github.com/open-telemetry/opentelemetry-collector-contrib/cmd/telemetrygen@latest

# Send test traces
telemetrygen traces \
  --otlp-endpoint localhost:4317 \
  --otlp-insecure \
  --otlp-header 'X-API-Key="YOUR_API_KEY"' \
  --traces 100 \
  --duration 10s

# Send test metrics
telemetrygen metrics \
  --otlp-endpoint localhost:4317 \
  --otlp-insecure \
  --otlp-header 'X-API-Key="YOUR_API_KEY"' \
  --metrics 5 \
  --duration 10s

# Send test logs
telemetrygen logs \
  --otlp-endpoint localhost:4317 \
  --otlp-insecure \
  --otlp-header 'X-API-Key="YOUR_API_KEY"' \
  --logs 100
```

### Option B: Quick Python Script

```python
# save as send_telemetry.py
from opentelemetry import trace
from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import OTLPSpanExporter
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor
import time

# Configure OpenTelemetry
trace.set_tracer_provider(TracerProvider())
tracer = trace.get_tracer(__name__)

# Set up OTLP exporter
otlp_exporter = OTLPSpanExporter(
    endpoint="localhost:4317",
    insecure=True,
    headers=(("x-api-key", "YOUR_API_KEY"),)
)

# Add exporter to tracer
trace.get_tracer_provider().add_span_processor(
    BatchSpanProcessor(otlp_exporter)
)

# Send a test span
with tracer.start_as_current_span("test-operation") as span:
    span.set_attribute("user.id", "123")
    span.set_attribute("operation.type", "test")
    time.sleep(0.1)
    print("Telemetry sent!")
```

Run it:
```bash
pip install opentelemetry-api opentelemetry-sdk opentelemetry-exporter-otlp
python send_telemetry.py
```

### Option C: Using curl (Simplest)

```bash
# Send a test log entry
curl -X POST http://localhost:8080/api/v1/logs \
  -H "X-API-Key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "resource": {
      "attributes": {
        "service.name": "test-service",
        "service.version": "1.0.0"
      }
    },
    "logs": [{
      "timestamp": "'$(date -u +%Y-%m-%dT%H:%M:%S.%3NZ)'",
      "severity": "INFO",
      "body": "Test log message from getting started",
      "attributes": {
        "user.id": "user123",
        "request.id": "req456"
      }
    }]
  }'
```

## View Your Data

1. Go back to the Monoscope UI
2. Navigate to "Log Explorer" in the left sidebar
3. You should see your test data appearing
4. Try the natural language search: "Show all logs from test-service"

## Installation Options

### Docker Compose (Recommended)

The default `docker-compose.yml` includes everything you need:
- Monoscope server
- TimescaleDB with TimescaleDB extension
- All necessary configuration

#### Prerequisites
- Docker and Docker Compose installed
- At least 4GB of available RAM
- Ports 8080 (web UI) and 4317 (gRPC) available

#### Configuration

Key environment variables in `docker-compose.yml`:

| Variable | Description | Default |
|----------|-------------|---------|
| `DATABASE_URL` | PostgreSQL connection string | Configured automatically |
| `PORT` | HTTP port for web UI | 8080 |
| `GRPC_PORT` | gRPC port for OpenTelemetry | 4317 |
| `BASIC_AUTH_USERNAME` | Username for basic auth | admin |
| `BASIC_AUTH_PASSWORD` | Password for basic auth | changeme |
| `SHOW_DEMO_PROJECT` | Show demo project | False |

### Using External Database

If you prefer to use an existing PostgreSQL/TimescaleDB instance:

1. Update `DATABASE_URL` in docker-compose.yml:
   ```yaml
   environment:
     DATABASE_URL: postgresql://user:pass@your-db-host:5432/monoscope?sslmode=require
   ```

2. Ensure your database has TimescaleDB extension enabled

3. Remove the `timescaledb` service from docker-compose.yml

### Production Deployment

For production environments:

1. **Use .env file for secrets**
   ```bash
   # Create .env file
   cp .env.example .env
   # Edit .env with your production values
   ```

2. **Enable Auth0 for SSO**
   ```yaml
   BASIC_AUTH_ENABLED: False
   AUTH0_DOMAIN: your-domain.auth0.com
   AUTH0_CLIENT_ID: your-client-id
   AUTH0_SECRET: your-secret
   AUTH0_CALLBACK: https://your-domain/auth_callback
   ```

3. **Set resource limits**
   ```yaml
   services:
     monoscope:
       deploy:
         resources:
           limits:
             memory: 4G
           reservations:
             memory: 2G
   ```

4. **Persist data volumes**
   ```yaml
   volumes:
     timescale_data:
       driver: local
       driver_opts:
         type: none
         o: bind
         device: /path/to/persistent/storage
   ```

## Integrate Your Applications

### Node.js Applications

```javascript
const { NodeSDK } = require('@opentelemetry/sdk-node');
const { OTLPTraceExporter } = require('@opentelemetry/exporter-trace-otlp-grpc');

const sdk = new NodeSDK({
  traceExporter: new OTLPTraceExporter({
    url: 'http://localhost:4317',
    headers: {
      'x-api-key': 'YOUR_API_KEY'
    }
  })
});

sdk.start();
```

### Python Applications

```python
from opentelemetry import trace
from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import OTLPSpanExporter
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor

trace.set_tracer_provider(TracerProvider())
trace.get_tracer_provider().add_span_processor(
    BatchSpanProcessor(
        OTLPSpanExporter(
            endpoint="localhost:4317",
            headers=(("x-api-key", "YOUR_API_KEY"),)
        )
    )
)
```

### Java Applications

Add to your JVM options:
```bash
-javaagent:/path/to/opentelemetry-javaagent.jar
-Dotel.exporter.otlp.endpoint=http://localhost:4317
-Dotel.exporter.otlp.headers=x-api-key=YOUR_API_KEY
-Dotel.service.name=my-java-app
```

### Kubernetes

See our [Kubernetes Guide](kubernetes.md) for deploying Monoscope on K8s and monitoring your cluster.

## Explore Features

### Natural Language Search

Try these example queries in the Log Explorer:
- "Show me all errors in the last hour"
- "What caused the spike in response time?"
- "Find all database queries taking longer than 1 second"
- "Show logs from the payment service"

### AI Anomaly Detection

The AI anomaly detection will automatically:
- Learn your system's normal behavior patterns (takes 24-48 hours)
- Detect unusual patterns in logs, metrics, and traces
- Group related anomalies to reduce alert noise
- Send notifications through configured channels

### Dashboards

Create custom dashboards by:
1. Navigate to "Dashboards" in the left sidebar
2. Click "New Dashboard"
3. Add widgets for metrics, logs, and traces
4. Customize time ranges and refresh intervals

## Updating Monoscope

To update to the latest version:

```bash
# Pull latest images
docker-compose pull

# Restart services
docker-compose up -d

# Check logs for any migration messages
docker-compose logs monoscope
```

## Troubleshooting

### Not seeing data?

1. Check Monoscope logs:
   ```bash
   docker-compose logs -f monoscope
   ```

2. Verify the endpoint is accessible:
   ```bash
   telnet localhost 4317
   ```

3. Check your API key is correct:
   ```bash
   curl http://localhost:8080/api/v1/health \
     -H "X-API-Key: YOUR_API_KEY"
   ```

### Port already in use?

Change ports in docker-compose.yml:
```yaml
ports:
  - "8081:8080"  # Use 8081 instead
  - "4318:4317"  # Use 4318 instead
```

### Database connection failed?

```bash
# Check database is running
docker-compose ps timescaledb

# Check database logs
docker-compose logs timescaledb
```

### Out of memory?

Increase Docker memory allocation:
- Docker Desktop: Settings → Resources → Memory

## Next Steps

- Read the [Configuration Reference](configuration.md) for all options
- Set up [Alert Configuration](alerts.md) for anomaly detection
- Deploy on [Kubernetes](kubernetes.md) for production
- Review the [Architecture](architecture.md) for technical details
- Join our [Discord](https://discord.gg/monoscope) community

## Need Help?

- 💬 [Discord](https://discord.gg/monoscope) - Chat with users and contributors
- 🐛 [Issues](https://github.com/monoscope-tech/monoscope/issues) - Report bugs
- 📝 [Blog](https://blog.monoscope.tech) - Tutorials and case studies