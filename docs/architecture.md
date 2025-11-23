# Architecture Overview

## System Architecture

Monoscope follows a microservices-inspired monolithic architecture, optimized for performance and ease of deployment.

```
┌─────────────────────────────────────────────────────────────┐
│                         Ingestion Layer                       │
├────────────────┬────────────────┬────────────────────────────┤
│   HTTP API     │   gRPC/OTLP    │    Kafka Consumer         │
│   (Port 8080)  │  (Port 4317)   │   (Optional)              │
└────────┬───────┴────────┬───────┴──────────┬─────────────────┘
         │                │                  │
         └────────────────┼──────────────────┘
                          │
                    ┌─────▼─────┐
                    │  Router   │
                    └─────┬─────┘
                          │
         ┌────────────────┼────────────────┐
         │                │                │
    ┌────▼────┐     ┌─────▼─────┐   ┌─────▼─────┐
    │  Logs   │     │  Metrics  │   │  Traces   │
    │ Pipeline│     │ Pipeline  │   │ Pipeline  │
    └────┬────┘     └─────┬─────┘   └─────┬─────┘
         │                │                │
         └────────────────┼────────────────┘
                          │
            ┌─────────────┴─────────────┐
            │    Storage Layer         │
            │    (Choose One)          │
            ├───────────┬───────────────┤
            │           │               │
      ┌─────▼─────┐  ┌──▼──────────┐
      │TimescaleDB│  │  TimeFusion │
      │(PostgreSQL│  │  (Direct S3 │
      │   based)  │  │   Storage)  │
      └───────────┘  └─────────────┘
```

## Core Components

### Ingestion Layer
- **HTTP API**: RESTful endpoints for direct data submission
- **gRPC/OTLP**: OpenTelemetry Protocol support for standard telemetry ingestion
- **Kafka Consumer**: Optional high-throughput ingestion from Kafka topics

### Processing Pipeline
- **Router**: Intelligently routes data to appropriate pipelines
- **Log Pipeline**: Processes and indexes log data
- **Metrics Pipeline**: Aggregates and stores time-series metrics
- **Trace Pipeline**: Correlates distributed traces

### Storage Layer (Choose One)
- **TimescaleDB**: PostgreSQL-based time-series storage, keeps all data in database
- **TimeFusion**: Direct S3 storage engine, stores all telemetry data directly in S3 buckets for cost-effective long-term storage

### AI/ML Layer
- **Anomaly Detection Engine**: Continuously analyzes data patterns
- **Natural Language Processor**: Translates natural language queries
- **Pattern Recognition**: Identifies trends and correlations

## Technology Stack

- **Language**: Haskell (core application)
- **Database**: PostgreSQL with TimescaleDB extension
- **Cache**: In-memory caching for frequently accessed data
- **Queue**: PostgreSQL-based job queue for background tasks
- **Frontend**: HTMX + Alpine.js for reactive UI with minimal JavaScript

## Data Flow

1. **Ingestion**: Data enters through HTTP, gRPC, or Kafka
2. **Validation**: Schema validation and authentication
3. **Enrichment**: Add metadata, correlate with existing data
4. **Storage**:
   - **TimescaleDB Option**: Write to PostgreSQL/TimescaleDB for immediate availability
   - **TimeFusion Option**: Write directly to S3 buckets with intelligent partitioning
5. **Indexing**: Create indexes for fast querying
6. **Analysis**: Background AI processing for anomaly detection

## Scalability Considerations

### Horizontal Scaling
- Stateless application servers can be scaled horizontally
- Load balancer distributes traffic across instances
- Shared database handles consistency

### Vertical Scaling
- TimescaleDB supports partitioning for large datasets
- Read replicas for query load distribution
- Connection pooling for database efficiency

### Performance Optimizations
- Batch processing for bulk ingestion
- Asynchronous job processing
- Aggressive caching strategies
- Query optimization with proper indexes

## Security Architecture

- **Authentication**: API keys, OAuth2/Auth0 integration
- **Authorization**: Project-based access control
- **Encryption**: TLS for transport, optional encryption at rest
- **Audit Logging**: All API access logged for compliance

## High Availability

- **Database**: Primary-replica setup with automatic failover
- **Application**: Multiple instances behind load balancer
- **Storage**: S3 provides built-in redundancy
- **Monitoring**: Self-monitoring with fallback alerting

## Development Philosophy

- **Simple Deployment**: Single binary with minimal dependencies
- **Configuration as Code**: Environment variables for all settings
- **Observability First**: Comprehensive internal metrics
- **Developer Experience**: Clear APIs, good error messages