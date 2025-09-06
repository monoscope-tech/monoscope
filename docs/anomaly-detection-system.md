# Anomaly Detection System Documentation

## Overview

The APIToolkit anomaly detection system automatically identifies and tracks changes in API behavior, including:
- New endpoints, request/response shapes, and field formats
- Runtime exceptions and errors
- Performance degradations and threshold violations

The system is designed to minimize noise by grouping related anomalies into unified issues that developers can easily understand and act upon.

## Architecture

### 1. Data Flow

```
HTTP Requests → PubSub/OTLP → Span Processing → Entity Extraction → Database Triggers → Background Jobs → Issue Creation → Notifications
```

### 2. Key Components

#### A. Request Ingestion (`src/ProcessMessage.hs`)
- Receives HTTP request data via PubSub messages or OTLP protocol
- Converts to OpenTelemetry span format for unified processing
- Stores in `otel_logs_and_spans` table with computed hashes

#### B. Entity Extraction (`src/ProcessMessage.hs:processSpanToEntities`)
- Extracts from each span:
  - **Endpoints**: Unique API routes (method + path + host)
  - **Shapes**: Request/response structure for each status code
  - **Fields**: Individual data fields with their paths
  - **Formats**: Field value patterns and examples
- Uses XXHash for deterministic hash generation
- Implements caching to reduce database operations

#### C. Anomaly Detection (Database Layer)
- PostgreSQL triggers on entity tables:
  - `endpoints_created_anomaly` - Fires on new endpoint insertion
  - `shapes_created_anomaly` - Fires on new shape insertion
  - `fields_created_anomaly` - Fires on new field insertion
  - `format_created_anomaly` - Fires on new format insertion
  - `error_created_anomaly` - Fires on new error insertion
- Stored procedure `apis.new_anomaly_proc()`:
  - Creates anomaly records in `apis.anomalies` table
  - Batches anomalies into background jobs by project/type
  - Deduplicates to prevent multiple jobs for same anomalies

#### D. Background Job Processing (`src/BackgroundJobs.hs`)
- **FiveMinuteSpanProcessing**: Processes spans to extract entities
- **OneMinuteErrorProcessing**: Extracts errors from span events
- **NewAnomaly**: Converts anomalies to user-facing issues

#### E. Issue Management (`src/Models/Apis/Issues.hs`)
- Three issue types:
  - **APIChange**: Groups endpoint/shape/format changes by endpoint
  - **RuntimeException**: Individual issues for each error pattern
  - **QueryAlert**: Threshold violations from monitoring
- Issue lifecycle: Created → Enhanced (LLM) → Acknowledged → Archived

#### F. Notification System (`src/Pkg/Mail.hs`)
- Multi-channel support: Email (Postmark), Slack, Discord, WhatsApp
- Template-based notifications with project-specific preferences
- Batched notifications to prevent spam

## Implementation Details

### Hash Calculation

Hashes are hierarchical to enable efficient lookups:
- Endpoint: `XXHash(projectId + host + method + urlPath)`
- Shape: `XXHash(endpointHash + statusCode + sortedFieldPaths)`
- Field: `XXHash(endpointHash + category + fieldPath)`
- Format: `XXHash(fieldHash + valuePattern)`

### Anomaly Types

1. **Endpoint Anomalies**
   - Triggered when a new API endpoint is discovered
   - Groups all related changes under single issue

2. **Shape Anomalies**
   - Triggered when request/response structure changes
   - Tracks added/removed/modified fields

3. **Format Anomalies**
   - Triggered when field value patterns change
   - Useful for detecting breaking changes in data formats

4. **Runtime Exceptions**
   - Extracted from OpenTelemetry span events
   - Groups by error type, message, and stack trace

### Error Detection Process

1. Every minute, queries spans with error indicators:
   - Status code = 'ERROR' or '2'
   - Events containing 'exception' or 'error'
   - Attributes indicating errors

2. Extracts error details from span events:
   - Exception type, message, stack trace
   - HTTP context (method, path)
   - Service and technology info

3. Creates runtime exception issues with:
   - Error grouping by hash
   - Occurrence counting
   - First/last seen timestamps

### Issue Enhancement

- Optional LLM integration for better issue titles and recommendations
- Analyzes code changes to suggest migration complexity
- Provides actionable recommendations for developers

## Database Schema

### Key Tables

- `apis.anomalies` - Raw anomaly records (legacy, being phased out)
- `apis.issues` - User-facing issues with enhanced information
- `apis.endpoints` - Discovered API endpoints
- `apis.shapes` - Request/response structures
- `apis.fields` - Individual data fields
- `apis.formats` - Field value patterns
- `apis.errors` - Runtime exceptions

### Important Columns

- `hash` - Deterministic identifier for deduplication
- `endpoint_hash` - Links shapes/fields to endpoints
- `issue_data` - Polymorphic JSON for different issue types
- `llm_enhanced_at` - Tracks AI enhancement status

## Configuration

### Project Settings
- `redact_fields_list` - Fields to redact from stored data
- `notification_channels` - Enabled notification methods
- `payment_plan` - Affects rate limiting and features

### Environment Variables
- Rate limits for free tier projects
- OpenAI API key for enhancement
- Notification service credentials

## Testing Considerations

### Current Test Coverage
- `ProcessMessageSpec` - Basic message processing
- `AnomalyListSpec` - UI rendering (currently disabled)
- Integration tests for entity extraction

### Recommended Tests
1. Trigger execution validation
2. Error extraction edge cases
3. Notification delivery confirmation
4. Issue grouping logic
5. Hash collision handling

## Common Operations

### Monitoring Anomaly Detection
```sql
-- Check recent anomalies
SELECT * FROM apis.anomalies 
WHERE created_at > NOW() - INTERVAL '1 hour'
ORDER BY created_at DESC;

-- Check background job queue
SELECT * FROM background_jobs 
WHERE tag = 'NewAnomaly' 
AND status = 'pending';
```

### Troubleshooting

1. **Missing Anomalies**
   - Check if triggers are enabled
   - Verify background jobs are running
   - Check for rate limiting on free tier

2. **Duplicate Issues**
   - Verify endpoint hash calculation
   - Check unique constraints on issues table

3. **Missing Notifications**
   - Verify notification channels configured
   - Check notification service logs
   - Verify user email addresses

## Future Improvements

1. **Performance**
   - Implement bloom filters for hash lookups
   - Optimize shape comparison algorithms
   - Add caching for frequently accessed data

2. **Features**
   - Machine learning for anomaly severity scoring
   - Custom anomaly detection rules
   - Webhook integrations for notifications

3. **Reliability**
   - Add retry mechanism for failed notifications
   - Implement dead letter queue for failed jobs
   - Add health checks for detection pipeline