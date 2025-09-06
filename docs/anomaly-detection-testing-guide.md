# Anomaly Detection Testing Guide

## Overview

This guide documents the comprehensive test suite created for the anomaly detection system in APIToolkit. The tests verify the complete flow from HTTP request ingestion through anomaly detection, issue creation, and notification sending.

## Test Structure

### 1. Unit Tests

#### ErrorExtractionSpec (`test/unit/Models/Telemetry/ErrorExtractionSpec.hs`)

**Purpose**: Tests the extraction of errors from OpenTelemetry spans

**Key Test Cases**:
- **Error Event Extraction**: Verifies that error events are correctly identified and extracted from span events
- **HTTP Context Extraction**: Ensures HTTP request context (method, path, host) is properly extracted from span attributes
- **Hash Generation**: Validates that consistent hashes are generated for similar errors
- **Multiple Error Handling**: Tests extraction of multiple errors from multiple spans
- **Edge Cases**: 
  - Missing events in spans
  - Malformed event data
  - Missing exception attributes
  - Extracting HTTP path from `http.target` when `http.route` is missing

**Why These Tests Matter**:
- Error extraction is critical for runtime exception anomaly detection
- Proper hash generation ensures deduplication of similar errors
- HTTP context is essential for grouping errors by endpoint

#### ProcessMessageEdgeCasesSpec (`test/unit/ProcessMessageEdgeCasesSpec.hs`)

**Purpose**: Tests edge cases in processing request messages into OpenTelemetry spans

**Key Test Cases**:
- **Malformed JSON Handling**: Tests graceful handling of invalid request/response bodies
- **Missing Fields**: Verifies system handles missing optional fields
- **Special Characters**: Tests processing of special characters in paths and parameters
- **Large Payloads**: Validates handling of oversized request/response bodies
- **Sensitive Data Redaction**: Ensures passwords, tokens, and API keys are redacted
- **Cache Behavior**: Tests caching of processed entities

**Why These Tests Matter**:
- Real-world data is often messy and incomplete
- Security is critical - sensitive data must never be stored
- Performance optimization through caching needs to work correctly

### 2. Integration Tests

#### AnomalyTriggersSpec (`test/integration/AnomalyTriggersSpec.hs`)

**Purpose**: Tests PostgreSQL database triggers that create anomalies

**Key Test Cases**:
- **Endpoint Trigger**: Verifies `endpoint_created_anomaly` trigger fires on new endpoint insertion
- **Shape Trigger**: Tests `shapes_created_anomaly` trigger for new API shapes
- **Field Trigger**: Tests field anomaly creation
- **Error Trigger**: Verifies error insertion triggers background jobs (but not anomaly records)
- **Background Job Batching**: Tests that multiple anomalies are batched into single job
- **Job Updates**: Verifies existing jobs are updated rather than creating duplicates

**Why These Tests Matter**:
- Database triggers provide 100% coverage - they can't be bypassed
- Batching reduces background job queue pressure
- Job deduplication prevents processing the same anomaly multiple times

#### AnomalyDetectionFlowSpec (`test/integration/AnomalyDetectionFlowSpec.hs`)

**Purpose**: End-to-end integration tests for the complete anomaly detection flow

**Key Test Cases**:
1. **New Endpoint Detection**:
   - Send HTTP request → Process into span → Extract entities → Trigger creates anomaly → Background job creates APIChange issue
   - Verifies the complete flow from request to issue creation

2. **Multiple Anomaly Grouping**:
   - Single request with complex JSON creates multiple entities (endpoint, shape, fields)
   - All anomalies grouped into single APIChange issue
   - Tests issue data contains all anomaly information

3. **Runtime Exception Detection**:
   - OpenTelemetry span with error event → Process errors → Create error record → Background job creates RuntimeException issue
   - Validates error extraction and issue creation

4. **Issue Updates**:
   - First request creates issue → Second request with changes updates same issue
   - Tests that issues are reused, not duplicated

**Why These Tests Matter**:
- Validates the entire system works end-to-end
- Ensures anomalies are properly grouped to avoid notification spam
- Confirms issue updates work to track ongoing changes

#### AnomalyNotificationsSpec (`test/integration/AnomalyNotificationsSpec.hs`)

**Purpose**: Tests the multi-channel notification system

**Key Test Cases**:
1. **Endpoint Anomaly Notifications**: Tests notifications for new API endpoints
2. **Runtime Error Notifications**: Validates error notifications with full context
3. **Notification Batching**: Multiple anomalies for same endpoint send single notification
4. **Channel Preferences**: Respects project settings for enabled channels (Email, Slack, Discord, WhatsApp)
5. **Notification Content**: Verifies correct data in notification payloads

**Why These Tests Matter**:
- Notifications are the user-facing output of the system
- Batching prevents notification spam
- Multi-channel support allows flexible alerting

#### AnomalyListSpec (`test/integration/Pages/Anomalies/AnomalyListSpec.hs`)

**Purpose**: Tests the anomaly list web UI endpoint

**Key Test Cases**:
- **Issue Listing**: Retrieves and displays issues correctly
- **Acknowledgment Flow**: Tests acknowledging anomalies marks them as reviewed
- **Hierarchical Display**: Parent anomalies hide children until acknowledged
- **Issue Type Filtering**: Proper filtering by APIChange vs RuntimeException

**Why These Tests Matter**:
- UI is how users interact with detected anomalies
- Acknowledgment workflow prevents re-alerting
- Hierarchical display reduces UI clutter

### 3. Test Utilities

#### AnomalyTestUtils (`test/Pkg/AnomalyTestUtils.hs`)

**Purpose**: Shared test utilities and helpers

**Key Components**:
- **Entity Creators**: Functions to create test endpoints, shapes, fields, errors
- **Verification Helpers**: Check if anomalies, issues, and jobs were created
- **Data Cleanup**: Clear test data between runs
- **Span Builder DSL**: Fluent interface for creating test OpenTelemetry spans
- **Background Job Helpers**: Process jobs and wait for completion

## Testing Flow Rationale

### Why These Specific Flows?

1. **HTTP Request → Anomaly Detection**:
   - This is the primary use case - detecting API changes from live traffic
   - Tests verify each step: ingestion → entity extraction → anomaly creation → issue generation

2. **Error Detection from Traces**:
   - Modern observability relies on OpenTelemetry
   - Errors in traces should automatically create issues
   - Tests ensure error context is preserved

3. **Anomaly Grouping**:
   - Individual field changes would create too many notifications
   - Grouping by endpoint provides the right level of abstraction
   - Tests verify grouping logic works correctly

4. **Multi-Channel Notifications**:
   - Different teams prefer different notification channels
   - Tests ensure all channels work and respect preferences

## Database Schema Assumptions

The tests assume this schema:
- `apis.endpoints`: Stores API endpoints
- `apis.shapes`: Request/response structures per endpoint
- `apis.fields`: Individual fields within shapes
- `apis.formats`: Field value patterns
- `apis.errors`: Runtime errors
- `apis.anomalies`: Individual detected changes
- `apis.issues`: User-facing grouped anomalies
- `background_jobs`: Async job queue

## TODO List for Recreating Tests

### Phase 1: Setup
- [ ] Create test database with proper schema
- [ ] Set up test fixtures and data generators
- [ ] Configure test environment variables
- [ ] Create `AnomalyTestUtils` module with helper functions

### Phase 2: Unit Tests
- [ ] Create `ErrorExtractionSpec`:
  - [ ] Test error event filtering from spans
  - [ ] Test HTTP context extraction
  - [ ] Test hash generation consistency
  - [ ] Test malformed data handling
  - [ ] Test technology stack extraction

- [ ] Create `ProcessMessageEdgeCasesSpec`:
  - [ ] Test malformed JSON handling
  - [ ] Test missing field handling
  - [ ] Test special character handling
  - [ ] Test large payload truncation
  - [ ] Test sensitive data redaction
  - [ ] Test entity caching

### Phase 3: Integration Tests
- [ ] Create `AnomalyTriggersSpec`:
  - [ ] Test endpoint creation trigger
  - [ ] Test shape creation trigger
  - [ ] Test field creation trigger
  - [ ] Test error insertion trigger
  - [ ] Test background job batching
  - [ ] Test job update logic

- [ ] Create `AnomalyDetectionFlowSpec`:
  - [ ] Test new endpoint detection flow
  - [ ] Test multiple anomaly grouping
  - [ ] Test runtime exception detection
  - [ ] Test issue update mechanism
  - [ ] Test complete end-to-end flow

- [ ] Create `AnomalyNotificationsSpec`:
  - [ ] Test Email notifications
  - [ ] Test Slack notifications
  - [ ] Test Discord notifications
  - [ ] Test WhatsApp notifications
  - [ ] Test notification batching
  - [ ] Test channel preferences

- [ ] Create `AnomalyListSpec`:
  - [ ] Test issue retrieval
  - [ ] Test acknowledgment flow
  - [ ] Test hierarchical display
  - [ ] Test filtering and pagination

### Phase 4: Test Infrastructure
- [ ] Configure test database cleanup between runs
- [ ] Set up parallel test execution
- [ ] Add test coverage reporting
- [ ] Create CI pipeline configuration
- [ ] Document how to run tests locally

### Phase 5: Additional Tests to Consider
- [ ] Performance tests for high-volume anomaly processing
- [ ] Stress tests for background job system
- [ ] Tests for LLM enhancement of issue titles
- [ ] Tests for anomaly approval workflow
- [ ] Tests for query alert anomalies
- [ ] Tests for webhook delivery

## Key Testing Principles Applied

1. **Test at Multiple Levels**: Unit tests for logic, integration tests for flows
2. **Test Real Scenarios**: Based on actual usage patterns
3. **Test Edge Cases**: Malformed data, missing fields, large payloads
4. **Test Security**: Ensure sensitive data is redacted
5. **Test Performance**: Caching and batching mechanisms
6. **Test User Experience**: Notifications and UI interactions

## Running the Tests

### Prerequisites
- PostgreSQL database with test schema
- Proper environment variables set
- All Haskell dependencies installed

### Commands
```bash
# Run unit tests
make test-unit

# Run integration tests
make test-integration

# Run all tests
make test
```

## Maintenance Notes

1. **Database Triggers**: If triggers change, update `AnomalyTriggersSpec`
2. **New Anomaly Types**: Add test cases for new types in flow tests
3. **New Notification Channels**: Add to `AnomalyNotificationsSpec`
4. **Schema Changes**: Update test data generators in `AnomalyTestUtils`