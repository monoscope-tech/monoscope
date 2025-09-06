-- Anomaly Detection Database Triggers Documentation
-- =================================================
-- 
-- The anomaly detection system uses PostgreSQL triggers to automatically detect
-- when new API entities are created. This ensures 100% detection coverage
-- regardless of how data enters the system.

-- Core Stored Procedure: apis.new_anomaly_proc()
-- ----------------------------------------------
-- This procedure is called by all anomaly detection triggers.
-- It performs the following actions:
-- 
-- 1. Creates an anomaly record in apis.anomalies table (unless skip flag is set)
-- 2. Creates or updates a background job to process the anomaly
-- 3. Batches multiple anomalies for the same project/type into a single job
--
-- Parameters:
-- - anomaly_type: 'endpoint', 'shape', 'field', 'format', 'runtime_exception'
-- - anomaly_action: 'created', 'updated', 'deleted' (currently only 'created' is used)
-- - skip_anomaly_record: Optional flag to skip creating anomaly record (used for errors)
--
-- The procedure intelligently batches anomalies by:
-- - Finding existing pending jobs for the same project and anomaly type
-- - Appending new target hashes to existing jobs
-- - Creating new jobs only when necessary

-- Trigger: endpoints_created_anomaly
-- ----------------------------------
-- Fires: AFTER INSERT ON apis.endpoints
-- Purpose: Detects new API endpoints
-- Impact: Creates endpoint anomaly, triggers APIChange issue creation
CREATE OR REPLACE TRIGGER endpoint_created_anomaly 
AFTER INSERT ON apis.endpoints 
FOR EACH ROW 
EXECUTE PROCEDURE apis.new_anomaly_proc('endpoint', 'created');

-- Trigger: shapes_created_anomaly
-- --------------------------------
-- Fires: AFTER INSERT ON apis.shapes
-- Purpose: Detects new request/response structures
-- Impact: Creates shape anomaly, groups with endpoint anomalies into APIChange issues
-- Note: Shape represents the "schema" of an API call for a specific status code
CREATE OR REPLACE TRIGGER shapes_created_anomaly 
AFTER INSERT ON apis.shapes 
FOR EACH ROW 
EXECUTE PROCEDURE apis.new_anomaly_proc('shape', 'created');

-- Trigger: fields_created_anomaly
-- --------------------------------
-- Fires: AFTER INSERT ON apis.fields
-- Purpose: Detects new data fields in requests/responses
-- Impact: Creates field anomaly, groups with endpoint anomalies
-- Note: Fields are individual data elements with their JSON paths
CREATE OR REPLACE TRIGGER fields_created_anomaly 
AFTER INSERT ON apis.fields 
FOR EACH ROW 
EXECUTE PROCEDURE apis.new_anomaly_proc('field', 'created');

-- Trigger: format_created_anomaly
-- --------------------------------
-- Fires: AFTER INSERT ON apis.formats
-- Purpose: Detects new field value patterns
-- Impact: Creates format anomaly, useful for detecting data type changes
-- Note: Formats capture examples of field values for pattern detection
CREATE OR REPLACE TRIGGER format_created_anomaly 
AFTER INSERT ON apis.formats 
FOR EACH ROW 
EXECUTE PROCEDURE apis.new_anomaly_proc('format', 'created');

-- Trigger: error_created_anomaly
-- -------------------------------
-- Fires: AFTER INSERT ON apis.errors
-- Purpose: Detects new runtime exceptions
-- Impact: Creates runtime exception issues (one per unique error)
-- Note: Uses 'skip_anomaly_record' flag since errors go directly to issues
CREATE OR REPLACE TRIGGER error_created_anomaly 
AFTER INSERT ON apis.errors 
FOR EACH ROW 
EXECUTE PROCEDURE apis.new_anomaly_proc('runtime_exception', 'created', 'skip_anomaly_record');

-- Background Job Creation
-- ----------------------
-- The new_anomaly_proc creates entries in the background_jobs table with:
-- - tag: 'NewAnomaly'
-- - payload: Contains projectId, anomalyType, anomalyAction, and targetHashes array
-- - status: 'pending'
--
-- The background job processor (BackgroundJobs.newAnomalyJob) then:
-- 1. Groups API changes by endpoint to prevent notification spam
-- 2. Creates individual issues for runtime exceptions
-- 3. Queues LLM enhancement jobs if configured
-- 4. Sends notifications based on project settings

-- Example: What happens when a new endpoint is discovered
-- -------------------------------------------------------
-- 1. ProcessMessage.processSpanToEntities extracts endpoint from HTTP span
-- 2. BackgroundJobs.bulkInsertEndpoints inserts with ON CONFLICT DO NOTHING
-- 3. If new endpoint, endpoint_created_anomaly trigger fires
-- 4. apis.new_anomaly_proc creates anomaly record and background job
-- 5. BackgroundJobs.newAnomalyJob processes the anomaly
-- 6. Issues.createAPIChangeIssue creates user-facing issue
-- 7. Notifications sent via configured channels

-- Monitoring Queries
-- -----------------
-- Check recent anomalies:
SELECT 
  a.created_at,
  a.anomaly_type,
  a.target_hash,
  p.title as project_name
FROM apis.anomalies a
JOIN projects p ON a.project_id = p.id
WHERE a.created_at > NOW() - INTERVAL '1 hour'
ORDER BY a.created_at DESC;

-- Check pending anomaly jobs:
SELECT 
  j.created_at,
  j.payload->>'projectId' as project_id,
  j.payload->>'anomalyType' as anomaly_type,
  jsonb_array_length(j.payload->'targetHashes') as anomaly_count
FROM background_jobs j
WHERE j.tag = 'NewAnomaly' 
  AND j.status = 'pending'
ORDER BY j.created_at DESC;

-- Check trigger status:
SELECT 
  tgname as trigger_name,
  tgenabled as enabled,
  tgrelid::regclass as table_name
FROM pg_trigger
WHERE tgname LIKE '%anomaly%'
ORDER BY tgname;