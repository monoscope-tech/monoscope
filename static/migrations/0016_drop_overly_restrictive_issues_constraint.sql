-- Drop overly restrictive table-level constraint on issues
-- The partial unique index unique_open_api_change_issue_per_endpoint already
-- enforces the correct business logic: only one open api_change issue per endpoint
BEGIN;

-- Drop the table-level constraint that prevents ANY duplicate (project_id, issue_type, endpoint_hash)
-- This was too restrictive - it prevented:
-- 1. Multiple query_alert issues for the same project
-- 2. Creating a new api_change issue after acknowledging an old one
ALTER TABLE apis.issues
DROP CONSTRAINT IF EXISTS unique_open_api_change_per_endpoint;

-- The partial unique index unique_open_api_change_issue_per_endpoint (created in migration 0007)
-- already enforces the real business requirement:
-- Only one OPEN (unacknowledged, unarchived) api_change issue per (project_id, endpoint_hash)

COMMIT;
