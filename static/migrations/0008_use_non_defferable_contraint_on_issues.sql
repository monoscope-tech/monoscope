BEGIN;

-- Drop the old deferrable constraint
ALTER TABLE apis.issues
DROP CONSTRAINT unique_open_api_change_per_endpoint;

-- Add the new, non-deferrable constraint
ALTER TABLE apis.issues
ADD CONSTRAINT unique_open_api_change_per_endpoint
UNIQUE (project_id, issue_type, endpoint_hash);

COMMIT;