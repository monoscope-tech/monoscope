BEGIN;

-- Add example payloads to shapes table for displaying context in issues
ALTER TABLE apis.shapes
ADD COLUMN IF NOT EXISTS example_request_payload JSONB NOT NULL DEFAULT '{}'::jsonb,
ADD COLUMN IF NOT EXISTS example_response_payload JSONB NOT NULL DEFAULT '{}'::jsonb;

-- Add new issue types for API changes
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'new_endpoint';
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'new_shape';
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'field_change';

COMMIT;
