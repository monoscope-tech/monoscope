-- Rebuild issues table with new structure for grouped anomalies and enhanced issue tracking
BEGIN;

-- Drop the old issues table
DROP TABLE IF EXISTS apis.issues CASCADE;

-- Drop and recreate issue type enum to ensure clean state
DROP TYPE IF EXISTS apis.issue_type CASCADE;
CREATE TYPE apis.issue_type AS ENUM ('api_change', 'runtime_exception', 'query_alert');

-- Create the new issues table
CREATE TABLE apis.issues
(
  id                        UUID NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  created_at                TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at                TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  project_id                UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
  issue_type                apis.issue_type NOT NULL,
  endpoint_hash             TEXT NOT NULL DEFAULT '', -- For API changes only
  
  -- Status fields
  acknowledged_at           TIMESTAMP WITH TIME ZONE,
  acknowledged_by           UUID REFERENCES users.users (id) ON DELETE SET NULL,
  archived_at               TIMESTAMP WITH TIME ZONE,
  
  -- Issue details
  title                     TEXT NOT NULL DEFAULT '',
  service                   TEXT NOT NULL DEFAULT '',
  critical                  BOOLEAN NOT NULL DEFAULT FALSE,
  severity                  TEXT NOT NULL DEFAULT 'info', -- 'critical', 'warning', 'info'
  
  -- Impact metrics
  affected_requests         INTEGER NOT NULL DEFAULT 0,
  affected_clients          INTEGER NOT NULL DEFAULT 0,
  error_rate                NUMERIC(5,2),
  
  -- Actions
  recommended_action        TEXT NOT NULL DEFAULT '',
  migration_complexity      TEXT NOT NULL DEFAULT 'low', -- 'low', 'medium', 'high', 'n/a'
  
  -- Polymorphic issue data
  issue_data                JSONB NOT NULL DEFAULT '{}'::jsonb,
  
  -- Payload changes tracking (for API changes)
  request_payloads          JSONB NOT NULL DEFAULT '[]'::jsonb,
  response_payloads         JSONB NOT NULL DEFAULT '[]'::jsonb,
  
  -- LLM enhancement tracking
  llm_enhanced_at           TIMESTAMP WITH TIME ZONE,
  llm_enhancement_version   INTEGER DEFAULT 1,
  
  -- Constraint: Only one unacknowledged/unarchived API change issue per endpoint
  CONSTRAINT unique_open_api_change_per_endpoint UNIQUE (project_id, issue_type, endpoint_hash) 
    DEFERRABLE INITIALLY DEFERRED
);

-- Add trigger for updated_at
SELECT manage_updated_at('apis.issues');

-- Create indexes
CREATE INDEX idx_issues_project_id ON apis.issues(project_id);
CREATE INDEX idx_issues_issue_type ON apis.issues(issue_type);
CREATE INDEX idx_issues_endpoint_hash ON apis.issues(endpoint_hash) WHERE endpoint_hash != '';
CREATE INDEX idx_issues_critical ON apis.issues(critical) WHERE critical = TRUE;
CREATE INDEX idx_issues_severity ON apis.issues(severity);
CREATE INDEX idx_issues_open ON apis.issues(project_id, acknowledged_at, archived_at) 
  WHERE acknowledged_at IS NULL AND archived_at IS NULL;
CREATE INDEX idx_issues_endpoint_open ON apis.issues(project_id, endpoint_hash, issue_type) 
  WHERE acknowledged_at IS NULL AND archived_at IS NULL AND issue_type = 'api_change';
CREATE INDEX idx_issues_llm_unenhanced ON apis.issues(created_at) 
  WHERE llm_enhanced_at IS NULL;
CREATE INDEX idx_issues_created_at ON apis.issues(created_at DESC);

-- Create a partial unique index to enforce the constraint only for open issues
CREATE UNIQUE INDEX unique_open_api_change_issue_per_endpoint 
  ON apis.issues(project_id, endpoint_hash) 
  WHERE issue_type = 'api_change' 
    AND acknowledged_at IS NULL 
    AND archived_at IS NULL 
    AND endpoint_hash != '';

-- Drop the old anomaly grouping columns from anomalies table if they exist
-- (These are now handled in the issues table)
ALTER TABLE apis.anomalies DROP COLUMN IF EXISTS issue_id;

COMMIT;
