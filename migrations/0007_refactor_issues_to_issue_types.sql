-- Migration: Refactor issues table to use issue types instead of anomaly types
-- This is a breaking change - all existing issues will be deleted

BEGIN;

-- Drop existing issues table and recreate with new structure
DROP TABLE IF EXISTS apis.issues CASCADE;

-- Create new issue type enum
CREATE TYPE apis.issue_type AS ENUM ('api_change', 'runtime_exception', 'query_alert');

-- Create new issues table
CREATE TABLE apis.issues (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
    issue_type apis.issue_type NOT NULL,
    endpoint_hash TEXT NOT NULL DEFAULT '',
    
    -- Status fields
    acknowledged_at TIMESTAMPTZ,
    acknowledged_by UUID REFERENCES users.users(id),
    archived_at TIMESTAMPTZ,
    
    -- Issue details
    title TEXT NOT NULL,
    service TEXT NOT NULL DEFAULT 'Unknown',
    critical BOOLEAN NOT NULL DEFAULT false,
    severity TEXT NOT NULL DEFAULT 'warning', -- 'critical', 'warning', 'info'
    
    -- Impact metrics
    affected_requests INTEGER NOT NULL DEFAULT 0,
    affected_clients INTEGER NOT NULL DEFAULT 0,
    error_rate DOUBLE PRECISION,
    
    -- Actions
    recommended_action TEXT NOT NULL DEFAULT 'Review the issue and take appropriate action.',
    migration_complexity TEXT NOT NULL DEFAULT 'medium', -- 'low', 'medium', 'high', 'n/a'
    
    -- Polymorphic data storage
    issue_data JSONB NOT NULL DEFAULT '{}',
    
    -- LLM enhancement tracking
    llm_enhanced_at TIMESTAMPTZ,
    llm_enhancement_version INTEGER,
    
    -- Constraints
    CONSTRAINT unique_open_api_change_per_endpoint UNIQUE (project_id, issue_type, endpoint_hash) 
        WHERE issue_type = 'api_change' AND acknowledged_at IS NULL AND archived_at IS NULL
);

-- Create indexes for efficient querying
CREATE INDEX idx_issues_project_type ON apis.issues(project_id, issue_type);
CREATE INDEX idx_issues_endpoint_hash ON apis.issues(endpoint_hash) WHERE endpoint_hash != '';
CREATE INDEX idx_issues_status ON apis.issues(project_id, acknowledged_at, archived_at);
CREATE INDEX idx_issues_created_at ON apis.issues(created_at DESC);
CREATE INDEX idx_issues_critical ON apis.issues(critical, created_at DESC) WHERE critical = true;
CREATE INDEX idx_issues_llm_enhancement ON apis.issues(llm_enhanced_at) WHERE llm_enhanced_at IS NULL;

-- Create a view for backward compatibility with anomaly-based queries (temporary)
CREATE OR REPLACE VIEW apis.issues_legacy AS
SELECT 
    id,
    created_at,
    updated_at,
    project_id,
    acknowledged_at,
    CASE 
        WHEN issue_type = 'api_change' THEN 'shape'::apis.anomaly_type
        WHEN issue_type = 'runtime_exception' THEN 'runtime_exception'::apis.anomaly_type
        ELSE 'unknown'::apis.anomaly_type
    END as anomaly_type,
    endpoint_hash as target_hash,
    issue_data,
    NULL::UUID as endpoint_id,
    acknowledged_by,
    archived_at,
    title,
    service,
    critical,
    CASE 
        WHEN issue_data->>'deletedFields' IS NOT NULL 
        THEN jsonb_array_length(issue_data->'deletedFields')
        ELSE 0 
    END as breaking_changes,
    CASE 
        WHEN issue_data->>'newFields' IS NOT NULL 
        THEN jsonb_array_length(issue_data->'newFields')
        ELSE 0 
    END as incremental_changes,
    affected_requests as affected_payloads,
    affected_clients,
    'N/A' as estimated_requests,
    migration_complexity,
    recommended_action,
    '[]'::jsonb as request_payloads,
    '[]'::jsonb as response_payloads,
    COALESCE(issue_data->'anomalyHashes', '[]'::jsonb) as anomaly_hashes,
    endpoint_hash
FROM apis.issues;

-- Add comment explaining the migration
COMMENT ON TABLE apis.issues IS 'Unified issues table supporting API changes, runtime exceptions, and query alerts';
COMMENT ON COLUMN apis.issues.issue_type IS 'Type of issue: api_change, runtime_exception, or query_alert';
COMMENT ON COLUMN apis.issues.endpoint_hash IS 'For API changes, the hash of the affected endpoint. Empty for other issue types';
COMMENT ON COLUMN apis.issues.issue_data IS 'Polymorphic JSON data specific to each issue type';

COMMIT;