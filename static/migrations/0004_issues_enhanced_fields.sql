-- Add enhanced fields to the issues table for better UI display
ALTER TABLE apis.issues 
ADD COLUMN IF NOT EXISTS title TEXT DEFAULT '',
ADD COLUMN IF NOT EXISTS service TEXT DEFAULT '',
ADD COLUMN IF NOT EXISTS critical BOOLEAN DEFAULT FALSE,
ADD COLUMN IF NOT EXISTS breaking_changes INTEGER DEFAULT 0,
ADD COLUMN IF NOT EXISTS incremental_changes INTEGER DEFAULT 0,
ADD COLUMN IF NOT EXISTS affected_payloads INTEGER DEFAULT 0,
ADD COLUMN IF NOT EXISTS affected_clients INTEGER DEFAULT 0,
ADD COLUMN IF NOT EXISTS estimated_requests TEXT DEFAULT '',
ADD COLUMN IF NOT EXISTS migration_complexity TEXT DEFAULT 'low',
ADD COLUMN IF NOT EXISTS recommended_action TEXT DEFAULT '',
ADD COLUMN IF NOT EXISTS request_payloads JSONB DEFAULT '[]'::jsonb,
ADD COLUMN IF NOT EXISTS response_payloads JSONB DEFAULT '[]'::jsonb;

-- Add indexes for commonly queried fields
CREATE INDEX IF NOT EXISTS idx_issues_critical ON apis.issues (critical) WHERE critical = TRUE;
CREATE INDEX IF NOT EXISTS idx_issues_breaking_changes ON apis.issues (breaking_changes) WHERE breaking_changes > 0;
CREATE INDEX IF NOT EXISTS idx_issues_service ON apis.issues (service);

-- Update the updated_at timestamp when these columns change
-- (Assuming manage_updated_at trigger already exists on the table)