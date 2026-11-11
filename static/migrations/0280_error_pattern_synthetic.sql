-- Seen only from automated browsers (OTel user_agent.synthetic.type); the first real occurrence clears it.
ALTER TABLE apis.error_patterns ADD COLUMN IF NOT EXISTS synthetic boolean NOT NULL DEFAULT false;
CREATE INDEX IF NOT EXISTS idx_error_patterns_synthetic_hash ON apis.error_patterns (project_id, hash) WHERE synthetic;
