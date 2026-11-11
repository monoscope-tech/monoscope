-- Seen only from automated browsers (OTel user_agent.synthetic.type); the first real occurrence clears it.
ALTER TABLE apis.error_patterns ADD COLUMN IF NOT EXISTS synthetic boolean NOT NULL DEFAULT false;
