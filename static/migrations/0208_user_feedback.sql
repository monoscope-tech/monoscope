-- User feedback issues (spans/logs named user.feedback), and a spam mark any issue can carry.
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'feedback';
ALTER TABLE apis.issues ADD COLUMN IF NOT EXISTS spam_at TIMESTAMPTZ;
