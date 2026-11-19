-- Failed Slack Connect invites are retried by later syncs only up to a cap (a bad address would otherwise retry daily forever).
ALTER TABLE projects.slack_connect_invites ADD COLUMN IF NOT EXISTS attempts INT NOT NULL DEFAULT 0;
UPDATE projects.slack_connect_invites SET attempts = 1 WHERE status = 'failed' AND attempts = 0;
