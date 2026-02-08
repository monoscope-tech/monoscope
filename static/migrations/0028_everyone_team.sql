BEGIN;

ALTER TABLE projects.teams ADD COLUMN IF NOT EXISTS is_everyone BOOLEAN DEFAULT FALSE;

-- Unique constraint: one @everyone per project
CREATE UNIQUE INDEX IF NOT EXISTS idx_teams_everyone_unique
  ON projects.teams(project_id) WHERE is_everyone = TRUE AND deleted_at IS NULL;

-- Create @everyone for existing projects
INSERT INTO projects.teams (project_id, name, handle, description, is_everyone, members, notify_emails, slack_channels, discord_channels, phone_numbers, pagerduty_services)
SELECT p.id, 'Everyone', 'everyone', 'All project members and configured integrations', TRUE,
  '{}'::uuid[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[]
FROM projects.projects p
WHERE NOT EXISTS (
  SELECT 1 FROM projects.teams t WHERE t.project_id = p.id AND t.is_everyone = TRUE AND t.deleted_at IS NULL
);

COMMIT;
