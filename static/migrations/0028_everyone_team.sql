BEGIN;

ALTER TABLE projects.teams ADD COLUMN IF NOT EXISTS is_everyone BOOLEAN DEFAULT FALSE;

-- Unique constraint: one @everyone per project
CREATE UNIQUE INDEX IF NOT EXISTS idx_teams_everyone_unique
  ON projects.teams(project_id) WHERE is_everyone = TRUE AND deleted_at IS NULL;

-- Create @everyone for existing projects that have at least one member
INSERT INTO projects.teams (project_id, name, handle, description, is_everyone, members, notify_emails, slack_channels, discord_channels, phone_numbers, pagerduty_services, created_by)
SELECT p.id, 'Everyone', 'everyone', 'All project members and configured integrations', TRUE,
  '{}'::uuid[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[],
  pm.user_id
FROM projects.projects p
CROSS JOIN LATERAL (
  SELECT user_id FROM projects.project_members
  WHERE project_id = p.id
  ORDER BY created_at
  LIMIT 1
) pm
WHERE NOT EXISTS (
  SELECT 1 FROM projects.teams t WHERE t.project_id = p.id AND t.is_everyone = TRUE AND t.deleted_at IS NULL
);

COMMIT;
