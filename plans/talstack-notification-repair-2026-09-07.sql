-- Apply after deploying the hx-vals array serialization fix.
-- Restores Talstack's requested defaults: all active members' emails and the
-- OAuth-selected Slack channel, with email and Slack enabled.
-- Guard against overwriting a newer settings save. Expect exactly one row.
-- This has not been executed against production.
BEGIN;

UPDATE projects.teams t
SET notify_emails = ARRAY(
      SELECT DISTINCT lower(u.email::text)
      FROM projects.project_members pm
      JOIN users.users u ON u.id = pm.user_id
      WHERE pm.project_id = t.project_id AND pm.active
    ),
    slack_channels = ARRAY(
      SELECT channel_id FROM apis.slack
      WHERE project_id = t.project_id AND channel_id <> ''
    ),
    disabled_channels = array_remove(array_remove(t.disabled_channels, 'email'), 'slack')
WHERE t.project_id = '6297304f-89c0-48a9-9b5c-20bcac61f54e'
  AND t.is_everyone AND t.deleted_at IS NULL
  AND t.updated_at = '2026-09-07 15:36:00.629445+00'::timestamptz
  AND t.notify_emails = ARRAY['']::text[]
  AND t.slack_channels = '{}'::text[]
  AND t.disabled_channels @> ARRAY['email', 'slack', 'discord', 'phone', 'pagerduty']
RETURNING t.project_id, cardinality(t.notify_emails) AS email_count,
          t.slack_channels, t.disabled_channels;

COMMIT;
