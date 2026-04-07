-- Backfill: any project that already has a Slack integration row should have
-- 'slack' in its notifications_channel so alerts actually get dispatched.
UPDATE projects.projects p
SET notifications_channel = notifications_channel || ARRAY['slack']::notification_channel_enum[]
FROM apis.slack s
WHERE s.project_id = p.id
  AND NOT ('slack'::notification_channel_enum = ANY(p.notifications_channel));
