-- Defensive follow-up to 0077. Idempotent. Runs on every deploy to:
--   1. Make sure every active project has an @everyone team. 0077's data
--      backfill is a no-op for projects that somehow don't have one, which
--      would silently drop their integration data when the old columns were
--      dropped. This backstops that.
--   2. Re-sync apis.slack.channel_id (the OAuth-time default channel that
--      stays on apis.slack as a display cache) into @everyone.slack_channels.
--      This catches any Slack installs that happened between 0077's run and
--      now, and makes the convergence rule explicit rather than implicit.
--
-- Discord's notifs_channel_id and the apis.pagerduty table are gone after
-- 0077, so we can't re-sync them here — they were a one-shot backfill.

BEGIN;

-- Creator is NOT NULL on projects.teams, so borrow any active project member's
-- user id. If a project has no members at all there's nothing we can do — skip.
INSERT INTO projects.teams
  (project_id, created_by, name, description, handle, is_everyone, members,
   notify_emails, slack_channels, discord_channels, phone_numbers, pagerduty_services, disabled_channels)
SELECT p.id, pm.user_id, 'Everyone', 'All project members and configured integrations', 'everyone', TRUE,
       '{}'::uuid[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[]
FROM projects.projects p
JOIN LATERAL (
  SELECT pm.user_id FROM projects.project_members pm
  WHERE pm.project_id = p.id AND pm.active = TRUE AND pm.deleted_at IS NULL
  ORDER BY pm.created_at ASC
  LIMIT 1
) pm ON TRUE
WHERE p.deleted_at IS NULL
  AND NOT EXISTS (
    SELECT 1 FROM projects.teams t
    WHERE t.project_id = p.id AND t.is_everyone = TRUE AND t.deleted_at IS NULL
  )
ON CONFLICT (project_id, handle) DO NOTHING;

UPDATE projects.teams t
SET slack_channels = ARRAY(
      SELECT DISTINCT x FROM unnest(t.slack_channels || ARRAY[s.channel_id]) x
      WHERE x IS NOT NULL AND x <> ''
    )
FROM apis.slack s
WHERE t.project_id = s.project_id
  AND t.is_everyone = TRUE AND t.deleted_at IS NULL
  AND s.channel_id IS NOT NULL AND s.channel_id <> ''
  AND NOT (s.channel_id = ANY (t.slack_channels));

COMMIT;
