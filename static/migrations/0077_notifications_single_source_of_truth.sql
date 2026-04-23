-- Consolidate notification routing onto the @everyone team.
-- apis.slack keeps credentials + OAuth-time channel metadata (display cache).
-- apis.discord keeps only guild_id. apis.pagerduty is dropped entirely.
-- projects.projects loses its enum toggles / legacy columns.

BEGIN;

-- Routing target arrays on @everyone. Union in any values still only on the
-- integration tables so we don't lose OAuth defaults / live integration keys.
UPDATE projects.teams t
SET slack_channels = ARRAY(
      SELECT DISTINCT x FROM unnest(t.slack_channels || ARRAY[s.channel_id]) x
      WHERE x IS NOT NULL AND x <> ''
    )
FROM apis.slack s
WHERE t.project_id = s.project_id
  AND t.is_everyone = TRUE AND t.deleted_at IS NULL
  AND s.channel_id IS NOT NULL AND s.channel_id <> '';

UPDATE projects.teams t
SET discord_channels = ARRAY(
      SELECT DISTINCT x FROM unnest(t.discord_channels || ARRAY[d.notifs_channel_id]) x
      WHERE x IS NOT NULL AND x <> ''
    )
FROM apis.discord d
WHERE t.project_id = d.project_id
  AND t.is_everyone = TRUE AND t.deleted_at IS NULL
  AND d.notifs_channel_id IS NOT NULL AND d.notifs_channel_id <> '';

UPDATE projects.teams t
SET pagerduty_services = ARRAY(
      SELECT DISTINCT x FROM unnest(t.pagerduty_services || ARRAY[p.integration_key]) x
      WHERE x IS NOT NULL AND x <> ''
    )
FROM apis.pagerduty p
WHERE t.project_id = p.project_id
  AND t.is_everyone = TRUE AND t.deleted_at IS NULL;

-- disabled_channels: channels the user has explicitly muted. A channel type
-- not listed here is enabled by default. Derive from the old per-project
-- notifications_channel enum array: whatever is NOT in the old list is disabled.
ALTER TABLE projects.teams
  ADD COLUMN IF NOT EXISTS disabled_channels TEXT[] DEFAULT '{}';

UPDATE projects.teams t
SET disabled_channels = ARRAY(
      SELECT ch FROM unnest(ARRAY['slack','discord','email','phone','pagerduty']) ch
      WHERE NOT (
        ch = ANY (
          SELECT CASE e::text
                   WHEN 'slack' THEN 'slack'
                   WHEN 'discord' THEN 'discord'
                   WHEN 'email' THEN 'email'
                   WHEN 'phone' THEN 'phone'
                   WHEN 'pagerduty' THEN 'pagerduty'
                 END
          FROM unnest(COALESCE(p.notifications_channel, '{}'::notification_channel_enum[])) e
        )
      )
    )
FROM projects.projects p
WHERE t.project_id = p.id
  AND t.is_everyone = TRUE AND t.deleted_at IS NULL;

-- Now drop the duplicated/legacy storage.
ALTER TABLE apis.slack     DROP COLUMN IF EXISTS webhook_url;
ALTER TABLE apis.discord   DROP COLUMN IF EXISTS notifs_channel_id;
DROP TABLE IF EXISTS apis.pagerduty;

ALTER TABLE projects.projects DROP COLUMN IF EXISTS notifications_channel;
ALTER TABLE projects.projects DROP COLUMN IF EXISTS notify_phone_number;
ALTER TABLE projects.projects DROP COLUMN IF EXISTS discord_url;

DROP TYPE IF EXISTS notification_channel_enum;

COMMIT;
