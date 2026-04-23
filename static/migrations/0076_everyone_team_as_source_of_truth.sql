-- Make the @everyone team the single source of truth for project notification settings.
-- Backfill any existing data from projects.projects + integration tables into @everyone,
-- then drop the redundant project-level columns.

BEGIN;

-- Notify emails and phone numbers were previously stored on projects.projects.
-- Merge them into the corresponding @everyone team, union-ing with any values
-- already on the team so we never lose data.
UPDATE projects.teams t
SET
  notify_emails = ARRAY(
    SELECT DISTINCT x FROM unnest(t.notify_emails || COALESCE(p.notify_emails, '{}'::text[])) x
    WHERE x IS NOT NULL AND x <> ''
  ),
  phone_numbers = ARRAY(
    SELECT DISTINCT x FROM unnest(t.phone_numbers || COALESCE(p.whatsapp_numbers, '{}'::text[])) x
    WHERE x IS NOT NULL AND x <> ''
  )
FROM projects.projects p
WHERE t.project_id = p.id
  AND t.is_everyone = TRUE
  AND t.deleted_at IS NULL;

-- Fold the current default Discord channel into @everyone.discord_channels.
UPDATE projects.teams t
SET discord_channels = ARRAY(
      SELECT DISTINCT x FROM unnest(t.discord_channels || ARRAY[d.notifs_channel_id]) x
      WHERE x IS NOT NULL AND x <> ''
    )
FROM apis.discord d
WHERE t.project_id = d.project_id
  AND t.is_everyone = TRUE
  AND t.deleted_at IS NULL
  AND d.notifs_channel_id IS NOT NULL
  AND d.notifs_channel_id <> '';

-- Fold PagerDuty integration keys into @everyone.pagerduty_services.
UPDATE projects.teams t
SET pagerduty_services = ARRAY(
      SELECT DISTINCT x FROM unnest(t.pagerduty_services || ARRAY[pd.integration_key]) x
      WHERE x IS NOT NULL AND x <> ''
    )
FROM apis.pagerduty pd
WHERE t.project_id = pd.project_id
  AND t.is_everyone = TRUE
  AND t.deleted_at IS NULL;

ALTER TABLE projects.projects DROP COLUMN IF EXISTS notify_emails;
ALTER TABLE projects.projects DROP COLUMN IF EXISTS whatsapp_numbers;

COMMIT;
