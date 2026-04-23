-- Heal the NULL inversion bug from 0077.
--
-- 0077 inverted projects.notifications_channel into @everyone.disabled_channels,
-- but COALESCE(NULL, '{}') made every channel "not in the enabled set" → every
-- channel got disabled. Result: any project whose notifications_channel was
-- NULL (legal — the column was nullable) came out of 0077 with zero alerts.
--
-- "All five types disabled" is never a real user intent (if you want silence,
-- you just don't configure channels — the disabled toggle is for muting one
-- type while keeping the others). Reset any @everyone team with the full set
-- muted back to empty (all enabled by default).
--
-- Idempotent.

BEGIN;

UPDATE projects.teams
SET disabled_channels = '{}'
WHERE is_everyone = TRUE
  AND deleted_at IS NULL
  AND 'slack' = ANY (disabled_channels)
  AND 'discord' = ANY (disabled_channels)
  AND 'email' = ANY (disabled_channels)
  AND 'phone' = ANY (disabled_channels)
  AND 'pagerduty' = ANY (disabled_channels);

COMMIT;
