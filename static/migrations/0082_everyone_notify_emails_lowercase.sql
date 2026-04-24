-- 0081 seeded @everyone.notify_emails from users.users.email but preserved the
-- original case. The runtime sync paths (insertProjectMembers, softDelete, …)
-- compare via lower(), so mixed-case entries would pile up as case-duplicates.
-- Normalize all existing entries to lowercase once. Idempotent.
UPDATE projects.teams
SET notify_emails = ARRAY(
  SELECT DISTINCT lower(e) FROM unnest(notify_emails) e
)
WHERE is_everyone = TRUE AND deleted_at IS NULL;
