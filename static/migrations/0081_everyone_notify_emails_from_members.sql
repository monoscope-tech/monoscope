-- Seed @everyone.notify_emails with the emails of every active project member,
-- making notify_emails the single source of truth for who gets alerted.
-- Idempotent: DISTINCT + union with existing values means re-running is a no-op.
UPDATE projects.teams t
SET notify_emails = ARRAY(
  SELECT DISTINCT e FROM unnest(
    t.notify_emails || ARRAY(
      SELECT u.email FROM projects.project_members pm
      JOIN users.users u ON u.id = pm.user_id
      WHERE pm.project_id = t.project_id AND pm.active = TRUE
    )
  ) e
)
WHERE t.is_everyone = TRUE AND t.deleted_at IS NULL;
