ALTER TABLE apis.error_patterns
  ADD COLUMN resolved_by UUID REFERENCES users.users(id) ON DELETE SET NULL;

CREATE OR REPLACE FUNCTION apis.log_auto_resolve_activity() RETURNS TRIGGER AS $$
BEGIN
  IF OLD.state IN ('new', 'escalating', 'ongoing', 'regressed') AND NEW.state = 'resolved' THEN
    INSERT INTO apis.issue_activity_log (issue_id, event, created_by, created_at)
    SELECT i.id, CASE WHEN NEW.resolved_by IS NULL THEN 'auto_resolved' ELSE 'resolved' END,
      NEW.resolved_by, CASE WHEN NEW.resolved_by IS NULL THEN app_now() ELSE COALESCE(NEW.resolved_at, app_now()) END
    FROM apis.issues i
    WHERE i.project_id = NEW.project_id AND i.target_hash = NEW.hash AND i.issue_type = 'runtime_exception'
      AND (NEW.resolved_by IS NOT NULL OR (i.acknowledged_at IS NULL AND i.archived_at IS NULL))
    ORDER BY i.created_at DESC, i.id DESC LIMIT 1
    ON CONFLICT DO NOTHING;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
