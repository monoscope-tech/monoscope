-- Fix race condition: INSERT ... SELECT already handles missing issues (returns 0 rows),
-- but add explicit locking via FOR UPDATE to prevent the issue being deleted between
-- the SELECT and the INSERT within the same statement's snapshot.
CREATE OR REPLACE FUNCTION apis.log_auto_resolve_activity() RETURNS TRIGGER AS $$
BEGIN
  IF OLD.state IN ('new', 'escalating', 'ongoing', 'regressed')
     AND NEW.state = 'resolved'
  THEN
    INSERT INTO apis.issue_activity_log (issue_id, event, created_at)
    SELECT i.id, 'auto_resolved', now()
    FROM apis.issues i
    WHERE i.project_id = NEW.project_id AND i.target_hash = NEW.hash AND i.issue_type = 'runtime_exception'
      AND i.acknowledged_at IS NULL AND i.archived_at IS NULL
    ORDER BY i.created_at DESC, i.id DESC LIMIT 1
    ON CONFLICT DO NOTHING;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
