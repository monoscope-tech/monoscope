CREATE TABLE IF NOT EXISTS apis.issue_activity_log (
  id         BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  issue_id   UUID NOT NULL REFERENCES apis.issues(id) ON DELETE CASCADE,
  event      TEXT NOT NULL,
  created_by UUID,
  metadata   JSONB,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS idx_issue_activity_log_issue ON apis.issue_activity_log(issue_id);

-- Auto-resolve trigger: logs activity when error patterns auto-resolve via batch updateOccurrenceCounts
CREATE OR REPLACE FUNCTION apis.log_auto_resolve_activity() RETURNS TRIGGER AS $$
BEGIN
  IF OLD.state IN ('new', 'escalating', 'ongoing', 'regressed')
     AND NEW.state = 'resolved'
  THEN
    INSERT INTO apis.issue_activity_log (issue_id, event, created_at)
    SELECT i.id, 'auto_resolved', now()
    FROM apis.issues i
    WHERE i.project_id = NEW.project_id AND i.target_hash = NEW.hash AND i.issue_type = 'runtime_exception'
    ORDER BY i.created_at DESC LIMIT 1;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_error_pattern_auto_resolve ON apis.error_patterns;
CREATE TRIGGER trg_error_pattern_auto_resolve
  AFTER UPDATE OF state ON apis.error_patterns
  FOR EACH ROW
  WHEN (OLD.state IS DISTINCT FROM NEW.state AND NEW.state = 'resolved')
  EXECUTE FUNCTION apis.log_auto_resolve_activity();
