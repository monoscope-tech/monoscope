BEGIN;

ALTER TABLE apis.endpoints
 ADD COLUMN IF NOT EXISTS first_trace_id TEXT,
 ADD COLUMN IF NOT EXISTS recent_trace_id TEXT,
 ADD COLUMN IF NOT EXISTS service TEXT;

ALTER TABLE apis.shapes
 ADD COLUMN IF NOT EXISTS first_trace_id TEXT,
 ADD COLUMN IF NOT EXISTS recent_trace_id TEXT,
 ADD COLUMN IF NOT EXISTS service TEXT;

CREATE OR REPLACE FUNCTION apis.update_occurance()
RETURNS trigger AS $$
DECLARE
  target_hash TEXT;
BEGIN
  target_hash := NEW.hash;
  UPDATE apis.issues
  SET
    affected_requests = affected_requests + 1,
    updated_at = NOW()
  WHERE endpoint_hash = hash;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_update_issue_from_endpoints AFTER UPDATE ON apis.endpoints FOR EACH ROW EXECUTE FUNCTION apis.update_occurance();
CREATE TRIGGER trg_update_issue_from_shapes AFTER UPDATE ON apis.shapes FOR EACH ROW EXECUTE FUNCTION apis.update_occurance();
CREATE TRIGGER trg_update_issue_from_format AFTER UPDATE ON apis.format FOR EACH ROW EXECUTE FUNCTION apis.update_occurance();
CREATE TRIGGER trg_update_issue_from_fields AFTER UPDATE ON apis.fields FOR EACH ROW EXECUTE FUNCTION apis.update_occurance();
CREATE TRIGGER trg_update_issue_from_errors AFTER UPDATE ON apis.errors FOR EACH ROW EXECUTE FUNCTION apis.update_occurance();

COMMIT;