
BEGIN;

CREATE OR REPLACE FUNCTION apis.api_change_detected_proc() RETURNS trigger AS $$
DECLARE
  job_tag TEXT;
BEGIN
  IF TG_WHEN <> 'AFTER' THEN
    RAISE EXCEPTION 'apis.api_change_detected_proc() may only run as an AFTER trigger';
  END IF;
  job_tag := TG_ARGV[0];
  INSERT INTO background_jobs (run_at, status, payload)
  VALUES (now(),'queued',jsonb_build_object('tag', job_tag,'projectId', NEW.project_id,'hash', NEW.hash));
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS fields_created_anomaly ON apis.fields;
DROP TRIGGER IF EXISTS endpoint_created_anomaly ON apis.endpoints;
DROP TRIGGER IF EXISTS shapes_created_anomaly ON apis.shapes;

CREATE TRIGGER endpoint_created_new AFTER INSERT ON apis.endpoints FOR EACH ROW EXECUTE FUNCTION apis.api_change_detected_proc('NewEndpoint');
CREATE TRIGGER shape_created_new AFTER INSERT ON apis.shapes FOR EACH ROW EXECUTE FUNCTION apis.api_change_detected_proc('NewShape');
CREATE TRIGGER field_created_new AFTER INSERT ON apis.fields FOR EACH ROW EXECUTE FUNCTION apis.api_change_detected_proc('NewFieldChange');

COMMIT;
