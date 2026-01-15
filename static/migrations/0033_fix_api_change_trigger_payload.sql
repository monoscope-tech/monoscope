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
  VALUES (now(), 'queued', jsonb_build_object('tag', job_tag, 'contents', jsonb_build_array(NEW.project_id, NEW.hash)));
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

COMMIT;
