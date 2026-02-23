-- Include source_field in the trigger payload so the background job can do
-- an exact lookup on the (project_id, source_field, pattern_hash) unique index.
CREATE OR REPLACE FUNCTION apis.new_log_pattern_proc() RETURNS trigger AS $$
BEGIN
  IF TG_WHEN <> 'AFTER' THEN
    RAISE EXCEPTION 'apis.new_log_pattern_proc() may only run as an AFTER trigger';
  END IF;
  INSERT INTO background_jobs (run_at, status, payload)
  VALUES (
    NOW(),
    'queued',
    jsonb_build_object(
      'tag', 'NewLogPatternDetected',
      'contents', jsonb_build_array(NEW.project_id, NEW.source_field, NEW.pattern_hash)
    )
  );
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Partial index for lookups by (project_id, pattern_hash) without source_field
CREATE INDEX IF NOT EXISTS idx_log_patterns_project_hash
  ON apis.log_patterns(project_id, pattern_hash);
