BEGIN;

-- Drop the old deferrable constraint
ALTER TABLE apis.issues
DROP CONSTRAINT unique_open_api_change_per_endpoint;

-- Add the new, non-deferrable constraint
ALTER TABLE apis.issues
ADD CONSTRAINT unique_open_api_change_per_endpoint
UNIQUE (project_id, issue_type, endpoint_hash);


CREATE OR REPLACE FUNCTION apis.new_anomaly_proc() RETURNS trigger AS $$
DECLARE
  anomaly_type apis.anomaly_type;
  anomaly_action apis.anomaly_action;
  should_record_anomaly BOOLEAN := true;
  existing_job_id INT;
  existing_target_hashes JSONB;
BEGIN
  IF TG_WHEN <> 'AFTER' THEN
    RAISE EXCEPTION 'apis.new_anomaly_proc() may only run as an AFTER trigger';
  END IF;

  anomaly_type := TG_ARGV[0];
  anomaly_action := TG_ARGV[1];

  IF array_length(TG_ARGV, 1) >= 3 AND TG_ARGV[2] = 'skip_anomaly_record' THEN
    should_record_anomaly := false;
  END IF;

  IF should_record_anomaly THEN
    INSERT INTO apis.anomalies (
      project_id, anomaly_type, action, target_hash
    ) VALUES (
      NEW.project_id, anomaly_type, anomaly_action, NEW.hash
    ) ON CONFLICT (project_id, target_hash) DO NOTHING;
  END IF;

  -- Look for existing job
  SELECT id, payload->'targetHashes'
  INTO existing_job_id, existing_target_hashes
  FROM background_jobs
  WHERE payload->>'tag' = 'NewAnomaly'
    AND payload->>'projectId' = NEW.project_id::TEXT
    AND payload->>'anomalyType' = anomaly_type::TEXT
    AND status = 'queued'
  ORDER BY run_at ASC
  LIMIT 1;

  IF existing_job_id IS NOT NULL THEN
    UPDATE background_jobs SET payload = jsonb_build_object(
      'tag', 'NewAnomaly',
      'projectId', NEW.project_id,
      'createdAt', to_jsonb(NEW.created_at),
      'anomalyType', anomaly_type::TEXT,
      'anomalyAction', anomaly_action::TEXT,
      'targetHashes', existing_target_hashes || to_jsonb(NEW.hash)
    ) WHERE id = existing_job_id;
  ELSE
    INSERT INTO background_jobs (run_at, status, payload)
    VALUES (
      now(),
      'queued',
      jsonb_build_object(
        'tag', 'NewAnomaly',
        'projectId', NEW.project_id,
        'createdAt', to_jsonb(NEW.created_at),
        'anomalyType', anomaly_type::TEXT,
        'anomalyAction', anomaly_action::TEXT,
        'targetHashes', jsonb_build_array(NEW.hash)
      )
    );
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;


-- Remove old policy
SELECT remove_retention_policy('otel_logs_and_spans');

-- Add new 30-day policy
SELECT add_retention_policy('otel_logs_and_spans', INTERVAL '30 days', true);

COMMIT;