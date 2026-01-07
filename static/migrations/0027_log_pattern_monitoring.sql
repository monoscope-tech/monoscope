BEGIN;

CREATE TABLE IF NOT EXISTS apis.log_patterns (
    id                      BIGSERIAL PRIMARY KEY,
    project_id              UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
    created_at              TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at              TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    pattern                 TEXT NOT NULL,
    pattern_hash            TEXT NOT NULL,

    service_name            TEXT,
    log_level               TEXT,
    sample_message          TEXT,

    first_seen_at           TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_seen_at            TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    occurrence_count        BIGINT NOT NULL DEFAULT 1,

    state                   TEXT NOT NULL DEFAULT 'new',  -- 'new', 'acknowledged', 'ignored'
    acknowledged_by         UUID REFERENCES users.users(id),
    acknowledged_at         TIMESTAMPTZ,

    -- Baseline for volume spike detection
    baseline_state                TEXT NOT NULL DEFAULT 'learning',  -- 'learning', 'established'
    baseline_volume_hourly_mean   FLOAT,
    baseline_volume_hourly_stddev FLOAT,
    baseline_samples              INT NOT NULL DEFAULT 0,
    baseline_updated_at           TIMESTAMPTZ,

    UNIQUE(project_id, pattern_hash)
);

SELECT manage_updated_at('apis.log_patterns');

-- Indexes for efficient queries
CREATE INDEX IF NOT EXISTS idx_log_patterns_project ON apis.log_patterns(project_id);
CREATE INDEX IF NOT EXISTS idx_log_patterns_project_state ON apis.log_patterns(project_id, state);
CREATE INDEX IF NOT EXISTS idx_log_patterns_last_seen ON apis.log_patterns(project_id, last_seen_at DESC);
CREATE INDEX IF NOT EXISTS idx_log_patterns_service ON apis.log_patterns(project_id, service_name);


CREATE OR REPLACE FUNCTION apis.new_log_pattern_proc() RETURNS trigger AS $$
BEGIN
  IF TG_WHEN <> 'AFTER' THEN
    RAISE EXCEPTION 'apis.new_log_pattern_proc() may only run as an AFTER trigger';
  END IF;

  -- Create a job for the new pattern
  -- JSON format matches Aeson's derived FromJSON for:
  -- NewLogPatternDetected Projects.ProjectId Text
  INSERT INTO background_jobs (run_at, status, payload)
  VALUES (
    NOW(),
    'queued',
    jsonb_build_object(
      'tag', 'NewLogPatternDetected',
      'contents', jsonb_build_array(NEW.project_id, NEW.pattern_hash)
    )
  );

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER log_pattern_created_notify
  AFTER INSERT ON apis.log_patterns
  FOR EACH ROW
  EXECUTE PROCEDURE apis.new_log_pattern_proc();

ALTER TABLE projects.projects
ADD COLUMN IF NOT EXISTS log_pattern_alerts BOOLEAN NOT NULL DEFAULT false;


ALTER TABLE apis.endpoints
ADD COLUMN IF NOT EXISTS baseline_state TEXT NOT NULL DEFAULT 'learning',
ADD COLUMN IF NOT EXISTS baseline_samples INT NOT NULL DEFAULT 0,
ADD COLUMN IF NOT EXISTS baseline_updated_at TIMESTAMPTZ,
ADD COLUMN IF NOT EXISTS baseline_error_rate_mean FLOAT,
ADD COLUMN IF NOT EXISTS baseline_error_rate_stddev FLOAT,
ADD COLUMN IF NOT EXISTS baseline_latency_mean FLOAT,
ADD COLUMN IF NOT EXISTS baseline_latency_stddev FLOAT,
ADD COLUMN IF NOT EXISTS baseline_latency_p95 FLOAT,
ADD COLUMN IF NOT EXISTS baseline_latency_p99 FLOAT,
ADD COLUMN IF NOT EXISTS baseline_volume_hourly_mean FLOAT,
ADD COLUMN IF NOT EXISTS baseline_volume_hourly_stddev FLOAT;



COMMIT;
