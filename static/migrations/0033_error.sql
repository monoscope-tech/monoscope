-- ALTER TYPE ADD VALUE must run outside a transaction (PG 12+ allows IF NOT EXISTS in txn).
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'error_escalating';
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'error_regressed';

BEGIN;

-- Drop the old apis.errors table, replaced by apis.error_patterns with fingerprinting and baselines
DROP TRIGGER IF EXISTS error_created_anomaly ON apis.errors;
DROP TABLE IF EXISTS apis.errors;

CREATE OR REPLACE FUNCTION apis.new_error_proc() RETURNS trigger AS $$
BEGIN
  IF TG_WHEN <> 'AFTER' THEN
    RAISE EXCEPTION 'apis.new_error_proc() may only run as an AFTER trigger';
  END IF;
  -- Create a job for the new error
  -- JSON format matches Aeson's derived FromJSON for:
  -- NewErrorDetected Projects.ProjectId Text
  INSERT INTO background_jobs (run_at, status, payload)
  VALUES (
    NOW(),
    'queued',
    jsonb_build_object(
      'tag', 'NewErrorDetected',
      'contents', jsonb_build_array(NEW.project_id, NEW.hash)
    )
  );
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TABLE apis.error_patterns (
    id                        UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    project_id                UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
    created_at                TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at                TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    error_type                TEXT NOT NULL,
    message                   TEXT NOT NULL,
    stacktrace                TEXT NOT NULL,
    hash                      TEXT NOT NULL,

    environment               TEXT,
    service                   TEXT,
    runtime                   TEXT,
    error_data                JSONB NOT NULL DEFAULT '{}',
    first_event_id            UUID,
    last_event_id             UUID,
    state                     TEXT NOT NULL DEFAULT 'new',
    assignee_id               UUID,
    assigned_at               TIMESTAMPTZ,
    resolved_at               TIMESTAMPTZ,
    regressed_at              TIMESTAMPTZ,

    occurrences_1m            INT NOT NULL DEFAULT 0,
    occurrences_5m            INT NOT NULL DEFAULT 0,
    occurrences_1h            INT NOT NULL DEFAULT 0,
    occurrences_24h           INT NOT NULL DEFAULT 0,

    quiet_minutes             INT NOT NULL DEFAULT 0,
    resolution_threshold_minutes INT NOT NULL DEFAULT 30,

    baseline_state TEXT NOT NULL DEFAULT 'learning',
    baseline_samples INT NOT NULL DEFAULT 0,
    baseline_error_rate_mean FLOAT,
    baseline_error_rate_stddev FLOAT,
    baseline_updated_at TIMESTAMPTZ,

    is_ignored                BOOLEAN DEFAULT false,
    ignored_until             TIMESTAMPTZ,

    subscribed                BOOLEAN NOT NULL DEFAULT FALSE,
    notify_every_minutes      INT NOT NULL DEFAULT 30,
    last_notified_at          TIMESTAMPTZ,
    slack_thread_ts           TEXT,
    discord_message_id        TEXT,
    first_trace_id            TEXT,
    recent_trace_id           TEXT
);
SELECT manage_updated_at('apis.error_patterns');

CREATE UNIQUE INDEX idx_apis_error_patterns_project_id_hash ON apis.error_patterns(project_id, hash);
CREATE INDEX idx_error_patterns_project_state ON apis.error_patterns (project_id, state);
CREATE INDEX idx_error_patterns_last_seen ON apis.error_patterns (project_id, last_event_id);
CREATE INDEX idx_error_patterns_active ON apis.error_patterns(project_id, state) WHERE state != 'resolved';
CREATE TRIGGER error_created_anomaly AFTER INSERT ON apis.error_patterns FOR EACH ROW EXECUTE PROCEDURE apis.new_error_proc('runtime_exception', 'created', 'skip_anomaly_record');

-- Hourly rollup for error occurrence stats.
-- Individual error occurrences are queryable from otel_logs_and_spans via hashes @> ARRAY['err:<hash>'].
CREATE TABLE apis.error_hourly_stats (
  project_id   UUID NOT NULL,
  error_id     UUID NOT NULL REFERENCES apis.error_patterns(id) ON DELETE CASCADE,
  hour_bucket  TIMESTAMPTZ NOT NULL,
  event_count  INT NOT NULL DEFAULT 0,
  user_count   INT NOT NULL DEFAULT 0,
  PRIMARY KEY (project_id, error_id, hour_bucket)
);

CREATE INDEX idx_error_hourly_project_time
  ON apis.error_hourly_stats (project_id, hour_bucket DESC);

COMMIT;
