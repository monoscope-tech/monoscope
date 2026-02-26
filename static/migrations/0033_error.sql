-- ALTER TYPE ADD VALUE must run outside a transaction (PG 12+ allows IF NOT EXISTS in txn).
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'error_escalating';
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'error_regressed';

BEGIN;

-- Drop existing trigger and table
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

CREATE TABLE apis.errors (
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
SELECT manage_updated_at('apis.errors');

CREATE UNIQUE INDEX idx_apis_errors_project_id_hash ON apis.errors(project_id, hash);
CREATE INDEX idx_errors_project_state ON apis.errors (project_id, state);
CREATE INDEX idx_errors_last_seen ON apis.errors (project_id, last_event_id);
CREATE INDEX idx_errors_active ON apis.errors(project_id, state) WHERE state != 'resolved';
CREATE TRIGGER error_created_anomaly AFTER INSERT ON apis.errors FOR EACH ROW EXECUTE PROCEDURE apis.new_error_proc('runtime_exception', 'created', 'skip_anomaly_record');

-- all data here that is not in errors (top level) go into error_data jsonb
-- Whenever the same error occurs, we update the error_data with latest values for these fields
-- Then an on update trigger on errors can update these fields from error_data if needed

CREATE TABLE apis.error_events (
    id                 UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    project_id         UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
    error_id           UUID NOT NULL REFERENCES apis.errors(id) ON DELETE CASCADE,
    occurred_at        TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    target_hash        TEXT NOT NULL,  -- references apis.errors.hash (no FK due to composite unique)
    exception_type     TEXT NOT NULL,   
    message            TEXT NOT NULL,
    stack_trace        TEXT NOT NULL,   
    service_name       TEXT NOT NULL, 
    release            TEXT,
    environment        TEXT,        
    request_method     TEXT,
    request_path       TEXT,
    endpoint_hash      TEXT,
    trace_id           TEXT,
    span_id            TEXT,
    parent_span_id     TEXT,
    user_id            TEXT,
    user_email         TEXT,
    user_ip            INET,
    session_id         TEXT,
    sample_rate        FLOAT NOT NULL DEFAULT 1.0
);

-- Indexes for efficient queries
CREATE INDEX idx_error_events_project ON apis.error_events (project_id, occurred_at DESC);
CREATE INDEX idx_error_event_error_hash ON apis.error_events (project_id, target_hash);
CREATE INDEX idx_error_events_service ON apis.error_events (service_name);
CREATE INDEX idx_error_events_error_id ON apis.error_events (error_id, occurred_at);

-- Trigger function to create error_events from error_data JSONB on errors table
-- The error_data column contains ATError structure with event-specific details
CREATE OR REPLACE FUNCTION apis.create_error_event_proc() RETURNS trigger AS $$
DECLARE
  v_user_ip INET;
BEGIN
  IF TG_WHEN <> 'AFTER' THEN
    RAISE EXCEPTION 'apis.create_error_event_proc() may only run as an AFTER trigger';
  END IF;

  IF NEW.error_data IS NOT NULL AND NEW.error_data != '{}'::jsonb THEN
    -- Safely attempt INET cast; invalid values become NULL
    BEGIN
      IF NEW.error_data->>'user_ip' IS NOT NULL AND NEW.error_data->>'user_ip' != '' THEN
        v_user_ip := (NEW.error_data->>'user_ip')::inet;
      END IF;
    EXCEPTION WHEN OTHERS THEN
      v_user_ip := NULL;
    END;

    INSERT INTO apis.error_events (
      project_id,
      error_id,
      occurred_at,
      target_hash,
      exception_type,
      message,
      stack_trace,
      service_name,
      environment,
      request_method,
      request_path,
      endpoint_hash,
      trace_id,
      span_id,
      parent_span_id,
      user_id,
      user_email,
      user_ip,
      session_id,
      sample_rate
    ) VALUES (
      NEW.project_id,
      NEW.id,
      COALESCE((NEW.error_data->>'when')::timestamptz, NOW()),
      NEW.hash,
      COALESCE(NEW.error_data->>'root_exception_type', NEW.error_data->>'error_type', NEW.error_type),
      COALESCE(NEW.error_data->>'root_exception_message', NEW.error_data->>'message', NEW.message),
      COALESCE(NEW.error_data->>'stack_trace', NEW.stacktrace),
      COALESCE(NEW.error_data->>'service_name', NEW.service, 'unknown'),
      COALESCE(NEW.error_data->>'environment', NEW.environment),
      NEW.error_data->>'request_method',
      NEW.error_data->>'request_path',
      NEW.error_data->>'endpoint_hash',
      NEW.error_data->>'trace_id',
      NEW.error_data->>'span_id',
      NEW.error_data->>'parent_span_id',
      NEW.error_data->>'user_id',
      NEW.error_data->>'user_email',
      v_user_ip,
      NEW.error_data->>'session_id',
      1.0 -- default sample rate
    );
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER error_insert_create_event AFTER INSERT ON apis.errors FOR EACH ROW EXECUTE PROCEDURE apis.create_error_event_proc();
CREATE TRIGGER error_update_create_event AFTER UPDATE OF error_data ON apis.errors FOR EACH ROW EXECUTE PROCEDURE apis.create_error_event_proc();

COMMIT;
