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

    exception_type            TEXT NOT NULL,
    message                   TEXT NOT NULL,               
    stacktrace                TEXT NOT NULL,              
    hash                      TEXT NOT NULL,               

    environment               TEXT NOT NULL,              
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
    ignored_until             TIMESTAMPTZ
);
SELECT manage_updated_at('apis.errors');

CREATE UNIQUE INDEX uniq_error_group ON apis.errors (project_id, hash, environment);
CREATE INDEX idx_errors_project_state ON apis.errors (project_id, state);
CREATE INDEX idx_errors_last_seen ON apis.errors (project_id, last_event_id);
CREATE UNIQUE INDEX idx_apis_errors_project_id_hash ON apis.errors(project_id, hash);
CREATE INDEX idx_apis_errors_project_id ON apis.errors(project_id);
CREATE INDEX idx_errors_active ON apis.errors(project_id, state) WHERE state != 'resolved';
CREATE INDEX idx_errors_state ON apis.errors(project_id, state);
CREATE TRIGGER error_created_anomaly AFTER INSERT ON apis.errors FOR EACH ROW EXECUTE PROCEDURE apis.new_error_proc('runtime_exception', 'created', 'skip_anomaly_record');

-- all data here that is not in errors (top level) go into error_data jsonb
-- Whenever the same error occurs, we update the error_data with latest values for these fields
-- Then an on update trigger on errors can update these fields from error_data if needed

CREATE TABLE apis.error_events (
    id                 UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    project_id         UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
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

ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'error_escalating';
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'error_regressed';


-- Trigger function to create error_events from error_data JSONB on errors table
-- The error_data column contains ATError structure with event-specific details
CREATE OR REPLACE FUNCTION apis.create_error_event_proc() RETURNS trigger AS $$
BEGIN
  IF TG_WHEN <> 'AFTER' THEN
    RAISE EXCEPTION 'apis.create_error_event_proc() may only run as an AFTER trigger';
  END IF;

  IF NEW.error_data IS NOT NULL AND NEW.error_data != '{}'::jsonb THEN
    INSERT INTO apis.error_events (
      project_id,
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
      -- Handle user_ip carefully - convert to INET if valid, otherwise NULL
      CASE
        WHEN NEW.error_data->>'user_ip' IS NOT NULL
             AND NEW.error_data->>'user_ip' != ''
        THEN (NEW.error_data->>'user_ip')::inet
        ELSE NULL
      END,
      NEW.error_data->>'session_id',
      1.0 -- default sample rate
    );
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER error_insert_create_event AFTER INSERT ON apis.errors FOR EACH ROW EXECUTE PROCEDURE apis.create_error_event_proc();
CREATE TRIGGER error_update_create_event AFTER UPDATE OF error_data ON apis.errors FOR EACH ROW EXECUTE PROCEDURE apis.create_error_event_proc();

ALTER TABLE apis.errors RENAME COLUMN exception_type TO error_type;
ALTER TABLE apis.errors
  ADD COLUMN first_trace_id TEXT,
  ADD COLUMN recent_trace_id TEXT;
COMMIT;