BEGIN;

-- ============================================================================
-- 2. ENDPOINT BASELINES
-- ============================================================================
-- Per-endpoint, service and log pattern behavioral baselines for detecting spikes/degradations.
-- Dimensions: error_rate, latency, volume

CREATE TABLE IF NOT EXISTS apis.baselines (
    id                      BIGSERIAL PRIMARY KEY,
    project_id              UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
    subject_type            TEXT NOT NULL,  -- 'endpoint', 'service', 'log_pattern'
    subject_key             TEXT NOT NULL,  -- endpoint_hash or service_name or pattern_hash

    state                   TEXT NOT NULL DEFAULT 'learning',  -- 'learning', 'established'
    min_observations        INT DEFAULT 1000,  -- need this many data points to establish

    baseline_data           JSONB NOT NULL DEFAULT '{}',
    /*
      error_rate: { "mean": 0.02, "stddev": 0.008, "samples": 5000 }
      latency:    { "mean": 65, "stddev": 40, "p50": 45, "p95": 120, "p99": 250, "samples": 5000 }
      volume:     { "mean": 150, "stddev": 35, "samples": 1440 }
    */

    baseline_window_hours   INT DEFAULT 24,

    last_calculated_at      TIMESTAMPTZ,
    established_at          TIMESTAMPTZ,
    created_at              TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at              TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    UNIQUE(project_id, subject_key, dimension)
);

SELECT manage_updated_at('apis.baselines');

CREATE INDEX IF NOT EXISTS idx_baselines_lookup
ON apis.baselines(project_id, subject_key, subject_type);

CREATE INDEX IF NOT EXISTS idx_baselines_established
ON apis.baselines(project_id, state)
WHERE state = 'established';


-- ============================================================================
-- 3. ERRORS TABLE (RECREATED WITH LIFECYCLE)
-- ============================================================================
-- Recreate errors table with lifecycle tracking.
-- States: new, escalating, ongoing, resolved, regressed

-- Drop existing trigger and table
DROP TRIGGER IF EXISTS error_created_anomaly ON apis.errors;
DROP TABLE IF EXISTS apis.errors;




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
    ignored_until             TIMESTAMPTZ,
    PRIMARY KEY (id)
);
SELECT manage_updated_at('apis.errors');

CREATE UNIQUE INDEX uniq_error_group ON apis.errors (project_id, hash, environment);
CREATE INDEX idx_errors_project_state ON apis.errors (project_id, state);
CREATE INDEX idx_errors_last_seen ON apis.errors (project_id, last_event_id);
CREATE UNIQUE INDEX idx_apis_errors_project_id_hash ON apis.errors(project_id, hash);
CREATE INDEX idx_apis_errors_project_id ON apis.errors(project_id);
CREATE INDEX idx_errors_active ON apis.errors(project_id, state, last_seen_at DESC) WHERE state != 'resolved';
CREATE INDEX idx_errors_state ON apis.errors(project_id, state);
CREATE TRIGGER error_created_anomaly AFTER INSERT ON apis.errors FOR EACH ROW EXECUTE PROCEDURE apis.new_anomaly_proc('runtime_exception', 'created', 'skip_anomaly_record');

-- all data here that is not in errors (top level) go into error_data jsonb
-- Whenever the same error occurs, we update the error_data with latest values for these fields
-- Then an on update trigger on errors can update these fields from error_data if needed

CREATE TABLE apis.error_events (
    id                 UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    project_id         UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
    occurred_at        TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    target_hash        TEXT NOT NULL REFERENCES apis.errors(hash) ON DELETE CASCADE,  
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
    sample_rate        FLOAT NOT NULL DEFAULT 1.0,
);

-- Indexes for efficient queries
CREATE INDEX idx_error_events_project ON apis.error_events (project_id, occurred_at DESC);
CREATE INDEX idx_error_event_error_hash ON apis.error_events (project_id, target_hash);
CREATE INDEX idx_error_events_service ON apis.error_events (service_name);

COMMIT;
