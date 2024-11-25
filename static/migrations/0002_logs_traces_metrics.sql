CREATE SCHEMA IF NOT EXISTS telemetry;

-- Define the ENUM type for severity_text
CREATE TYPE telemetry.severity_level AS ENUM ('DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL');

CREATE TABLE IF NOT EXISTS telemetry.logs (
    project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
    id UUID NOT  NULL DEFAULT  gen_random_uuid(),
    timestamp TIMESTAMPTZ NOT NULL,
    observed_timestamp TIMESTAMPTZ NOT NULL,
    trace_id TEXT NOT NULL,
    span_id TEXT, -- Ensure trace_id is present if span_id is present
    severity_text telemetry.severity_level, -- Nullable ENUM for severity levels
    severity_number INT NOT NULL,
    body JSONB,
    body_tsvector TSVECTOR GENERATED ALWAYS AS (
      jsonb_to_tsvector('english', body, '["string", "numeric"]')
    ) STORED,
    attributes JSONB,
    resource JSONB,
    instrumentation_scope JSONB,
    PRIMARY KEY(project_id,timestamp,id)
);
SELECT create_hypertable('telemetry.logs', by_range('timestamp', INTERVAL '1 hours'), migrate_data => true);
SELECT add_retention_policy('telemetry.logs',INTERVAL '3 days',true);

-- Indexes for efficient querying
CREATE INDEX idx_telemetry_logs_project_id_trace_id ON telemetry.logs(project_id, trace_id, timestamp DESC);
CREATE INDEX idx_telemetry_logs_project_id_span_id ON telemetry.logs(project_id, span_id, timestamp DESC);
CREATE INDEX idx_telemetry_logs_project_id_severity_text ON telemetry.logs(project_id, severity_text, timestamp DESC);
CREATE INDEX idx_telemetry_logs_project_id_service_name ON telemetry.logs (project_id, (resource->>'service.name'), timestamp DESC);

-- =================================================================
-- TRACES
-- =================================================================
CREATE TYPE telemetry.span_status AS ENUM ('OK', 'ERROR', 'UNSET');

-- Define the ENUM type for span kind
CREATE TYPE telemetry.span_kind AS ENUM ('INTERNAL', 'SERVER', 'CLIENT', 'PRODUCER', 'CONSUMER');

CREATE TABLE IF NOT EXISTS telemetry.spans (
    project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
    timestamp TIMESTAMPTZ NOT NULL,
    trace_id TEXT NOT NULL, -- Unique identifier for the trace
    span_id TEXT, -- Unique identifier for the span
    parent_span_id TEXT, -- Identifier for the parent span
    trace_state TEXT, -- Trace state
    span_name TEXT NOT NULL, -- Name of the span
    start_time TIMESTAMPTZ NOT NULL, -- Start time of the span
    end_time TIMESTAMPTZ, -- End time of the span
    kind telemetry.span_kind, -- Kind of span (internal, server, client, etc.)
    status telemetry.span_status, -- Status of the span (OK, ERROR, UNSET)
    status_message TEXT, -- Message describing the status if it's an error
    attributes JSONB, -- Arbitrary attributes associated with the span
    events JSONB, -- Events associated with the span
    links JSONB, -- Links to other spans or traces
    resource JSONB, -- Resource information (e.g., service name, version)
    instrumentation_scope JSONB, -- Information about the instrumentation library
    PRIMARY KEY(project_id,timestamp,span_id)
);
SELECT create_hypertable('telemetry.spans', by_range('timestamp', INTERVAL '1 hours'), migrate_data => true);
SELECT add_retention_policy('telemetry.spans',INTERVAL '3 days',true);
ALTER TABLE telemetry.spans ADD COLUMN IF NOT EXISTS id UUID NOT NULL DEFAULT gen_random_uuid();

-- Indexes for efficient querying
CREATE INDEX idx_traces_trace_id ON telemetry.spans(project_id, trace_id, timestamp DESC);
CREATE INDEX idx_traces_parent_span_id ON telemetry.spans(project_id, parent_span_id, timestamp DESC);
CREATE INDEX idx_traces_span_name ON telemetry.spans(project_id, span_name, timestamp DESC);
CREATE INDEX idx_traces_status ON telemetry.spans(project_id, status, timestamp DESC);
CREATE INDEX idx_traces_kind ON telemetry.spans(project_id, kind, timestamp DESC);
CREATE INDEX idx_traces_resource_service_name ON telemetry.spans (project_id, (resource->>'service.name'), timestamp DESC);

-- =================================================================
-- Query history and saved queries 
-- =================================================================
CREATE TYPE projects.query_library_kind AS ENUM ('history', 'saved');
CREATE TABLE IF NOT EXISTS projects.query_library (
  id         UUID NOT NULL DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
  created_at TIMESTAMP WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,  
  user_id    UUID NOT NULL REFERENCES users.users (id) ON DELETE CASCADE,
  query_type projects.query_library_kind NOT NULL DEFAULT 'history',
  query_text TEXT NOT NULL DEFAULT '',
  query_ast JSONB NOT NULL,
  title      TEXT,
  PRIMARY KEY (id)
);
SELECT manage_updated_at('projects.query_library');
CREATE UNIQUE INDEX unique_user_query ON projects.query_library (user_id, query_type, query_text);
CREATE INDEX idx_user_project_type_created
ON projects.query_library (user_id, project_id, query_type, created_at DESC);
CREATE INDEX idx_project_user_type_created
ON projects.query_library (project_id, user_id, query_type, created_at DESC);
