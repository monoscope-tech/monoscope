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

ALTER TABLE telemetry.spans ADD COLUMN IF NOT EXISTS duration_ns BIGINT NOT NULL DEFAULT 0;


ALTER TABLE telemetry.spans
-- HTTP span fields
ADD COLUMN IF NOT EXISTS http_status_code INTEGER,
ADD COLUMN IF NOT EXISTS http_method TEXT,
ADD COLUMN IF NOT EXISTS http_url TEXT,
ADD COLUMN IF NOT EXISTS http_path TEXT,
ADD COLUMN IF NOT EXISTS http_host TEXT,

-- Database span fields
ADD COLUMN IF NOT EXISTS db_system TEXT,
ADD COLUMN IF NOT EXISTS db_name TEXT,
ADD COLUMN IF NOT EXISTS db_statement TEXT,
ADD COLUMN IF NOT EXISTS db_operation TEXT,

-- Service/component identification
ADD COLUMN IF NOT EXISTS service_name TEXT,

-- Error details
ADD COLUMN IF NOT EXISTS error_type TEXT,
ADD COLUMN IF NOT EXISTS error_message TEXT,
ADD COLUMN IF NOT EXISTS error_stack TEXT,

-- Additional context
ADD COLUMN IF NOT EXISTS user_id TEXT,
ADD COLUMN IF NOT EXISTS session_id TEXT,
ADD COLUMN IF NOT EXISTS transaction_id TEXT;

-- Create indexes for common query patterns
CREATE INDEX IF NOT EXISTS idx_spans_http_status_code ON telemetry.spans(project_id, http_status_code);
CREATE INDEX IF NOT EXISTS idx_spans_http_method ON telemetry.spans(project_id, http_method);
CREATE INDEX IF NOT EXISTS idx_spans_http_path ON telemetry.spans(project_id, http_path);
CREATE INDEX IF NOT EXISTS idx_spans_service_name ON telemetry.spans(project_id, service_name);
CREATE INDEX IF NOT EXISTS idx_spans_db_system ON telemetry.spans(project_id, db_system);
CREATE INDEX IF NOT EXISTS idx_spans_duration_ms ON telemetry.spans(project_id, duration_ms);
CREATE INDEX IF NOT EXISTS idx_spans_error_type ON telemetry.spans(project_id, error_type);

-- Indexes for efficient querying
CREATE INDEX idx_traces_trace_id ON telemetry.spans(project_id, trace_id, timestamp DESC);
CREATE INDEX idx_traces_parent_span_id ON telemetry.spans(project_id, parent_span_id, timestamp DESC);
CREATE INDEX idx_traces_span_name ON telemetry.spans(project_id, span_name, timestamp DESC);
CREATE INDEX idx_traces_status ON telemetry.spans(project_id, status, timestamp DESC);
CREATE INDEX idx_traces_kind ON telemetry.spans(project_id, kind, timestamp DESC);
CREATE INDEX idx_traces_resource_service_name ON telemetry.spans (project_id, (resource->>'service.name'), timestamp DESC);



CREATE TABLE IF NOT EXISTS telemetry.metrics (
    id UUID NOT NULL DEFAULT gen_random_uuid(),
    project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
    timestamp TIMESTAMPTZ NOT NULL,
    metric_time TIMESTAMPTZ NOT NULL,
    metric_name TEXT NOT NULL,
    metric_type TEXT NOT NULL,
    metric_unit TEXT NOT NULL,
    metric_description TEXT NOT NULL,
    attributes JSONB,
    resource JSONB,
    instrumentation_scope JSONB,
    metric_value JSONB,
    exemplars  JSONB,
    flags INT,
    metricMetadata JSONB,
    PRIMARY KEY(project_id, timestamp, id)
);

SELECT create_hypertable('telemetry.metrics', by_range('timestamp', INTERVAL '1 hours'), migrate_data => true);

SELECT add_retention_policy('telemetry.metrics', INTERVAL '30 days', true);

CREATE INDEX idx_metrics_project_id_metric_name ON telemetry.metrics (project_id, metric_name, timestamp DESC);
CREATE INDEX idx_metrics_project_id_resource_service_name ON telemetry.metrics (project_id, (resource->>'service.name'), timestamp DESC);


CREATE TABLE IF NOT EXISTS telemetry.metrics_meta (
      id UUID NOT NULL DEFAULT gen_random_uuid(),
      project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
      created_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
      updated_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
      metric_name TEXT NOT NULL,
      metric_type TEXT NOT NULL,
      metric_unit TEXT NOT NULL,
      metric_description TEXT NOT NULL,
      service_name TEXT NOT NULL,
      UNIQUE (project_id, metric_name, service_name)
);
CREATE INDEX idx_metrics_meta_project_id_metric_name ON telemetry.metrics_meta (project_id, metric_name, service_name);
SELECT manage_updated_at('telemetry.metrics_meta');

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
CREATE INDEX idx_user_project_type_created ON projects.query_library (user_id, project_id, query_type, created_at DESC);
CREATE INDEX idx_project_user_type_created ON projects.query_library (project_id, user_id, query_type, created_at DESC);

-- ===================================================================
-- Custom Dashboards
-- ===================================================================

CREATE TABLE IF NOT EXISTS projects.dashboards (
  id      UUID NOT NULL DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
  created_at TIMESTAMP WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
  created_by    UUID NOT NULL REFERENCES users.users (id) ON DELETE CASCADE,
  base_template TEXT,
  schema JSONB,
  starred_since TIMESTAMP WITH TIME ZONE,
  homepage_since TIMESTAMP WITH TIME ZONE,
  tags TEXT[] NOT NULL DEFAULT '{}',
  title TEXT NOT NULL DEFAULT 'Untitled',
  PRIMARY KEY (id)
);
SELECT manage_updated_at('projects.dashboards');





CREATE TABLE IF NOT EXISTS otel_logs_and_spans (
    id                      UUID NOT NULL DEFAULT gen_random_uuid(),
    project_id               Text NOT NULL,
    timestamp                TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
    parent_id                TEXT,
    observed_timestamp       TIMESTAMPTZ,
    hashes                   TEXT[],
    name                     TEXT,
    kind                     TEXT,
    status_code              TEXT,
    status_message           TEXT,
    level                    TEXT,
    severity                 JSONB,
    severity___severity_text TEXT,
    severity___severity_number INTEGER,
    body                     JSONB,
    duration                 BIGINT,
    start_time               TIMESTAMPTZ,
    end_time                 TIMESTAMPTZ,
    context                  JSONB,
    context___trace_id       TEXT,
    context___span_id        TEXT,
    context___trace_state    TEXT,
    context___trace_flags    TEXT,
    context___is_remote      BOOLEAN,
    events                   JSONB,
    links                    JSONB,
    attributes               JSONB,
    attributes___client___address         TEXT,
    attributes___client___port            INTEGER,
    attributes___server___address         TEXT,
    attributes___server___port            INTEGER,
    attributes___network___local__address TEXT,
    attributes___network___local__port    INTEGER,
    attributes___network___peer___address TEXT,
    attributes___network___peer__port     INTEGER,
    attributes___network___protocol___name TEXT,
    attributes___network___protocol___version TEXT,
    attributes___network___transport       TEXT,
    attributes___network___type            TEXT,
    attributes___code___number             INTEGER,
    attributes___code___file___path        TEXT,
    attributes___code___function___name    TEXT,
    attributes___code___line___number      INTEGER,
    attributes___code___stacktrace         TEXT,
    attributes___log__record___original    TEXT,
    attributes___log__record___uid         TEXT,
    attributes___error___type              TEXT,
    attributes___exception___type          TEXT,
    attributes___exception___message       TEXT,
    attributes___exception___stacktrace    TEXT,
    attributes___url___fragment            TEXT,
    attributes___url___full                TEXT,
    attributes___url___path                TEXT,
    attributes___url___query               TEXT,
    attributes___url___scheme              TEXT,
    attributes___user_agent___original     TEXT,
    attributes___http___request___method   TEXT,
    attributes___http___request___method_original TEXT,
    attributes___http___response___status_code INTEGER,
    attributes___http___request___resend_count   INTEGER,
    attributes___http___request___body___size    BIGINT,
    attributes___session___id              TEXT,
    attributes___session___previous___id   TEXT,
    attributes___db___system___name        TEXT,
    attributes___db___collection___name    TEXT,
    attributes___db___namespace            TEXT,
    attributes___db___operation___name     TEXT,
    attributes___db___response___status_code INTEGER,
    attributes___db___operation___batch___size INTEGER,
    attributes___db___query___summary      TEXT,
    attributes___db___query___text         TEXT,
    attributes___user___id                 TEXT,
    attributes___user___email              TEXT,
    attributes___user___full_name          TEXT,
    attributes___user___name               TEXT,
    attributes___user___hash               TEXT,
    resource                               JSONB,
    resource___service___name              TEXT,
    resource___service___version           TEXT,
    resource___service___instance___id     TEXT,
    resource___service___namespace         TEXT,
    resource___telemetry___sdk___language  TEXT,
    resource___telemetry___sdk___name      TEXT,
    resource___telemetry___sdk___version   TEXT,
    resource___user_agent___original       TEXT
);
SELECT create_hypertable('otel_logs_and_spans', by_range('timestamp', INTERVAL '1 hours'), migrate_data => true);
SELECT add_retention_policy('otel_logs_and_spans',INTERVAL '3 days',true);

CREATE INDEX idx_logs_and_spans_trace_id ON otel_logs_and_spans (project_id, context___trace_id);
CREATE INDEX idx_logs_and_spans_span_id ON otel_logs_and_spans (project_id, context___span_id);
CREATE INDEX idx_logs_and_spans_parent_id ON otel_logs_and_spans (project_id, parent_id);
CREATE INDEX idx_logs_and_spans_service_name ON otel_logs_and_spans (project_id, resource___service___name);

