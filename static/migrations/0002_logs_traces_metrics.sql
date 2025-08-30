CREATE SCHEMA IF NOT EXISTS telemetry;

-- Define the ENUM type for severity_text
CREATE TYPE telemetry.severity_level AS ENUM ('DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL');
-- =================================================================
-- TRACES
-- =================================================================
CREATE TYPE telemetry.span_status AS ENUM ('OK', 'ERROR', 'UNSET');

-- Define the ENUM type for span kind
CREATE TYPE telemetry.span_kind AS ENUM ('INTERNAL', 'SERVER', 'CLIENT', 'PRODUCER', 'CONSUMER');


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

SELECT create_hypertable('telemetry.metrics', by_range('timestamp', INTERVAL '1 hours'), migrate_data => true, if_not_exists => true);

SELECT add_retention_policy('telemetry.metrics', INTERVAL '30 days', true);

CREATE INDEX IF NOT EXISTS idx_metrics_project_id_metric_name ON telemetry.metrics (project_id, metric_name, timestamp DESC);
CREATE INDEX IF NOT EXISTS idx_metrics_project_id_resource_service_name ON telemetry.metrics (project_id, (resource->>'service.name'), timestamp DESC);


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
CREATE INDEX IF NOT EXISTS idx_metrics_meta_project_id_metric_name ON telemetry.metrics_meta (project_id, metric_name, service_name);
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
CREATE UNIQUE INDEX IF NOT EXISTS unique_user_query ON projects.query_library (user_id, query_type, query_text);
CREATE INDEX IF NOT EXISTS idx_user_project_type_created ON projects.query_library (user_id, project_id, query_type, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_project_user_type_created ON projects.query_library (project_id, user_id, query_type, created_at DESC);

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
    links                    Text,
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
    attributes___db___response___status_code TEXT,
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
    resource___user_agent___original       TEXT,
    summary                        TEXT[] NOT NULL,
    date  TIMESTAMPTZ NOT NULL DEFAULT current_timestamp
);
SELECT create_hypertable('otel_logs_and_spans', by_range('timestamp', INTERVAL '1 hours'), migrate_data => true, if_not_exists => true);
SELECT add_retention_policy('otel_logs_and_spans',INTERVAL '14 days',true);

CREATE INDEX IF NOT EXISTS idx_logs_and_spans_trace_id ON otel_logs_and_spans (project_id, context___trace_id);
CREATE INDEX IF NOT EXISTS idx_logs_and_spans_span_id ON otel_logs_and_spans (project_id, context___span_id);
CREATE INDEX IF NOT EXISTS idx_logs_and_spans_parent_id ON otel_logs_and_spans (project_id, parent_id);
CREATE INDEX IF NOT EXISTS idx_logs_and_spans_service_name ON otel_logs_and_spans (project_id, resource___service___name);
CREATE INDEX IF NOT EXISTS idx_logs_and_spans_name ON otel_logs_and_spans (project_id, name);
