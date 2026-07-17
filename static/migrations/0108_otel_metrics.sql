-- Raw OTLP metric data points. This schema deliberately mirrors the TimeFusion
-- table provisioned for production: raw metric writes may target TimescaleDB,
-- TimeFusion, or both; the relational catalog below remains TimescaleDB-only.
CREATE TABLE IF NOT EXISTS otel_metrics (
    project_id TEXT NOT NULL,
    timestamp TIMESTAMPTZ NOT NULL,
    start_timestamp TIMESTAMPTZ,
    ingested_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
    id UUID NOT NULL,
    series_id TEXT NOT NULL,

    metric_name TEXT NOT NULL,
    metric_description TEXT,
    metric_unit TEXT NOT NULL DEFAULT '',
    metric_type TEXT NOT NULL,
    aggregation_temporality TEXT,
    is_monotonic BOOLEAN,
    flags BIGINT NOT NULL DEFAULT 0,

    resource JSONB,
    resource_schema_url TEXT,
    scope_name TEXT,
    scope_version TEXT,
    scope_schema_url TEXT,
    attributes JSONB,
    dropped_attributes_count BIGINT NOT NULL DEFAULT 0,
    exemplars JSONB,

    resource___service___name TEXT,
    resource___service___namespace TEXT,
    resource___service___instance___id TEXT,
    resource___service___version TEXT,
    resource___deployment___environment___name TEXT,
    resource___host___name TEXT,
    resource___container___name TEXT,
    resource___k8s___cluster___name TEXT,
    resource___k8s___namespace___name TEXT,
    resource___k8s___pod___name TEXT,
    resource___k8s___container___name TEXT,
    resource___cloud___provider TEXT,
    resource___cloud___region TEXT,
    resource___cloud___availability___zone TEXT,

    attributes___http___request___method TEXT,
    attributes___http___route TEXT,
    attributes___http___response___status_code INTEGER,
    attributes___error___type TEXT,
    attributes___rpc___service TEXT,
    attributes___rpc___method TEXT,
    attributes___rpc___grpc___status_code INTEGER,
    attributes___db___system___name TEXT,
    attributes___db___operation___name TEXT,
    attributes___messaging___system TEXT,
    attributes___messaging___operation TEXT,
    attributes___messaging___destination___name TEXT,

    value DOUBLE PRECISION,
    value_double DOUBLE PRECISION,
    value_int BIGINT,

    distribution_count BIGINT,
    distribution_sum DOUBLE PRECISION,
    distribution_min DOUBLE PRECISION,
    distribution_max DOUBLE PRECISION,
    hist_bucket_counts BIGINT[],
    hist_explicit_bounds DOUBLE PRECISION[],
    exp_hist_scale INTEGER,
    exp_hist_zero_count BIGINT,
    exp_hist_zero_threshold DOUBLE PRECISION,
    exp_hist_pos_offset INTEGER,
    exp_hist_pos_buckets BIGINT[],
    exp_hist_neg_offset INTEGER,
    exp_hist_neg_buckets BIGINT[],
    summary_quantiles DOUBLE PRECISION[],
    summary_values DOUBLE PRECISION[],

    message_size_bytes BIGINT NOT NULL DEFAULT 0,
    PRIMARY KEY (project_id, timestamp, id)
);

SELECT create_hypertable('otel_metrics', by_range('timestamp', INTERVAL '1 hours'), migrate_data => true, if_not_exists => true);
SELECT add_retention_policy('otel_metrics', INTERVAL '30 days', if_not_exists => true);
CREATE INDEX IF NOT EXISTS idx_otel_metrics_project_metric_time ON otel_metrics (project_id, metric_name, timestamp DESC);
CREATE INDEX IF NOT EXISTS idx_otel_metrics_project_service_time ON otel_metrics (project_id, resource___service___name, timestamp DESC);
CREATE INDEX IF NOT EXISTS idx_otel_metrics_project_series_time ON otel_metrics (project_id, series_id, timestamp DESC);

-- A low-volume TimescaleDB-only catalog for metric discovery and Metrics-page
-- filters. It is buffered by the application; never create it in TimeFusion.
CREATE TABLE IF NOT EXISTS otel_metrics_meta (
    project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
    metric_name TEXT NOT NULL,
    metric_type TEXT NOT NULL,
    metric_unit TEXT NOT NULL DEFAULT '',
    metric_description TEXT,
    service_name TEXT NOT NULL DEFAULT 'unknown',
    scope_name TEXT NOT NULL DEFAULT '',
    scope_version TEXT,
    first_seen_at TIMESTAMPTZ NOT NULL,
    last_seen_at TIMESTAMPTZ NOT NULL,
    first_timestamp TIMESTAMPTZ NOT NULL,
    last_timestamp TIMESTAMPTZ NOT NULL,
    PRIMARY KEY (project_id, metric_name, metric_type, metric_unit, service_name, scope_name)
);
CREATE INDEX IF NOT EXISTS idx_otel_metrics_meta_project_last_seen ON otel_metrics_meta (project_id, last_seen_at DESC);
CREATE INDEX IF NOT EXISTS idx_otel_metrics_meta_project_service_name ON otel_metrics_meta (project_id, service_name, metric_name);
