-- A. Extract metric_value JSONB into native columns for faster aggregations
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS value DOUBLE PRECISION;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS metric_count BIGINT;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS metric_sum DOUBLE PRECISION;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS bucket_counts JSONB;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS explicit_bounds JSONB;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS point_min DOUBLE PRECISION;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS point_max DOUBLE PRECISION;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS quantiles JSONB;

-- B. Store aggregation_temporality and is_monotonic for Sum metrics
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS aggregation_temporality SMALLINT;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS is_monotonic BOOLEAN;

-- C. GIN index on attributes for label-based filtering
CREATE INDEX IF NOT EXISTS idx_metrics_attributes_gin ON telemetry.metrics USING GIN (attributes jsonb_path_ops);

-- D. Continuous aggregates for common rollups
-- Requires NoNewTransaction migration mode since CREATE MATERIALIZED VIEW cannot run in a transaction
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM timescaledb_information.continuous_aggregates WHERE view_name = 'metrics_hourly') THEN
    EXECUTE $cagg$
      CREATE MATERIALIZED VIEW telemetry.metrics_hourly
      WITH (timescaledb.continuous) AS
      SELECT project_id, metric_name, metric_type,
             time_bucket('1 hour', timestamp) AS bucket,
             avg(COALESCE(value, metric_sum)) AS avg_value,
             min(COALESCE(value, metric_sum)) AS min_value,
             max(COALESCE(value, metric_sum)) AS max_value,
             sum(COALESCE(value, metric_sum)) AS sum_value,
             count(*) AS sample_count,
             sum(metric_count) AS total_count
      FROM telemetry.metrics
      GROUP BY project_id, metric_name, metric_type, time_bucket('1 hour', timestamp)
      WITH NO DATA
    $cagg$;
  END IF;
END $$;

SELECT add_continuous_aggregate_policy('telemetry.metrics_hourly',
  start_offset => INTERVAL '3 hours',
  end_offset => INTERVAL '1 hour',
  schedule_interval => INTERVAL '1 hour',
  if_not_exists => true
);

SELECT add_retention_policy('telemetry.metrics_hourly', INTERVAL '90 days', if_not_exists => true);
