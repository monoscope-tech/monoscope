-- A. Extract metric_value JSONB into native columns for faster aggregations
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS value DOUBLE PRECISION;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS metric_count BIGINT;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS metric_sum DOUBLE PRECISION;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS bucket_counts JSONB;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS explicit_bounds JSONB;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS point_min DOUBLE PRECISION;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS point_max DOUBLE PRECISION;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS quantiles JSONB;

-- C. Store aggregation_temporality and is_monotonic for Sum metrics
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS aggregation_temporality SMALLINT;
ALTER TABLE telemetry.metrics ADD COLUMN IF NOT EXISTS is_monotonic BOOLEAN;

-- Helper: safe numeric cast that returns NULL instead of aborting on malformed data
CREATE OR REPLACE FUNCTION pg_temp.safe_float(text) RETURNS double precision AS $$
BEGIN RETURN $1::double precision; EXCEPTION WHEN OTHERS THEN RETURN NULL; END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION pg_temp.safe_bigint(text) RETURNS bigint AS $$
BEGIN RETURN $1::bigint; EXCEPTION WHEN OTHERS THEN RETURN NULL; END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Backfill native columns from existing metric_value JSONB
-- Gauge/Sum: extract value
UPDATE telemetry.metrics SET value = pg_temp.safe_float(metric_value->'contents'->>'value')
WHERE metric_type IN ('GAUGE', 'SUM') AND value IS NULL AND metric_value->'contents' IS NOT NULL;

-- Histogram: extract sum, count, min, max, bucket_counts, explicit_bounds
UPDATE telemetry.metrics SET
  metric_sum = pg_temp.safe_float(metric_value->'contents'->>'sum'),
  metric_count = pg_temp.safe_bigint(metric_value->'contents'->>'count'),
  point_min = pg_temp.safe_float(metric_value->'contents'->>'point_min'),
  point_max = pg_temp.safe_float(metric_value->'contents'->>'point_max'),
  bucket_counts = metric_value->'contents'->'bucket_counts',
  explicit_bounds = metric_value->'contents'->'explicit_bounds'
WHERE metric_type = 'HISTOGRAM' AND metric_sum IS NULL AND metric_value->'contents' IS NOT NULL;

-- Summary: extract sum, count, quantiles
UPDATE telemetry.metrics SET
  metric_sum = pg_temp.safe_float(metric_value->'contents'->>'sum'),
  metric_count = pg_temp.safe_bigint(metric_value->'contents'->>'count'),
  quantiles = metric_value->'contents'->'quantiles'
WHERE metric_type = 'SUMMARY' AND metric_sum IS NULL AND metric_value->'contents' IS NOT NULL;

-- ExponentialHistogram: extract sum, count, min, max
UPDATE telemetry.metrics SET
  metric_sum = pg_temp.safe_float(metric_value->'contents'->>'sum'),
  metric_count = pg_temp.safe_bigint(metric_value->'contents'->>'count'),
  point_min = pg_temp.safe_float(metric_value->'contents'->>'point_min'),
  point_max = pg_temp.safe_float(metric_value->'contents'->>'point_max')
WHERE metric_type = 'EXPONENTIALHISTOGRAM' AND metric_sum IS NULL AND metric_value->'contents' IS NOT NULL;

-- B. Fix compression: add segmentby for project_id and metric_name
DO $$
DECLARE
  chunk_name regclass;
  failed_chunks int := 0;
BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = 'telemetry' AND table_name = 'metrics') THEN
    -- Decompress all compressed chunks first (required to change compression settings)
    FOR chunk_name IN
      SELECT c.chunk_name::regclass FROM timescaledb_information.chunks c
      WHERE c.hypertable_schema = 'telemetry' AND c.hypertable_name = 'metrics' AND c.is_compressed = true
    LOOP
      BEGIN
        PERFORM decompress_chunk(chunk_name, true);
      EXCEPTION
        WHEN lock_not_available THEN
          RAISE NOTICE 'Chunk % is locked, skipping: %', chunk_name, SQLERRM;
          failed_chunks := failed_chunks + 1;
      END;
    END LOOP;

    IF failed_chunks > 0 THEN
      RAISE EXCEPTION '% chunks could not be decompressed due to locks. Retry the migration.', failed_chunks;
    END IF;

    -- Update compression settings with segmentby
    ALTER TABLE telemetry.metrics SET (
      timescaledb.compress_segmentby = 'project_id, metric_name',
      timescaledb.compress_orderby = 'timestamp DESC'
    );

    -- Recompress all chunks
    FOR chunk_name IN
      SELECT c.chunk_name::regclass FROM timescaledb_information.chunks c
      WHERE c.hypertable_schema = 'telemetry' AND c.hypertable_name = 'metrics' AND c.is_compressed = false
    LOOP
      PERFORM compress_chunk(chunk_name, true);
    END LOOP;
  END IF;
END $$;

-- D. GIN index on attributes for label-based filtering
CREATE INDEX IF NOT EXISTS idx_metrics_attributes_gin ON telemetry.metrics USING GIN (attributes jsonb_path_ops);

-- E. Continuous aggregates for common rollups (all metric types, using value/metric_sum)
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
GROUP BY project_id, metric_name, metric_type, time_bucket('1 hour', timestamp);

-- Refresh policy: refresh hourly, covering the last 2 hours
SELECT add_continuous_aggregate_policy('telemetry.metrics_hourly',
  start_offset => INTERVAL '2 hours',
  end_offset => INTERVAL '1 hour',
  schedule_interval => INTERVAL '1 hour',
  if_not_exists => true
);

-- Retention on continuous aggregate (keep 90 days of rollups vs 30 days raw)
SELECT add_retention_policy('telemetry.metrics_hourly', INTERVAL '90 days', if_not_exists => true);
