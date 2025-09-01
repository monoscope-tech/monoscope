BEGIN;

DROP MATERIALIZED VIEW IF EXISTS apis.endpoint_request_stats;

CREATE MATERIALIZED VIEW IF NOT EXISTS apis.endpoint_request_stats AS
WITH request_stats AS (
    SELECT
        project_id,
        attributes___url___path AS url_path,
        attributes___http___request___method AS method,
        hashes[1] AS endpoint_hash,
        coalesce(attributes->'net'->'host'->>'name', '') AS host,
        percentile_agg(duration) AS agg,
        sum(duration) AS total_time,
        count(*) AS total_requests,
        sum(sum(duration)) OVER (PARTITION BY project_id) AS total_time_proj,
        sum(count(*)) OVER (PARTITION BY project_id) AS total_requests_proj
    FROM otel_logs_and_spans
    WHERE name = 'monoscope.http' OR name = 'apitoolkit-http-span'
    GROUP BY project_id, url_path, method, hashes[1], host
)
SELECT
    enp.id AS endpoint_id,
    enp.hash AS endpoint_hash,
    rds.project_id,
    rds.url_path,
    rds.method,
    rds.host,
    coalesce(approx_percentile(0,    agg) / 1e9, 0) AS min,
    coalesce(approx_percentile(0.50, agg) / 1e9, 0) AS p50,
    coalesce(approx_percentile(0.75, agg) / 1e9, 0) AS p75,
    coalesce(approx_percentile(0.90, agg) / 1e9, 0) AS p90,
    coalesce(approx_percentile(0.95, agg) / 1e9, 0) AS p95,
    coalesce(approx_percentile(0.99, agg) / 1e9, 0) AS p99,
    coalesce(approx_percentile(1,    agg) / 1e9, 0) AS max,
    CAST(total_time / 1e9 AS FLOAT8) AS total_time,
    CAST(total_time_proj / 1e9 AS FLOAT8) AS total_time_proj,
    CAST(total_requests AS INT),
    CAST(total_requests_proj AS INT)
FROM apis.endpoints enp
JOIN request_stats rds
  ON rds.project_id = enp.project_id::text
 AND enp.hash = rds.endpoint_hash;

CREATE INDEX IF NOT EXISTS idx_apis_endpoint_request_stats_project_id ON apis.endpoint_request_stats(project_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_endpoint_request_stats_endpoint_id ON apis.endpoint_request_stats(endpoint_id);

COMMIT;