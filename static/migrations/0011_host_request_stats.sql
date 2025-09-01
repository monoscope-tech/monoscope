BEGIN;

DROP MATERIALIZED VIEW IF EXISTS apis.host_requests_stats;

CREATE MATERIALIZED VIEW apis.host_requests_stats AS
WITH filtered_requests AS (
    SELECT attributes->'net'->'host'->>'name' AS host,
           COUNT(*) AS events_count,
           MAX(timestamp) AS last_seen,
           MIN(timestamp) AS first_seen,
           project_id
    FROM otel_logs_and_spans
    WHERE name IN ('monoscope.http', 'apitoolkit-http-span')
    GROUP BY project_id, host
)
SELECT DISTINCT ON (ep.project_id, ep.host)
       ep.project_id,
       ep.host,
       COALESCE(fr.events_count, 0) AS events_count,
       fr.last_seen,
       fr.first_seen,
       ep.outgoing
FROM apis.endpoints ep
LEFT JOIN filtered_requests fr
       ON ep.host = fr.host
      AND ep.project_id::text = fr.project_id
WHERE ep.host != '';

CREATE UNIQUE INDEX idx_host_requests_stats_project_host
    ON apis.host_requests_stats(project_id, host);

CREATE OR REPLACE PROCEDURE apis.refresh_host_request_stats_every_5mins(job_id int, config jsonb)
LANGUAGE plpgsql
AS $$
BEGIN
  RAISE NOTICE 'Refreshing host_requests_stats (job_id %, config %)', job_id, config;
  REFRESH MATERIALIZED VIEW CONCURRENTLY apis.host_requests_stats;
END;
$$;

-- Schedule it to run every 5 minutes
SELECT add_job('apis.refresh_host_request_stats_every_5mins', '5min');


COMMIT;