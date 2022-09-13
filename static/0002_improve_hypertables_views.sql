CREATE MATERIALIZED VIEW apis.project_requests_by_endpoint_per_min WITH (timescaledb.continuous)
    AS SELECT
    time_bucket('1 minute', created_at) as timeB,
    project_id,
    endpoint_hash,
    concat_ws(' ', method, url_path)::text endpoint_title,
    count(id) total_count,
    percentile_agg(EXTRACT(epoch FROM duration)) as agg,
    sum(EXTRACT(epoch FROM duration)) as total_time,
  FROM
    apis.request_dumps
  GROUP BY
    project_id,
    timeB,
    method,
    url_path;

-- continuous aggregate policy to refresh every hour.
SELECT add_continuous_aggregate_policy('apis.project_requests_by_endpoint_per_min',
     start_offset => INTERVAL '2 months',
     end_offset => INTERVAL '1 day',
     schedule_interval => INTERVAL '1 hour');


-- Create a view that tracks project request related statistic points from the request dump table.
DROP MATERIALIZED VIEW IF EXISTS apis.project_requests_by_endpoint_per_min;
CREATE MATERIALIZED VIEW apis.project_requests_by_endpoint_per_min AS 
  SELECT
      project_id,
      json_agg(json_build_array(timeB,enp, count))::text ts_text
  FROM
      (

      ) ts_data
      GROUP BY project_id;
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_project_requests_by_endpoint_per_min_project_id ON apis.project_requests_by_endpoint_per_min(project_id);
