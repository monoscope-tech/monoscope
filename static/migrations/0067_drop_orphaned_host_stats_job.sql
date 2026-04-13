-- The host_requests_stats materialized view was dropped in 0012 but
-- its refresh job and procedure were never cleaned up.
SELECT delete_job(job_id) FROM timescaledb_information.jobs
  WHERE proc_name = 'refresh_host_request_stats_every_5mins';
DROP PROCEDURE IF EXISTS apis.refresh_host_request_stats_every_5mins(int, jsonb);
