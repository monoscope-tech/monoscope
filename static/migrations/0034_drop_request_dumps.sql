BEGIN;
SELECT delete_job(job_id) FROM timescaledb_information.jobs
  WHERE proc_name = 'refresh_request_dump_views_every_5mins';
DROP PROCEDURE IF EXISTS apis.refresh_request_dump_views_every_5mins(int, jsonb);
DROP TABLE IF EXISTS apis.share_requests;
DROP TABLE IF EXISTS apis.request_dumps CASCADE;
COMMIT;
