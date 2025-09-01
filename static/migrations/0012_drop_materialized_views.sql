BEGIN;

DROP MATERIALIZED VIEW IF EXISTS apis.host_requests_stats;
DROP MATERIALIZED VIEW IF EXISTS apis.endpoint_request_stats;

COMMIT;