-- PRECONDITION: remove existing duplicate rows grouped by
-- (project_id, timestamp, id), and verify the query below returns no rows.
-- This can be expensive on the telemetry hypertable and must be scheduled as
-- an operational cleanup, not run implicitly during an application rollout.
-- TimescaleDB hypertables do not support CREATE INDEX CONCURRENTLY, so activate
-- this only in a maintenance window sized for the blocking index build.
--
-- SELECT project_id, timestamp, id
-- FROM otel_logs_and_spans
-- GROUP BY project_id, timestamp, id
-- HAVING count(*) > 1
-- LIMIT 1;
--
-- Activate after cleanup with:
-- git mv static/migrations-pending/0130_otel_event_id_uniqueness.sql static/migrations/

CREATE UNIQUE INDEX IF NOT EXISTS otel_logs_and_spans_event_id_uq
  ON otel_logs_and_spans (project_id, timestamp, id);
