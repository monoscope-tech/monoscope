-- telemetry.metrics_meta (created in 0002, indexed in 0055) is superseded by
-- otel_metrics_meta (0108+). Nothing writes or reads it anymore. Dropping the
-- table also drops its indexes, unique constraint, and manage_updated_at trigger.
DROP TABLE IF EXISTS telemetry.metrics_meta;
