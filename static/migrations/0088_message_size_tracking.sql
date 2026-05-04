-- Track ingested payload size per row, plus daily rollup of bytes and a
-- separate metrics-row count alongside the existing log/span count.
--
-- Pricing math is unchanged: total_requests still drives splitUsageIntoChunks
-- and the $/request estimator. The new columns are visibility-only for now.

ALTER TABLE otel_logs_and_spans
  ADD COLUMN IF NOT EXISTS message_size_bytes BIGINT NOT NULL DEFAULT 0;

ALTER TABLE telemetry.metrics
  ADD COLUMN IF NOT EXISTS message_size_bytes BIGINT NOT NULL DEFAULT 0;

ALTER TABLE apis.daily_usage
  ADD COLUMN IF NOT EXISTS total_metrics      BIGINT NOT NULL DEFAULT 0,
  ADD COLUMN IF NOT EXISTS total_event_bytes  BIGINT NOT NULL DEFAULT 0,
  ADD COLUMN IF NOT EXISTS total_metric_bytes BIGINT NOT NULL DEFAULT 0;
