CREATE INDEX IF NOT EXISTS idx_metrics_meta_updated_at
  ON telemetry.metrics_meta (updated_at);
