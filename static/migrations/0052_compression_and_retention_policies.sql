DO $$ BEGIN
  PERFORM add_compression_policy('otel_logs_and_spans', INTERVAL '1 hour', if_not_exists => true);
  PERFORM add_compression_policy('telemetry.metrics', INTERVAL '1 hour', if_not_exists => true);
  PERFORM add_retention_policy('otel_logs_and_spans', INTERVAL '30 days', if_not_exists => true);
  PERFORM add_retention_policy('telemetry.metrics', INTERVAL '30 days', if_not_exists => true);
END $$;
