DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name = 'otel_logs_and_spans') THEN
    IF NOT EXISTS (SELECT 1 FROM timescaledb_information.compression_settings WHERE hypertable_name = 'otel_logs_and_spans') THEN
      ALTER TABLE otel_logs_and_spans SET (timescaledb.compress, timescaledb.compress_orderby = 'timestamp DESC');
    END IF;
    PERFORM add_compression_policy('otel_logs_and_spans', INTERVAL '1 hour', if_not_exists => true);
    PERFORM add_retention_policy('otel_logs_and_spans', INTERVAL '30 days', if_not_exists => true);
  END IF;

  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = 'telemetry' AND table_name = 'metrics') THEN
    IF NOT EXISTS (SELECT 1 FROM timescaledb_information.compression_settings WHERE hypertable_schema = 'telemetry' AND hypertable_name = 'metrics') THEN
      ALTER TABLE telemetry.metrics SET (timescaledb.compress, timescaledb.compress_orderby = 'timestamp DESC');
    END IF;
    PERFORM add_compression_policy('telemetry.metrics', INTERVAL '1 hour', if_not_exists => true);
    PERFORM add_retention_policy('telemetry.metrics', INTERVAL '30 days', if_not_exists => true);
  END IF;
END $$;
