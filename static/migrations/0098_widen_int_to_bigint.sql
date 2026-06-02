-- Align column types with hasql's strict decoders. Generic 'DecodeRow' uses
-- 'D.int8' for Haskell 'Int' (64-bit) and 'D.float8' for 'Double', so any
-- INT/NUMERIC column mapped to those fields blows up at decode time. BIGINT /
-- DOUBLE PRECISION are strict supersets of the existing data.
--
-- Per-table ALTERs are combined so PG performs a single heap+TOAST+index
-- rewrite per table instead of one per column (4 rewrites vs 23).
ALTER TABLE apis.issues
  ALTER COLUMN affected_requests       TYPE BIGINT,
  ALTER COLUMN affected_clients        TYPE BIGINT,
  ALTER COLUMN llm_enhancement_version TYPE BIGINT,
  ALTER COLUMN seq_num                 TYPE BIGINT,
  ALTER COLUMN error_rate              TYPE DOUBLE PRECISION;

ALTER TABLE apis.error_patterns
  ALTER COLUMN occurrences_1m               TYPE BIGINT,
  ALTER COLUMN occurrences_5m               TYPE BIGINT,
  ALTER COLUMN occurrences_1h               TYPE BIGINT,
  ALTER COLUMN occurrences_24h              TYPE BIGINT,
  ALTER COLUMN quiet_minutes                TYPE BIGINT,
  ALTER COLUMN resolution_threshold_minutes TYPE BIGINT,
  ALTER COLUMN baseline_samples             TYPE BIGINT,
  ALTER COLUMN notify_every_minutes         TYPE BIGINT,
  ALTER COLUMN regression_count             TYPE BIGINT;

ALTER TABLE apis.notification_rate_limit
  ALTER COLUMN count TYPE BIGINT;

ALTER TABLE monitors.query_monitors
  ALTER COLUMN check_interval_mins          TYPE BIGINT,
  ALTER COLUMN threshold_sustained_for_mins TYPE BIGINT,
  ALTER COLUMN renotify_interval_mins       TYPE BIGINT,
  ALTER COLUMN stop_after_count             TYPE BIGINT,
  ALTER COLUMN time_window_mins             TYPE BIGINT,
  ALTER COLUMN notification_count           TYPE BIGINT;
