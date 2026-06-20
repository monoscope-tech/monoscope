-- attributes___db___response___status_code is the DBMS-specific response/error
-- code. Per OTel semconv it is a STRING (a Postgres SQLSTATE like '42804', a
-- MongoDB error name, etc.). The migration source (0002), TimeFusion's Utf8
-- column, and the ingest encoder all treat it as text — but the LIVE PG column
-- drifted to INTEGER. Any batch carrying a non-null value now fails the PG-leg
-- INSERT ("column is of type integer but expression is of type text") and the
-- whole multi-row batch is dropped. Realign PG to TEXT.
--
-- SCALE NOTE: otel_logs_and_spans is a compressed TimescaleDB hypertable
-- (multi-TB, 30-day retention). An in-place `ALTER COLUMN ... TYPE TEXT` is NOT
-- viable here — TimescaleDB rejects type changes on compressed hypertables, and
-- forcing it would decompress + rewrite every chunk. DROP + re-ADD is instead a
-- metadata-only operation (milliseconds at any size). Cost: the historical
-- INTEGER values in THIS one column are discarded — acceptable because it is
-- NULL for nearly all spans, ages out under the 30-day policy, and TimeFusion
-- holds the authoritative copy going forward.
--
-- Guarded so it only fires on the drifted (integer) DB; fresh DBs created from
-- 0002 already have it as TEXT and skip the block (idempotent, re-runnable).
-- (DROP COLUMN on a compressed hypertable requires TimescaleDB >= 2.10.)
DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'otel_logs_and_spans'
      AND column_name = 'attributes___db___response___status_code'
      AND data_type = 'integer'
  ) THEN
    ALTER TABLE otel_logs_and_spans DROP COLUMN IF EXISTS attributes___db___response___status_code;
    ALTER TABLE otel_logs_and_spans ADD COLUMN attributes___db___response___status_code TEXT;
  END IF;
END $$;
