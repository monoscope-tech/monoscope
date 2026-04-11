-- Safety-net marker for the in-process extraction worker (see plan
-- squishy-imagining-diffie.md). Rows ingested after the deploy cutoff are
-- claimed by the worker's eager track via UPDATE-1 setting processed_at = now().
-- The hourly SafetyNetReprocess job sweeps rows where processed_at IS NULL and
-- re-drives them through the worker, acting as the only catch-up mechanism
-- once the legacy 5-min / 1-min jobs are retired.
ALTER TABLE otel_logs_and_spans
  ADD COLUMN IF NOT EXISTS processed_at TIMESTAMPTZ NULL;

-- Time-scoped partial index: only rows ingested at or after the deploy cutoff
-- are eligible for the safety net. Pre-cutoff rows are invisible here — they
-- were processed by the legacy 5-min / 1-min jobs before the cutover and
-- would otherwise blow up WAL on a full-table backfill UPDATE. The cutoff
-- literal MUST match EnvConfig.processedAtCutoff at runtime; a CI check
-- asserts both agree at startup.
--
-- No CONCURRENTLY: TimescaleDB hypertables do not support concurrent index
-- creation. TimescaleDB's CREATE INDEX handles chunk-level locking internally
-- and the partial index starts near-empty (only post-cutoff rows qualify), so
-- the build is cheap regardless.
CREATE INDEX IF NOT EXISTS idx_otel_unprocessed
  ON otel_logs_and_spans (project_id, timestamp)
  WHERE processed_at IS NULL
    AND timestamp >= TIMESTAMPTZ '2026-04-15 00:00:00+00';
