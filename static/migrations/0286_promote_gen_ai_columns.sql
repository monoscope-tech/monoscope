-- Promote the OpenTelemetry GenAI identity and usage out of the attributes blob, matching TimeFusion
-- (#457, storage widened 2026-11-16). `otelColumns` builds one insert for both dual-write legs, so
-- the Postgres table needs the columns too. Nullable, so no rewrite: rows written before this read NULL.
ALTER TABLE otel_logs_and_spans
  ADD COLUMN IF NOT EXISTS attributes___gen_ai___operation___name TEXT,
  ADD COLUMN IF NOT EXISTS attributes___gen_ai___request___model TEXT,
  ADD COLUMN IF NOT EXISTS attributes___gen_ai___conversation___id TEXT,
  ADD COLUMN IF NOT EXISTS attributes___gen_ai___agent___name TEXT,
  ADD COLUMN IF NOT EXISTS attributes___gen_ai___tool___name TEXT,
  ADD COLUMN IF NOT EXISTS attributes___gen_ai___usage___input_tokens BIGINT,
  ADD COLUMN IF NOT EXISTS attributes___gen_ai___usage___output_tokens BIGINT,
  ADD COLUMN IF NOT EXISTS attributes___gen_ai___usage___cost DOUBLE PRECISION;
