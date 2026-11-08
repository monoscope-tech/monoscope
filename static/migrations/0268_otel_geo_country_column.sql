-- Promote geo.country.iso_code out of the attributes blob, matching TimeFusion (#450).
-- `otelColumns` builds one insert for both dual-write legs, so the Postgres table needs the
-- column too. Nullable, so no rewrite: rows written before this read NULL.
ALTER TABLE otel_logs_and_spans ADD COLUMN IF NOT EXISTS attributes___geo___country___iso_code TEXT;
