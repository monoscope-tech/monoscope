-- Preserve the event timestamp paired with the first sampled trace.
-- Processing time can be hours later for delayed ingestion. Existing rows remain
-- NULL because their first event timestamp cannot be recovered reliably.
ALTER TABLE apis.error_patterns ADD COLUMN IF NOT EXISTS first_trace_at TIMESTAMPTZ;
