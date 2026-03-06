ALTER TABLE apis.error_patterns
  ADD COLUMN IF NOT EXISTS root_cause TEXT,
  ADD COLUMN IF NOT EXISTS error_category TEXT;
