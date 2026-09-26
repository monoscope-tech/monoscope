-- When the error was first seen in its current last_release (the release marker on the issue chart).
-- last_release_at tracks the newest event instead, which is what orders late batches.
ALTER TABLE apis.error_patterns ADD COLUMN IF NOT EXISTS last_release_since TIMESTAMPTZ;
UPDATE apis.error_patterns SET last_release_since = last_release_at WHERE last_release_since IS NULL;
