ALTER TABLE monitors.query_monitors ADD COLUMN IF NOT EXISTS muted_until TIMESTAMPTZ;
ALTER TABLE monitors.query_monitors DROP COLUMN IF EXISTS muted_at;
