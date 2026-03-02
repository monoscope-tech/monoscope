ALTER TABLE monitors.query_monitors ADD COLUMN IF NOT EXISTS muted_at TIMESTAMPTZ;
