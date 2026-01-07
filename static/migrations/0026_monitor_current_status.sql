-- Add current_status column to track hysteresis-aware alert status
ALTER TABLE monitors.query_monitors
  ADD COLUMN IF NOT EXISTS current_status TEXT DEFAULT 'normal';

-- Add constraint for valid status values
DO $$ BEGIN
  ALTER TABLE monitors.query_monitors
    ADD CONSTRAINT check_current_status
    CHECK (current_status IN ('normal', 'warning', 'alerting'));
EXCEPTION
  WHEN duplicate_object THEN NULL;
END $$;
