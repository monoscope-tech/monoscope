-- Widget-integrated alerts: link alerts to dashboard widgets

-- Add new columns
ALTER TABLE monitors.query_monitors
  ADD COLUMN IF NOT EXISTS widget_id TEXT,
  ADD COLUMN IF NOT EXISTS dashboard_id UUID REFERENCES projects.dashboards(id) ON DELETE CASCADE,
  ADD COLUMN IF NOT EXISTS alert_recovery_threshold DOUBLE PRECISION,
  ADD COLUMN IF NOT EXISTS warning_recovery_threshold DOUBLE PRECISION,
  ADD COLUMN IF NOT EXISTS current_status TEXT NOT NULL DEFAULT 'normal',
  ADD COLUMN IF NOT EXISTS current_value DOUBLE PRECISION NOT NULL DEFAULT 0;

-- Convert existing threshold columns to DOUBLE PRECISION for decimal support
ALTER TABLE monitors.query_monitors
  ALTER COLUMN alert_threshold TYPE DOUBLE PRECISION,
  ALTER COLUMN warning_threshold TYPE DOUBLE PRECISION;

DO $$ BEGIN
  ALTER TABLE monitors.query_monitors
    ADD CONSTRAINT check_current_status
    CHECK (current_status IN ('normal', 'warning', 'alerting'));
EXCEPTION
  WHEN duplicate_object THEN NULL;
END $$;

CREATE INDEX IF NOT EXISTS idx_query_monitors_widget_id
  ON monitors.query_monitors(widget_id)
  WHERE widget_id IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_query_monitors_dashboard_id
  ON monitors.query_monitors(dashboard_id)
  WHERE dashboard_id IS NOT NULL;
