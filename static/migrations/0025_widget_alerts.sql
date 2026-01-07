-- Widget-integrated alerts: link alerts to dashboard widgets

ALTER TABLE monitors.query_monitors
  ADD COLUMN IF NOT EXISTS widget_id TEXT,
  ADD COLUMN IF NOT EXISTS dashboard_id UUID REFERENCES projects.dashboards(id) ON DELETE CASCADE,
  ADD COLUMN IF NOT EXISTS show_threshold_lines TEXT DEFAULT 'always',
  ADD COLUMN IF NOT EXISTS alert_recovery_threshold INT,
  ADD COLUMN IF NOT EXISTS warning_recovery_threshold INT,
  ADD COLUMN IF NOT EXISTS current_status TEXT DEFAULT 'normal',
  ADD COLUMN IF NOT EXISTS current_value INT DEFAULT 0;

DO $$ BEGIN
  ALTER TABLE monitors.query_monitors
    ADD CONSTRAINT check_show_threshold_lines
    CHECK (show_threshold_lines IN ('always', 'on_breach', 'never'));
EXCEPTION
  WHEN duplicate_object THEN NULL;
END $$;

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
