ALTER TABLE monitors.query_monitors
  ADD COLUMN IF NOT EXISTS renotify_interval_mins INT,
  ADD COLUMN IF NOT EXISTS stop_after_count INT,
  ADD COLUMN IF NOT EXISTS notification_count INT NOT NULL DEFAULT 0;
