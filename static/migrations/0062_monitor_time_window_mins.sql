-- Store the "Include rows from" time window on query monitors so BackgroundJobs
-- can scope its alert query to the same lookback the user configured.
ALTER TABLE monitors.query_monitors
  ADD COLUMN IF NOT EXISTS time_window_mins INT NOT NULL DEFAULT 60;
