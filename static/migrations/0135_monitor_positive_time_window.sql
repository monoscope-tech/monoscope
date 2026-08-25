UPDATE monitors.query_monitors
SET time_window_mins = 60
WHERE time_window_mins <= 0;

ALTER TABLE monitors.query_monitors
  DROP CONSTRAINT IF EXISTS query_monitors_positive_time_window,
  ADD CONSTRAINT query_monitors_positive_time_window CHECK (time_window_mins > 0);
