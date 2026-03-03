-- Align with Sentry: auto-resolve after 7 days of inactivity instead of 6 hours
-- 7 days = 10080 minutes
ALTER TABLE apis.error_patterns ALTER COLUMN resolution_threshold_minutes SET DEFAULT 10080;
UPDATE apis.error_patterns SET resolution_threshold_minutes = 10080 WHERE resolution_threshold_minutes = 360;
