ALTER TABLE apis.error_patterns ADD COLUMN IF NOT EXISTS regression_count INT NOT NULL DEFAULT 0;
ALTER TABLE apis.error_patterns ALTER COLUMN resolution_threshold_minutes SET DEFAULT 360;
UPDATE apis.error_patterns SET resolution_threshold_minutes = 360 WHERE resolution_threshold_minutes = 30;
