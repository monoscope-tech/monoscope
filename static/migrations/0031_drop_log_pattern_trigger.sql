-- Drop per-row trigger that enqueued a background job per new log pattern insert.
-- New pattern issue creation is now batched in the hourly LogPatternHourlyProcessing job.
DROP TRIGGER IF EXISTS log_pattern_created_notify ON apis.log_patterns;
DROP FUNCTION IF EXISTS apis.new_log_pattern_proc();
