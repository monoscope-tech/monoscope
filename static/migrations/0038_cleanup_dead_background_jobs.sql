-- Clean up failed jobs with tags that no longer exist in the codebase
DELETE FROM background_jobs
WHERE status = 'failed'
  AND payload->>'tag' IN ('FifteenMinutesLogsPatternProcessing', 'QueryMonitorsTriggered');

-- Drop stub SQL procedures; scheduling is fully handled in Haskell now
DROP PROCEDURE IF EXISTS monitors.check_triggered_query_monitors(int, jsonb);
DROP PROCEDURE IF EXISTS monoscope_daily_job(int, jsonb);
