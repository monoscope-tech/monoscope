-- Clean up failed jobs with tags that no longer exist in the codebase
DELETE FROM background_jobs
WHERE status = 'failed'
  AND payload->>'tag' IN ('FifteenMinutesLogsPatternProcessing', 'QueryMonitorsTriggered');
