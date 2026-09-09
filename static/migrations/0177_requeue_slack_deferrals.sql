-- OddJobs polls queued/retry jobs; pending was never a runnable status.
UPDATE background_jobs
SET status = 'queued'
WHERE status = 'pending'
  AND payload->>'tag' IN ('ProcessSlackEvent', 'RefreshSlackProgress');
