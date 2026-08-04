-- Queue the temporary online object migration after the application containing
-- the MigrateReplayStorage decoder is deployed. The job copies + verifies each
-- object before changing replay_sessions and only then deletes the legacy key.
--
-- TEMPORARY: this migration remains in migration history, but the Haskell job
-- constructor/handler can be deleted after all environments report zero legacy
-- replay keys and leftover old-key cleanup has been verified.
INSERT INTO background_jobs (run_at, status, payload)
SELECT now(), 'queued', '{"tag":"MigrateReplayStorage","contents":100}'::jsonb
WHERE NOT EXISTS (
  SELECT 1
  FROM background_jobs
  WHERE payload->>'tag' = 'MigrateReplayStorage'
    AND status IN ('queued', 'locked')
);
