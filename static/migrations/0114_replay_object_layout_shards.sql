-- Queue bounded, deterministic shards because the original migration job's
-- self-enqueue was not durable in production. Each session belongs to exactly
-- one shard; verified-copy and conditional-update semantics make retries safe.
--
-- TEMPORARY: this migration remains in history, but the
-- MigrateReplayStorageShard Haskell constructor and handler can be deleted
-- after every environment reports zero legacy replay keys and old-key cleanup
-- has been verified.
INSERT INTO background_jobs (run_at, status, payload)
SELECT
  now(),
  'queued',
  jsonb_build_object(
    'tag', 'MigrateReplayStorageShard',
    'contents', jsonb_build_array(32, shard_index, 1000)
  )
FROM generate_series(0, 31) AS shard_index
WHERE NOT EXISTS (
  SELECT 1
  FROM background_jobs
  WHERE payload->>'tag' = 'MigrateReplayStorageShard'
    AND (payload->'contents'->>1)::int = shard_index
    AND status IN ('queued', 'locked')
);
