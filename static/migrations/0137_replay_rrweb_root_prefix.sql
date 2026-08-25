-- Re-seed the replay storage migration after the key layout moved from
-- <project>/rrweb/<date>/<session>/ (and the older flat <session>/) to a single
-- rrweb/<project>/<date>/<session>/ root, so replay objects no longer sit at the
-- bucket root alongside everything else we store.
--
-- Both legacy shapes fail `NOT LIKE 'rrweb/%'`, so one pass drains them together.
-- Batch size is well above the current per-shard session count because a shard
-- job does not re-enqueue its own remainder; sessions beyond the batch would
-- wait for the next seeding migration.
--
-- TEMPORARY: this migration remains in history, but the
-- MigrateReplayStorageShard Haskell constructor and handler can be deleted after
-- every environment reports zero legacy replay keys and old-key cleanup has been
-- verified.
INSERT INTO background_jobs (run_at, status, payload)
SELECT
  now(),
  'queued',
  jsonb_build_object(
    'tag', 'MigrateReplayStorageShard',
    'contents', jsonb_build_array(32, shard_index, 4000)
  )
FROM generate_series(0, 31) AS shard_index
WHERE NOT EXISTS (
  SELECT 1
  FROM background_jobs
  WHERE payload->>'tag' = 'MigrateReplayStorageShard'
    AND (payload->'contents'->>1)::int = shard_index
    AND status IN ('queued', 'locked')
);
