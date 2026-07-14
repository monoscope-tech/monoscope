-- Track sealed replay shard object keys in Postgres instead of discovering them
-- by listing S3. minio-hs's listObjects does not work against Cloudflare R2
-- (getObject/putObject do), so the read/manifest path must never list — it reads
-- the ordered shard keys from here and getObjects them.
ALTER TABLE projects.replay_sessions ADD COLUMN IF NOT EXISTS shard_keys TEXT[] NOT NULL DEFAULT '{}';

-- Bridge legacy sessions: those merged by the pre-sharding code have a single
-- <sid>/merged.json.gz monolith. Register it as the session's one shard so the
-- DB-sourced read path finds it (its unparseable key sorts first = oldest).
UPDATE projects.replay_sessions
   SET shard_keys = ARRAY[session_id::text || '/merged.json.gz']
 WHERE merged = TRUE AND (shard_keys IS NULL OR shard_keys = '{}');
