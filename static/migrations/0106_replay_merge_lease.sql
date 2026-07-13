-- Serialize replay-session merges: at most one worker merges a given session at
-- a time. Duplicate/queued MergeReplaySession jobs that find the lease held skip
-- instead of piling on (the 2026-07-13 incident: 24 concurrent merges of one
-- 1.4 GiB session, each holding multi-GB heap, melted every app replica).
-- Nullable so a lease left behind by a worker that died mid-merge can be
-- reclaimed once it goes stale (see mergeOneSessionByKeys).
ALTER TABLE projects.replay_sessions ADD COLUMN IF NOT EXISTS merge_started_at TIMESTAMPTZ;
