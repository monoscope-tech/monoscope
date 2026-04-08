ALTER TABLE projects.replay_sessions ADD COLUMN IF NOT EXISTS file_keys TEXT[] NOT NULL DEFAULT '{}';

-- Existing unmerged sessions have no tracked file_keys; mark them merged so the
-- background merge job doesn't re-pick them forever. In-flight data remains in S3
-- and is still served via the legacy fallback path in getSessionEvents.
UPDATE projects.replay_sessions SET merged = TRUE, event_file_count = 0 WHERE merged = FALSE AND file_keys = '{}';
