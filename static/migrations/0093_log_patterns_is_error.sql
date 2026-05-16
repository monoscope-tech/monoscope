-- Flag log patterns whose originating events were errors (level in
-- {error,fatal,critical,...} or span status ERROR / http >= 400 / exception
-- attributes present). Set at extraction time and OR-merged on upsert so a
-- pattern that ever fires as an error stays flagged. Drives the alert title
-- ("New error log pattern" vs generic) and the patterns-list badge so every
-- surface agrees on severity instead of re-deriving it from incomplete fields.
ALTER TABLE apis.log_patterns
  ADD COLUMN IF NOT EXISTS is_error BOOLEAN NOT NULL DEFAULT FALSE;

CREATE INDEX IF NOT EXISTS log_patterns_is_error_idx
  ON apis.log_patterns (project_id, is_error)
  WHERE is_error;
