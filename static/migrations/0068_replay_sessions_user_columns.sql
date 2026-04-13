ALTER TABLE projects.replay_sessions
  ADD COLUMN IF NOT EXISTS user_id    TEXT,
  ADD COLUMN IF NOT EXISTS user_email TEXT,
  ADD COLUMN IF NOT EXISTS user_name  TEXT;

CREATE INDEX IF NOT EXISTS idx_replay_sessions_user_id
  ON projects.replay_sessions(user_id) WHERE user_id IS NOT NULL;
