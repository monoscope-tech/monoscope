CREATE TABLE IF NOT EXISTS projects.replay_sessions (
  id            BIGSERIAL PRIMARY KEY,
  session_id    UUID NOT NULL,
  project_id    UUID NOT NULL REFERENCES projects.projects(id),
  last_event_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
  merged        BOOLEAN NOT NULL DEFAULT FALSE,
  created_at    TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
  updated_at    TIMESTAMPTZ NOT NULL DEFAULT current_timestamp
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_replay_sessions_session_id ON projects.replay_sessions(session_id);
CREATE INDEX IF NOT EXISTS idx_replay_sessions_unmerged ON projects.replay_sessions(last_event_at) WHERE merged = FALSE;
CREATE INDEX IF NOT EXISTS idx_replay_sessions_project_id ON projects.replay_sessions(project_id);
