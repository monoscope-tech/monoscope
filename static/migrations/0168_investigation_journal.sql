CREATE TABLE apis.investigation_journal (
  id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  run_id UUID NOT NULL,
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  user_id UUID NOT NULL REFERENCES users.users(id) ON DELETE CASCADE,
  team_id TEXT NOT NULL CHECK (team_id <> ''),
  channel_id TEXT NOT NULL CHECK (channel_id <> ''),
  thread_ts TEXT NOT NULL CHECK (thread_ts ~ '^[0-9]+\.[0-9]+$'),
  message_ts TEXT NOT NULL CHECK (message_ts ~ '^[0-9]+\.[0-9]+$'),
  observed_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  event JSONB NOT NULL
);
CREATE INDEX investigation_journal_thread ON apis.investigation_journal
  (project_id, team_id, channel_id, thread_ts, id DESC);
CREATE INDEX investigation_journal_run ON apis.investigation_journal (run_id, id);
