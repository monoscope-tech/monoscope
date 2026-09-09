CREATE TABLE apis.slack_investigation_progress (
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  team_id TEXT NOT NULL,
  channel_id TEXT NOT NULL,
  thread_ts TEXT NOT NULL CHECK (thread_ts ~ '^[0-9]+\.[0-9]+$'),
  message_ts TEXT NOT NULL CHECK (message_ts ~ '^[0-9]+\.[0-9]+$'),
  progress_ts TEXT NOT NULL CHECK (progress_ts ~ '^[0-9]+\.[0-9]+$'),
  PRIMARY KEY (project_id, team_id, channel_id, thread_ts, message_ts)
);
