CREATE TABLE apis.slack_investigation_threads (
  team_id TEXT NOT NULL CHECK (team_id <> ''),
  channel_id TEXT NOT NULL CHECK (channel_id <> ''),
  thread_ts TEXT NOT NULL CHECK (thread_ts ~ '^[0-9]+\.[0-9]+$'),
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  PRIMARY KEY (team_id, channel_id, thread_ts)
);
