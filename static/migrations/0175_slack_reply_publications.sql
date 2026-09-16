CREATE TABLE apis.slack_reply_publications (
  publication_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL,
  conversation_id UUID NOT NULL,
  user_id UUID NOT NULL,
  message_ts TEXT NOT NULL,
  part INTEGER NOT NULL CHECK (part >= 0),
  team_id TEXT NOT NULL CHECK (team_id <> ''),
  channel_id TEXT NOT NULL CHECK (channel_id <> ''),
  thread_ts TEXT NOT NULL CHECK (thread_ts ~ '^[0-9]+\.[0-9]+$'),
  slack_ts TEXT CHECK (slack_ts ~ '^[0-9]+\.[0-9]+$'),
  UNIQUE (project_id, conversation_id, user_id, message_ts, part),
  FOREIGN KEY (project_id, conversation_id, user_id, message_ts)
    REFERENCES apis.slack_reply_batches(project_id, conversation_id, user_id, message_ts) ON DELETE CASCADE
);
