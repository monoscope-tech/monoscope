CREATE TABLE apis.slack_turn_checkpoints (
  project_id UUID NOT NULL,
  conversation_id UUID NOT NULL,
  user_id UUID NOT NULL REFERENCES users.users(id) ON DELETE CASCADE,
  message_ts TEXT NOT NULL CHECK (message_ts ~ '^[0-9]+\.[0-9]+$'),
  checkpoint JSONB NOT NULL,
  revision BIGINT NOT NULL DEFAULT 0 CHECK (revision >= 0),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY (project_id, conversation_id, user_id, message_ts),
  FOREIGN KEY (project_id, conversation_id)
    REFERENCES apis.ai_conversations(project_id, conversation_id) ON DELETE CASCADE
);
