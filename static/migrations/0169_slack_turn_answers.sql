ALTER TABLE apis.ai_chat_messages ADD COLUMN slack_message_ts TEXT
  CHECK (slack_message_ts ~ '^[0-9]+\.[0-9]+$');
CREATE UNIQUE INDEX ai_chat_messages_slack_turn ON apis.ai_chat_messages
  (project_id, conversation_id, slack_message_ts, role) WHERE slack_message_ts IS NOT NULL;
CREATE TABLE apis.slack_turn_answers (
  project_id UUID NOT NULL,
  conversation_id UUID NOT NULL,
  user_id UUID NOT NULL REFERENCES users.users(id) ON DELETE CASCADE,
  message_ts TEXT NOT NULL CHECK (message_ts ~ '^[0-9]+\.[0-9]+$'),
  answer JSONB NOT NULL,
  completed_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY (project_id, conversation_id, user_id, message_ts),
  FOREIGN KEY (project_id, conversation_id)
    REFERENCES apis.ai_conversations(project_id, conversation_id) ON DELETE CASCADE
);
