-- Report replies also use a stable conversation key without creating chat history.
CREATE TABLE apis.slack_reply_batches (
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  conversation_id UUID NOT NULL,
  user_id UUID NOT NULL REFERENCES users.users(id) ON DELETE CASCADE,
  message_ts TEXT NOT NULL CHECK (message_ts ~ '^[0-9]+\.[0-9]+$'),
  replies JSONB NOT NULL CHECK (jsonb_typeof(replies) = 'array' AND jsonb_array_length(replies) > 0),
  delivered_count INTEGER NOT NULL DEFAULT 0 CHECK (delivered_count >= 0 AND delivered_count <= jsonb_array_length(replies)),
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY (project_id, conversation_id, user_id, message_ts)
);
