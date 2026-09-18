BEGIN;

-- The thread page reads the newest messages with a deterministic id tie-breaker.
-- The earlier three-column index cannot satisfy the full ordering and leaves an
-- incremental sort on every history read.
CREATE INDEX IF NOT EXISTS ai_chat_messages_conversation_history
  ON apis.ai_chat_messages (project_id, conversation_id, created_at DESC, id DESC);

-- Conversation navigation derives an untitled thread's label from its first user
-- message. Keep that lateral lookup on the small role-specific index.
CREATE INDEX IF NOT EXISTS ai_chat_messages_first_user
  ON apis.ai_chat_messages (project_id, conversation_id, created_at, id)
  WHERE role = 'user';

COMMIT;
