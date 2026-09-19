BEGIN;

ALTER TABLE apis.ai_chat_messages
  DROP CONSTRAINT ai_chat_messages_role_check,
  ADD CONSTRAINT ai_chat_messages_role_check
    CHECK (role IN ('user', 'assistant', 'system', 'execution_event'));

COMMIT;
