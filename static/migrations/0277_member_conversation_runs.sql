BEGIN;

ALTER TABLE apis.ai_routine_runs
  DROP CONSTRAINT interactive_agent_run_policy,
  ADD CONSTRAINT interactive_agent_run_policy CHECK (
    source->>'tag' NOT IN ('InteractiveRun', 'ConversationRun')
    OR (routine_id IS NULL AND policy IS NOT NULL AND input IS NOT NULL)
  ),
  ADD CONSTRAINT member_conversation_run_actor CHECK (
    source->>'tag' <> 'ConversationRun' OR requested_by IS NOT NULL
  );

COMMIT;
