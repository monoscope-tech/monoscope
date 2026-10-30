BEGIN;

-- Keep the existing store and Slack keys during the transition to shared runs.
ALTER TABLE apis.slack_turn_checkpoints
  DROP CONSTRAINT slack_turn_checkpoints_pkey;

ALTER TABLE apis.slack_turn_checkpoints
  ALTER COLUMN message_ts DROP NOT NULL,
  ADD COLUMN run_id UUID REFERENCES apis.ai_routine_runs(id) ON DELETE CASCADE,
  ADD CONSTRAINT slack_turn_checkpoints_legacy_turn
    UNIQUE (project_id, conversation_id, user_id, message_ts),
  ADD CONSTRAINT agent_checkpoint_identity
    UNIQUE NULLS NOT DISTINCT (project_id, conversation_id, user_id, message_ts, run_id),
  ADD CONSTRAINT agent_checkpoint_source
    CHECK (num_nonnulls(message_ts, run_id) = 1);

COMMIT;
