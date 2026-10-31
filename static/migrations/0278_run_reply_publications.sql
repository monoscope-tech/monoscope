BEGIN;

DO $$ DECLARE constraint_name text; BEGIN
  FOR constraint_name IN SELECT conname FROM pg_constraint
    WHERE conrelid = 'apis.slack_reply_publications'::regclass
      AND confrelid = 'apis.slack_reply_batches'::regclass
  LOOP
    EXECUTE format('ALTER TABLE apis.slack_reply_publications DROP CONSTRAINT %I', constraint_name);
  END LOOP;
END $$;

ALTER TABLE apis.slack_reply_batches
  DROP CONSTRAINT slack_reply_batches_pkey,
  ALTER COLUMN message_ts DROP NOT NULL,
  ADD COLUMN run_id uuid UNIQUE REFERENCES apis.ai_routine_runs(id) ON DELETE CASCADE,
  ADD UNIQUE (project_id, conversation_id, user_id, message_ts),
  ADD UNIQUE (project_id, conversation_id, user_id, run_id),
  ADD CHECK ((message_ts IS NULL) <> (run_id IS NULL));

ALTER TABLE apis.slack_reply_publications
  ALTER COLUMN message_ts DROP NOT NULL,
  ALTER COLUMN thread_ts DROP NOT NULL,
  ADD COLUMN run_id uuid,
  ADD UNIQUE (run_id, part),
  ADD CHECK ((message_ts IS NULL) <> (run_id IS NULL)),
  ADD FOREIGN KEY (project_id, conversation_id, user_id, message_ts)
    REFERENCES apis.slack_reply_batches(project_id, conversation_id, user_id, message_ts) ON DELETE CASCADE,
  ADD FOREIGN KEY (project_id, conversation_id, user_id, run_id)
    REFERENCES apis.slack_reply_batches(project_id, conversation_id, user_id, run_id) ON DELETE CASCADE;

COMMIT;
