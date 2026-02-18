BEGIN;

ALTER TABLE apis.errors
  ADD COLUMN slack_thread_ts TEXT,
  ADD COLUMN discord_message_id TEXT;

COMMIT;
