CREATE TABLE apis.slack_app_contexts (
  team_id TEXT NOT NULL CHECK (team_id <> ''),
  channel_id TEXT NOT NULL CHECK (channel_id <> ''),
  user_id TEXT NOT NULL CHECK (user_id <> ''),
  context JSONB NOT NULL CHECK (jsonb_typeof(context) = 'object'),
  context_event_ts NUMERIC NOT NULL,
  PRIMARY KEY (team_id, channel_id, user_id)
);

ALTER TABLE apis.slack_investigation_threads
  ADD COLUMN title TEXT,
  ADD COLUMN title_event_ts NUMERIC,
  ADD CONSTRAINT slack_thread_title_timestamp CHECK ((title IS NULL) = (title_event_ts IS NULL));
