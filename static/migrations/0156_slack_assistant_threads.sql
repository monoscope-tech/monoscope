-- Navigation context is retained independently of any project authorization.
CREATE TABLE apis.slack_assistant_threads (
  team_id TEXT NOT NULL CHECK (team_id <> ''),
  channel_id TEXT NOT NULL CHECK (channel_id <> ''),
  thread_ts TEXT NOT NULL CHECK (thread_ts ~ '^[0-9]+\.[0-9]+$'),
  user_id TEXT NOT NULL CHECK (user_id <> ''),
  context JSONB NOT NULL CHECK (jsonb_typeof(context) = 'object'),
  context_event_ts NUMERIC NOT NULL,
  PRIMARY KEY (team_id, channel_id, thread_ts)
);
