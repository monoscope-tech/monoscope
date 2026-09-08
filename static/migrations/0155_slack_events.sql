CREATE TABLE apis.slack_events (
  id UUID PRIMARY KEY,
  team_id TEXT NOT NULL CHECK (team_id <> ''),
  event_id TEXT NOT NULL CHECK (event_id <> ''),
  payload JSONB NOT NULL CHECK (jsonb_typeof(payload) = 'object'),
  received_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  processed_at TIMESTAMPTZ,
  UNIQUE (team_id, event_id)
);
