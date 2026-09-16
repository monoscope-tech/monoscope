CREATE TABLE apis.slack_api_cooldowns (
  team_id TEXT NOT NULL CHECK (team_id <> ''),
  method TEXT NOT NULL CHECK (method <> ''),
  retry_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (team_id, method)
);
