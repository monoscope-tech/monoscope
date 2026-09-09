CREATE TABLE apis.slack_identity_requests (
  id UUID PRIMARY KEY,
  receipt_id UUID NOT NULL UNIQUE REFERENCES apis.slack_events(id) ON DELETE CASCADE,
  expires_at TIMESTAMPTZ NOT NULL DEFAULT now() + interval '15 minutes',
  consumed_at TIMESTAMPTZ
);

CREATE TABLE apis.slack_identities (
  team_id TEXT NOT NULL CHECK (team_id <> ''),
  slack_user_id TEXT NOT NULL CHECK (slack_user_id <> ''),
  user_id UUID NOT NULL REFERENCES users.users(id) ON DELETE CASCADE,
  project_id UUID NOT NULL REFERENCES apis.slack(project_id) ON DELETE CASCADE,
  PRIMARY KEY (team_id, slack_user_id)
);
