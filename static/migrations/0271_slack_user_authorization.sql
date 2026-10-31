-- Workspace installation and personal evidence consent share expiring OAuth receipts.
ALTER TABLE apis.slack_install_requests
  ADD COLUMN team_id TEXT,
  ADD COLUMN slack_user_id TEXT,
  ADD COLUMN code_verifier TEXT,
  ADD COLUMN consumed_at TIMESTAMPTZ,
  ADD CONSTRAINT slack_oauth_purpose CHECK (
    (team_id IS NULL AND slack_user_id IS NULL AND code_verifier IS NULL)
    OR (team_id IS NOT NULL AND team_id <> '' AND slack_user_id IS NOT NULL AND slack_user_id <> ''
      AND code_verifier IS NOT NULL AND code_verifier ~ '^[A-Za-z0-9._~-]{43,128}$' AND NOT onboarding)
  );

CREATE TABLE apis.slack_user_credentials (
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  user_id UUID NOT NULL REFERENCES users.users(id) ON DELETE CASCADE,
  team_id TEXT NOT NULL,
  slack_user_id TEXT NOT NULL,
  access_token TEXT NOT NULL CHECK (access_token LIKE 'v1:%'),
  refresh_token TEXT CHECK (refresh_token LIKE 'v1:%'),
  expires_at TIMESTAMPTZ,
  scopes TEXT[] NOT NULL,
  PRIMARY KEY (project_id, user_id, team_id, slack_user_id),
  FOREIGN KEY (team_id, slack_user_id) REFERENCES apis.slack_identities(team_id, slack_user_id) ON DELETE CASCADE,
  CONSTRAINT slack_user_token_lifetime CHECK ((refresh_token IS NULL) = (expires_at IS NULL)),
  CONSTRAINT slack_user_read_scopes CHECK (
    cardinality(scopes) > 0 AND scopes <@ ARRAY[
      'search:read.public', 'search:read.private', 'search:read.im', 'search:read.mpim',
      'channels:history', 'groups:history', 'im:history', 'mpim:history'
    ]::TEXT[]
  )
);
