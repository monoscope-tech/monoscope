CREATE TABLE apis.slack_install_requests (
  id UUID PRIMARY KEY,
  user_id UUID NOT NULL REFERENCES users.users(id) ON DELETE CASCADE,
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  onboarding BOOLEAN NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL DEFAULT now() + interval '15 minutes'
);

CREATE INDEX slack_install_requests_expiry ON apis.slack_install_requests(expires_at);
