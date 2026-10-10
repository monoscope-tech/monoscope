CREATE TABLE projects.github_installation_attempts (
  id UUID PRIMARY KEY,
  session_id UUID NOT NULL REFERENCES users.persistent_sessions(id) ON DELETE CASCADE,
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  destination TEXT NOT NULL CHECK (destination IN ('code', 'sync')),
  installation_id BIGINT,
  expires_at TIMESTAMPTZ NOT NULL
);
