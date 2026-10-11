CREATE TABLE projects.repository_sync_leases (
  sync_id UUID PRIMARY KEY REFERENCES projects.git_sync(id) ON DELETE CASCADE,
  owner UUID NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL
);
