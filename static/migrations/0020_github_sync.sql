-- GitHub sync configuration per project
-- Supports both GitHub App (installation_id) and PAT (access_token) auth
CREATE TABLE IF NOT EXISTS projects.github_sync (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  owner TEXT NOT NULL,
  repo TEXT NOT NULL,
  branch TEXT NOT NULL DEFAULT 'main',
  access_token TEXT,                              -- Encrypted PAT (optional if using GitHub App)
  installation_id BIGINT,                         -- GitHub App installation ID
  path_prefix TEXT NOT NULL DEFAULT '',           -- Optional folder prefix for dashboards
  webhook_secret TEXT,
  last_tree_sha TEXT,
  sync_enabled BOOLEAN NOT NULL DEFAULT true,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(project_id),
  -- At least one auth method must be configured
  CONSTRAINT github_sync_auth_check CHECK (installation_id IS NOT NULL OR access_token IS NOT NULL)
);

CREATE INDEX IF NOT EXISTS idx_github_sync_project_id ON projects.github_sync(project_id);
CREATE INDEX IF NOT EXISTS idx_github_sync_owner_repo ON projects.github_sync(owner, repo);
CREATE INDEX IF NOT EXISTS idx_github_sync_installation ON projects.github_sync(installation_id) WHERE installation_id IS NOT NULL;

-- Add file path and content hash columns to dashboards
-- file_path: canonical path like "folder/dashboard-title.yaml"
-- file_sha: SHA256 hash of the YAML content for change detection
ALTER TABLE projects.dashboards
  ADD COLUMN IF NOT EXISTS file_path TEXT,
  ADD COLUMN IF NOT EXISTS file_sha TEXT;

CREATE INDEX IF NOT EXISTS idx_dashboards_file_path ON projects.dashboards(project_id, file_path) WHERE file_path IS NOT NULL;
