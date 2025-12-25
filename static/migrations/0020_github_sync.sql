-- GitHub sync configuration per project
CREATE TABLE projects.github_sync (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  owner TEXT NOT NULL,
  repo TEXT NOT NULL,
  branch TEXT NOT NULL DEFAULT 'main',
  access_token TEXT NOT NULL,
  webhook_secret TEXT,
  last_tree_sha TEXT,
  sync_enabled BOOLEAN NOT NULL DEFAULT true,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(project_id)
);

CREATE INDEX idx_github_sync_project_id ON projects.github_sync(project_id);
CREATE INDEX idx_github_sync_owner_repo ON projects.github_sync(owner, repo);

-- Add file path and content hash columns to dashboards
-- file_path: canonical path like "folder/dashboard-title.yaml"
-- file_sha: SHA256 hash of the YAML content for change detection
ALTER TABLE projects.dashboards ADD COLUMN file_path TEXT;
ALTER TABLE projects.dashboards ADD COLUMN file_sha TEXT;

CREATE INDEX idx_dashboards_file_path ON projects.dashboards(project_id, file_path) WHERE file_path IS NOT NULL;
