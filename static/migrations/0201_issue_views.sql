-- Saved issue-list views: a named query string (tab, sort, period, type/service filters).
CREATE TABLE IF NOT EXISTS apis.issue_views (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  query TEXT NOT NULL,
  created_by UUID REFERENCES users.users(id) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS idx_issue_views_project ON apis.issue_views (project_id, created_at);
