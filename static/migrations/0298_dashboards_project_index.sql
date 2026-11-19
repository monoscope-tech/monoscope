-- Every page's nav lists the project's dashboards, starred first.
CREATE INDEX IF NOT EXISTS idx_dashboards_project_starred ON projects.dashboards (project_id, starred_since DESC NULLS LAST);
