-- Tracks which dashboard templates were ever auto-provisioned for a project
-- based on detected metrics. A row here means "already handled": the hourly
-- auto-provision job never creates the same template twice, so a user deleting
-- an auto-created dashboard is a respected opt-out (the marker outlives the
-- dashboard row).
CREATE TABLE IF NOT EXISTS projects.auto_provisioned_dashboards (
  project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
  base_template TEXT NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  PRIMARY KEY (project_id, base_template)
);
