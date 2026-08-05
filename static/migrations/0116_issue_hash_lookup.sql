CREATE INDEX IF NOT EXISTS idx_issues_project_target_updated
ON apis.issues (project_id, target_hash, updated_at DESC, id DESC);
