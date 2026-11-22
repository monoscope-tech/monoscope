-- Every page's nav lists the project's recent open issues, newest activity first.
CREATE INDEX IF NOT EXISTS idx_apis_issues_nav ON apis.issues (project_id, updated_at DESC) WHERE archived_at IS NULL;
