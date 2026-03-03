DELETE FROM background_jobs WHERE status = 'queued';

CREATE INDEX IF NOT EXISTS idx_issues_project_apichange_created
ON apis.issues(project_id, created_at DESC)
WHERE issue_type = 'api_change';

CREATE INDEX IF NOT EXISTS idx_error_patterns_new_recent
ON apis.error_patterns(project_id, created_at DESC)
WHERE last_notified_at IS NULL AND state != 'resolved';
