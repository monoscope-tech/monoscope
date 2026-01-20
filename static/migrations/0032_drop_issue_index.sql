BEGIN;
  DROP INDEX IF EXISTS apis.idx_issues_unresolved;
  DROP INDEX IF EXISTS apis.idx_issues_unique_open;
  CREATE UNIQUE INDEX issues_project_target_type_open_idx
  ON apis.issues (project_id, target_hash, issue_type)
  WHERE acknowledged_at IS NULL AND archived_at IS NULL;
  ALTER TABLE apis.errors RENAME COLUMN exception_type TO error_type;
  ALTER TABLE apis.errors
  ADD COLUMN first_trace_id TEXT,
  ADD COLUMN recent_trace_id TEXT;
  ALTER TABLE apis.log_patterns ADD COLUMN trace_id TEXT;
COMMIT;