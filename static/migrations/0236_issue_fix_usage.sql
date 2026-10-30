BEGIN;
-- Keep legacy review columns for rolling deployments; fix identities are distinct.
ALTER TABLE projects.pr_review_attempts
  ALTER COLUMN run_id DROP NOT NULL,
  ADD COLUMN fix_run_id uuid REFERENCES projects.issue_fix_runs(id) ON DELETE CASCADE,
  ADD CHECK (num_nonnulls(run_id, fix_run_id) = 1),
  ADD UNIQUE (id, fix_run_id, project_id);
CREATE INDEX pr_review_attempts_project_fix ON projects.pr_review_attempts (project_id, fix_run_id) WHERE fix_run_id IS NOT NULL;
ALTER TABLE projects.pr_review_usage
  ALTER COLUMN run_id DROP NOT NULL,
  ADD COLUMN fix_run_id uuid REFERENCES projects.issue_fix_runs(id) ON DELETE CASCADE,
  ADD CHECK (num_nonnulls(run_id, fix_run_id) = 1),
  ADD CHECK (fix_run_id IS NULL OR attempt_id IS NOT NULL),
  ADD FOREIGN KEY (attempt_id, fix_run_id, project_id) REFERENCES projects.pr_review_attempts (id, fix_run_id, project_id);
ALTER TABLE projects.pr_review_checks
  ALTER COLUMN run_id DROP NOT NULL,
  ADD COLUMN fix_run_id uuid REFERENCES projects.issue_fix_runs(id) ON DELETE CASCADE,
  ADD CHECK (num_nonnulls(run_id, fix_run_id) = 1),
  ADD CHECK (fix_run_id IS NULL OR attempt_id IS NOT NULL),
  ADD FOREIGN KEY (attempt_id, fix_run_id, project_id) REFERENCES projects.pr_review_attempts (id, fix_run_id, project_id);
CREATE INDEX pr_review_usage_project_fix ON projects.pr_review_usage (project_id, fix_run_id) WHERE fix_run_id IS NOT NULL;
CREATE INDEX pr_review_checks_project_fix ON projects.pr_review_checks (project_id, fix_run_id) WHERE fix_run_id IS NOT NULL;
COMMIT;
