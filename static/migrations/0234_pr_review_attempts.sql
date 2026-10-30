BEGIN;
CREATE TABLE projects.pr_review_attempts (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  run_id uuid NOT NULL REFERENCES projects.pr_review_runs(id) ON DELETE CASCADE,
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  billing_provider text NOT NULL,
  customer_id text,
  provider text NOT NULL,
  model_spec text NOT NULL,
  started_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE (id, run_id, project_id)
);
CREATE INDEX pr_review_attempts_project_run ON projects.pr_review_attempts (project_id, run_id);
ALTER TABLE projects.pr_review_usage ADD COLUMN attempt_id uuid,
  ADD FOREIGN KEY (attempt_id, run_id, project_id) REFERENCES projects.pr_review_attempts (id, run_id, project_id);
ALTER TABLE projects.pr_review_checks ADD COLUMN attempt_id uuid,
  ADD FOREIGN KEY (attempt_id, run_id, project_id) REFERENCES projects.pr_review_attempts (id, run_id, project_id);
COMMIT;
