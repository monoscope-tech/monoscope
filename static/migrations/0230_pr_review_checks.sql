BEGIN;
CREATE TABLE projects.pr_review_checks (
  id uuid PRIMARY KEY,
  run_id uuid NOT NULL REFERENCES projects.pr_review_runs(id) ON DELETE CASCADE,
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  billing_provider text NOT NULL,
  customer_id text,
  receipt jsonb NOT NULL CHECK (jsonb_typeof(receipt) = 'object'),
  created_at timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX pr_review_checks_project_run ON projects.pr_review_checks (project_id, run_id);
COMMIT;
