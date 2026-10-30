BEGIN;
-- Internal model-cost estimates. These rows do not enter billable AI usage.
CREATE TABLE projects.pr_review_usage (
  id uuid PRIMARY KEY,
  run_id uuid NOT NULL REFERENCES projects.pr_review_runs(id) ON DELETE CASCADE,
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  billing_provider text NOT NULL,
  customer_id text,
  receipt jsonb NOT NULL CHECK (jsonb_typeof(receipt) = 'object'),
  cost_nanousd numeric(20, 0) CHECK (cost_nanousd >= 0),
  created_at timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX pr_review_usage_project_run ON projects.pr_review_usage (project_id, run_id);
CREATE INDEX pr_review_usage_customer ON projects.pr_review_usage (billing_provider, customer_id, created_at)
  WHERE customer_id IS NOT NULL;
COMMIT;
