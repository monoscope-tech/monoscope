BEGIN;
CREATE TABLE projects.issue_fix_runs (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  issue_id uuid NOT NULL REFERENCES apis.issues(id) ON DELETE CASCADE,
  setup jsonb NOT NULL CHECK (jsonb_typeof(setup) = 'object'),
  billing_provider text NOT NULL,
  customer_id text,
  version bigint NOT NULL DEFAULT 0 CHECK (version >= 0),
  created_at timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX issue_fix_runs_project_issue ON projects.issue_fix_runs (project_id, issue_id, created_at DESC);
CREATE TABLE projects.issue_fix_steps (
  id uuid PRIMARY KEY,
  run_id uuid NOT NULL REFERENCES projects.issue_fix_runs(id) ON DELETE CASCADE,
  version bigint NOT NULL CHECK (version > 0),
  step jsonb NOT NULL CHECK (jsonb_typeof(step) = 'object'),
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE (run_id, version)
);
COMMIT;
