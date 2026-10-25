BEGIN;
CREATE TABLE projects.pr_review_knowledge (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  run_id uuid NOT NULL REFERENCES projects.pr_review_runs(id) ON DELETE CASCADE,
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  owner text NOT NULL,
  repo text NOT NULL,
  service text NOT NULL,
  revision text NOT NULL,
  summary text NOT NULL,
  evidence_keys text[] NOT NULL,
  window_start timestamptz NOT NULL,
  window_end timestamptz NOT NULL,
  evidence jsonb NOT NULL CHECK (jsonb_typeof(evidence) = 'array'),
  created_at timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX pr_review_knowledge_scope ON projects.pr_review_knowledge
  (project_id, owner, repo, service, created_at DESC);
COMMIT;
