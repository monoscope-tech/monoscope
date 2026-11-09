BEGIN;
-- The setup script (start the app, migrate, seed) behind a repository's last successful proof.
-- The next review gets these exact bytes back, so proofs reuse code rather than rediscover it.
CREATE TABLE projects.pr_review_proof_recipes (
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  owner text NOT NULL,
  repo text NOT NULL,
  setup text NOT NULL,
  revision text NOT NULL,
  run_id uuid NOT NULL REFERENCES projects.pr_review_runs(id) ON DELETE CASCADE,
  updated_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (project_id, owner, repo)
);
COMMIT;
