BEGIN;
CREATE TABLE projects.pr_review_settings (
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  owner text NOT NULL,
  repo text NOT NULL,
  enabled boolean NOT NULL DEFAULT true,
  include_evidence boolean NOT NULL DEFAULT true,
  PRIMARY KEY (project_id, owner, repo)
);
CREATE TABLE projects.pr_review_threads (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  owner text NOT NULL,
  repo text NOT NULL,
  number integer NOT NULL CHECK (number > 0),
  latest_revision text NOT NULL,
  event_at timestamptz NOT NULL,
  comment_id bigint,
  lease_owner uuid,
  lease_until timestamptz,
  UNIQUE (project_id, owner, repo, number)
);
CREATE TABLE projects.pr_review_runs (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  thread_id uuid NOT NULL REFERENCES projects.pr_review_threads(id) ON DELETE CASCADE,
  revision text NOT NULL CHECK (revision ~ '^[0-9a-f]{40}$'),
  state text NOT NULL DEFAULT 'queued' CHECK (state IN ('queued', 'reviewing', 'completed', 'incomplete', 'superseded')),
  result jsonb,
  error text,
  created_at timestamptz NOT NULL DEFAULT now(),
  finished_at timestamptz,
  UNIQUE (thread_id, revision)
);
CREATE INDEX pr_review_runs_thread_created ON projects.pr_review_runs(thread_id, created_at DESC);
COMMIT;
