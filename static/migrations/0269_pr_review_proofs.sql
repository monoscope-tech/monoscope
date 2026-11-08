BEGIN;
-- Recorded proof media for a review: one row per claim recording and revision side.
CREATE TABLE projects.pr_review_proofs (
  run_id uuid NOT NULL REFERENCES projects.pr_review_runs(id) ON DELETE CASCADE,
  key text NOT NULL CHECK (key ~ '^proof-[1-9][0-9]*$'),
  side text NOT NULL CHECK (side IN ('base', 'head')),
  claim text NOT NULL,
  revision text NOT NULL,
  exit_code integer,
  cast_bytes bytea NOT NULL,
  gif bytea NOT NULL,
  mp4 bytea NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (run_id, key, side)
);
COMMIT;
