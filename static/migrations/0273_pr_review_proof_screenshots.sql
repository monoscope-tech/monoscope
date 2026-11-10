BEGIN;
-- A UI proof's media: its preview and video come from a browser recording, with chapters into that
-- video and up to four labelled screenshots per side, shown as merge-base and pull-request stills.
ALTER TABLE projects.pr_review_proofs
  ADD COLUMN browser boolean NOT NULL DEFAULT false,
  ADD COLUMN chapters jsonb NOT NULL DEFAULT '[]' CHECK (jsonb_typeof(chapters) = 'array');
CREATE TABLE projects.pr_review_proof_shots (
  run_id uuid NOT NULL,
  key text NOT NULL,
  side text NOT NULL,
  n smallint NOT NULL CHECK (n BETWEEN 1 AND 4),
  label text NOT NULL CHECK (length(label) <= 80),
  png bytea NOT NULL,
  PRIMARY KEY (run_id, key, side, n),
  FOREIGN KEY (run_id, key, side) REFERENCES projects.pr_review_proofs (run_id, key, side) ON DELETE CASCADE
);
COMMIT;
