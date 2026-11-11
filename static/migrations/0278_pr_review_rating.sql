BEGIN;
-- The PR author's answer to "Did the proof get it right?", from the signed links in the review comment.
ALTER TABLE projects.pr_review_runs
  ADD COLUMN rating text CHECK (rating IN ('up', 'down')),
  ADD COLUMN rated_at timestamptz;
COMMIT;
