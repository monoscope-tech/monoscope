BEGIN;
ALTER TABLE projects.issue_fix_jobs
  ADD COLUMN review_attempt_id uuid REFERENCES projects.pr_review_attempts(id),
  ADD COLUMN review_rationale text,
  ADD CONSTRAINT issue_fix_review_pair CHECK (
    (review_attempt_id IS NULL AND review_rationale IS NULL) OR
    (review_attempt_id IS NOT NULL AND review_rationale IS NOT NULL AND verified_result IS NOT NULL
      AND length(btrim(review_rationale)) BETWEEN 1 AND 1200)
  );
COMMIT;
