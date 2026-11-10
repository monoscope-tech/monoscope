BEGIN;
-- A review's live comment: the claims its agent plans to prove and how far each has got, and the
-- comment that shows them until the finished review replaces it.
ALTER TABLE projects.pr_review_runs
  ADD COLUMN proof_progress jsonb NOT NULL DEFAULT '[]' CHECK (jsonb_typeof(proof_progress) = 'array'),
  ADD COLUMN progress_comment_id bigint;
COMMIT;
