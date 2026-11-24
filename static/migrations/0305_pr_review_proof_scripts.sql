-- What each proof ran (claim, command, setup), so a review of a later push can replay it.
ALTER TABLE projects.pr_review_proofs ADD COLUMN IF NOT EXISTS script jsonb;
