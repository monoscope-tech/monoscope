-- When a setup-only run last tried the repository's setup: a setup short of per-role logins is retried
-- once per version, so a repository that serves no app is not retried daily.
ALTER TABLE projects.pr_review_proof_recipes ADD COLUMN setup_tried_at timestamptz;
