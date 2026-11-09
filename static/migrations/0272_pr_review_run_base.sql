-- The pull request's base revision when the run was queued, so its workspace build needs no
-- GitHub fetch and one build serves every queued review of that base. Null on older runs.
ALTER TABLE projects.pr_review_runs ADD COLUMN IF NOT EXISTS base text;
