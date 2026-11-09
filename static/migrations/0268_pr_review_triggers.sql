BEGIN;
ALTER TABLE projects.pr_review_settings ADD COLUMN trigger text NOT NULL DEFAULT 'ready_pull_requests'
  CHECK (trigger IN ('ready_pull_requests', 'all_pull_requests', 'when_asked'));
UPDATE projects.pr_review_settings SET trigger = 'when_asked' WHERE NOT enabled;
-- `enabled` stays until a later migration: the previous binary still reads it during a rolling deploy.
-- NULL is an automatic run; otherwise the GitHub comment that asked for it.
ALTER TABLE projects.pr_review_runs ADD COLUMN request jsonb;
COMMIT;
