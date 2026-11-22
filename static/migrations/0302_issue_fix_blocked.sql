-- A fix agent the workflow stopped before a verified candidate records its root cause and blocker instead of failing.
ALTER TABLE projects.issue_fix_jobs ADD COLUMN IF NOT EXISTS blocked JSONB;
ALTER TABLE projects.issue_fix_jobs DROP CONSTRAINT IF EXISTS issue_fix_blocked_is_terminal;
ALTER TABLE projects.issue_fix_jobs ADD CONSTRAINT issue_fix_blocked_is_terminal
  CHECK (blocked IS NULL OR (jsonb_typeof(blocked) = 'object' AND result IS NULL AND failure IS NULL));
