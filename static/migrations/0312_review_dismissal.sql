-- A member's @monoscope dismiss sets a finished review's proof aside; its comment is redrawn from the
-- stored result and the full review's address, kept when the run publishes.
ALTER TABLE projects.pr_review_runs ADD COLUMN IF NOT EXISTS dismissed_by text, ADD COLUMN IF NOT EXISTS review_url text;
