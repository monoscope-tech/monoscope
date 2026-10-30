CREATE INDEX pr_review_usage_attempt ON projects.pr_review_usage (attempt_id) WHERE attempt_id IS NOT NULL;
CREATE INDEX pr_review_checks_attempt ON projects.pr_review_checks (attempt_id) WHERE attempt_id IS NOT NULL;
