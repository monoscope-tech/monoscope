ALTER TABLE projects.issue_fix_jobs ADD COLUMN explanation jsonb;
ALTER TABLE projects.issue_fix_jobs ADD CONSTRAINT issue_fix_explanation_requires_result
  CHECK (explanation IS NULL OR result IS NOT NULL);
