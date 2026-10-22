ALTER TABLE projects.issue_fix_jobs ADD COLUMN verified_result jsonb;
ALTER TABLE projects.issue_fix_jobs ADD CONSTRAINT issue_fix_checkpoint_requires_investigation
  CHECK (verified_result IS NULL OR investigation IS NOT NULL);
