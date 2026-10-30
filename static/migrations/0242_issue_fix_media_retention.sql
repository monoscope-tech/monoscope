ALTER TABLE projects.issue_fix_jobs ADD COLUMN media_retained_at timestamptz;
ALTER TABLE projects.issue_fix_jobs ADD CONSTRAINT issue_fix_retained_media_requires_explanation
  CHECK (media_retained_at IS NULL OR (result IS NOT NULL AND explanation IS NOT NULL));
