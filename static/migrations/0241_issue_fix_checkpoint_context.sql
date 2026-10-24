BEGIN;
ALTER TABLE projects.issue_fix_jobs ADD COLUMN checkpoint_investigation jsonb
  CHECK (jsonb_typeof(checkpoint_investigation) = 'object');
UPDATE projects.issue_fix_jobs SET checkpoint_investigation = investigation WHERE verified_result IS NOT NULL;
ALTER TABLE projects.issue_fix_jobs DROP CONSTRAINT issue_fix_checkpoint_requires_investigation;
ALTER TABLE projects.issue_fix_jobs ADD CONSTRAINT issue_fix_checkpoint_pair
  CHECK ((verified_result IS NULL) = (checkpoint_investigation IS NULL));
COMMIT;
