BEGIN;

-- Backfill target_hash from endpoint_hash for existing issues
-- Without this, the unique index on (project_id, target_hash, issue_type)
-- would fail if any project has >1 open issue of the same type with target_hash = ''
UPDATE apis.issues SET target_hash = endpoint_hash WHERE target_hash = '';

-- Add source_field to log_pattern_hourly_stats
-- log_patterns is unique on (project_id, source_field, pattern_hash), so two patterns
-- from different source_fields (e.g. summary and url_path) can share the same hash.
-- Without source_field in the PK, their hourly counts would merge incorrectly.
ALTER TABLE apis.log_pattern_hourly_stats ADD COLUMN IF NOT EXISTS source_field TEXT NOT NULL DEFAULT 'summary';

-- Recreate PK with source_field included
ALTER TABLE apis.log_pattern_hourly_stats DROP CONSTRAINT log_pattern_hourly_stats_pkey;
ALTER TABLE apis.log_pattern_hourly_stats ADD PRIMARY KEY (project_id, source_field, pattern_hash, hour_bucket);

-- Recreate index to include source_field for efficient per-project queries
DROP INDEX IF EXISTS idx_lp_hourly_stats_project_hour;
CREATE INDEX idx_lp_hourly_stats_project_hour
  ON apis.log_pattern_hourly_stats(project_id, source_field, hour_bucket DESC);

COMMIT;
