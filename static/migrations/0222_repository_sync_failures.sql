-- Retain legacy failures as import failures; retries previously always imported.
ALTER TABLE projects.git_sync ALTER COLUMN last_error TYPE JSONB
USING CASE WHEN last_error IS NULL THEN NULL ELSE jsonb_build_object(
  'operation', jsonb_build_object('tag', 'ImportDashboards'), 'message', last_error
) END;
