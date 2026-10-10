ALTER TABLE projects.git_sync DROP CONSTRAINT IF EXISTS github_sync_project_id_key;
ALTER TABLE projects.git_sync DROP CONSTRAINT IF EXISTS git_sync_project_id_key;
ALTER TABLE projects.git_sync
  ADD CONSTRAINT git_sync_repository_key UNIQUE NULLS NOT DISTINCT (project_id, host, api_base, owner, repo),
  ADD CONSTRAINT git_sync_project_id_id_key UNIQUE (project_id, id),
  ADD COLUMN last_error TEXT,
  ADD COLUMN last_synced_at TIMESTAMPTZ;

ALTER TABLE projects.dashboards
  ADD COLUMN git_sync_id UUID,
  ADD CONSTRAINT dashboards_git_sync_project_fk
    FOREIGN KEY (project_id, git_sync_id) REFERENCES projects.git_sync (project_id, id)
    ON DELETE SET NULL (git_sync_id);

-- Retain duplicate legacy paths as local copies instead of deleting dashboards.
WITH sources AS (
  SELECT d.id, s.id AS sync_id,
         row_number() OVER (PARTITION BY d.project_id, d.file_path ORDER BY d.updated_at DESC, d.id) AS rank
  FROM projects.dashboards d JOIN projects.git_sync s ON s.project_id = d.project_id
  WHERE d.file_path IS NOT NULL AND d.file_sha IS NOT NULL
)
UPDATE projects.dashboards d
SET git_sync_id = CASE WHEN s.rank = 1 THEN s.sync_id END,
    file_path = CASE WHEN s.rank = 1 THEN d.file_path END,
    file_sha = CASE WHEN s.rank = 1 THEN d.file_sha END
FROM sources s WHERE d.id = s.id;

CREATE UNIQUE INDEX dashboards_repository_file_key
  ON projects.dashboards (git_sync_id, file_path) WHERE git_sync_id IS NOT NULL;
