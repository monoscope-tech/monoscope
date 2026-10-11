-- Keep the newest dashboard when legacy paths name the same repository file.
WITH normalized AS (
  SELECT d.id, d.git_sync_id, d.updated_at,
         CASE
           WHEN starts_with(d.file_path, p.prefix) THEN substr(d.file_path, length(p.prefix) + 1)
           WHEN starts_with(d.file_path, 'dashboards/') THEN substr(d.file_path, 12)
           ELSE d.file_path
         END AS path
  FROM projects.dashboards d JOIN projects.git_sync s ON s.id = d.git_sync_id
  CROSS JOIN LATERAL (
    SELECT CASE WHEN s.path_prefix = '' THEN 'dashboards/' ELSE s.path_prefix || '/dashboards/' END AS prefix
  ) p
  WHERE d.file_path IS NOT NULL
), ranked AS (
  SELECT id, row_number() OVER (PARTITION BY git_sync_id, path ORDER BY updated_at DESC, id) AS rank
  FROM normalized
)
UPDATE projects.dashboards d
SET git_sync_id = NULL, file_path = NULL, file_sha = NULL
FROM ranked r WHERE d.id = r.id AND r.rank > 1;

-- Detach collisions first so the unique index is safe in any update order.
UPDATE projects.dashboards d
SET file_path = CASE
  WHEN starts_with(d.file_path, p.prefix) THEN substr(d.file_path, length(p.prefix) + 1)
  WHEN starts_with(d.file_path, 'dashboards/') THEN substr(d.file_path, 12)
  ELSE d.file_path
END
FROM projects.git_sync s
CROSS JOIN LATERAL (
  SELECT CASE WHEN s.path_prefix = '' THEN 'dashboards/' ELSE s.path_prefix || '/dashboards/' END AS prefix
) p
WHERE d.git_sync_id = s.id AND d.file_path IS NOT NULL;
