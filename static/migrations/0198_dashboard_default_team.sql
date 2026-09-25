UPDATE projects.dashboards d
SET teams = ARRAY[t.id]
FROM projects.teams t
WHERE t.project_id = d.project_id AND t.is_everyone AND t.deleted_at IS NULL
  AND cardinality(d.teams) = 0;
