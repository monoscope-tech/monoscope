BEGIN;

ALTER TABLE projects.dashboards ADD COLUMN saved_views jsonb[] NOT NULL DEFAULT '{}';

-- Segments could be saved before a project had opened its RUM dashboard.
INSERT INTO projects.dashboards (project_id, created_by, base_template, title, teams)
SELECT DISTINCT s.project_id, '00000000-0000-0000-0000-000000000000'::uuid, 'rum.yaml', 'RUM',
  ARRAY(SELECT id FROM projects.teams WHERE project_id = s.project_id AND is_everyone AND deleted_at IS NULL)
FROM projects.rum_segments s
WHERE NOT EXISTS (SELECT 1 FROM projects.dashboards d WHERE d.project_id = s.project_id AND d.base_template = 'rum.yaml');

UPDATE projects.dashboards d SET saved_views = s.views
FROM (
  SELECT project_id, array_agg(jsonb_build_object(
    'name', name,
    'variables', jsonb_strip_nulls(filters - 'errored') || jsonb_build_object('errors', COALESCE(filters->>'errored', 'false'))
  ) ORDER BY name) AS views
  FROM projects.rum_segments GROUP BY project_id
) s
WHERE d.project_id = s.project_id AND d.base_template = 'rum.yaml';

DROP TABLE projects.rum_segments;

COMMIT;
