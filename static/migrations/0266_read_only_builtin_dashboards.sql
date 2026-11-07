-- Overview and Endpoint Analytics are read-only now: they always render the shipped YAML.
-- A row that froze its own schema keeps that version as an editable copy; the original relinks.
INSERT INTO projects.dashboards (project_id, created_at, created_by, schema, tags, title, teams, saved_views)
SELECT project_id, created_at, created_by, schema, tags, title || ' (Customized)', teams, saved_views
FROM projects.dashboards
WHERE base_template IN ('_overview.yaml', 'endpoint-stats.yaml') AND schema IS NOT NULL;

UPDATE projects.dashboards SET schema = NULL
WHERE base_template IN ('_overview.yaml', 'endpoint-stats.yaml') AND schema IS NOT NULL;
