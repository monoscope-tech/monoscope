-- The overview template's Infra stats selected `::text || '%'` into the numeric stat decoder, so
-- every tile rendered an error. Dashboards that stored a copy of the template keep that SQL; give
-- them the template's fix: numeric results, with '%' carried by the widget unit.
UPDATE projects.dashboards
SET schema = replace(replace(replace(replace(replace(replace(schema::text,
  'ROUND(AVG(value::numeric * 100), 1)::text || ''%''', 'ROUND(AVG(value::numeric * 100), 1)::float8'),
  'ROUND(AVG(value::numeric), 1)::text || ''%''', 'ROUND(AVG(value::numeric), 1)::float8'),
  'COUNT(DISTINCT resource___container___name)::text\nFROM otel_metrics', 'COUNT(DISTINCT resource___container___name)::float8\nFROM otel_metrics'),
  'COUNT(DISTINCT resource___service___name)::text\nFROM otel_metrics', 'COUNT(DISTINCT resource___service___name)::float8\nFROM otel_metrics'),
  '"icon": "cpu", "type": "stat", "title": "Avg CPU Utilization"', '"icon": "cpu", "type": "stat", "unit": "%", "title": "Avg CPU Utilization"'),
  '"icon": "hard-drive", "type": "stat", "title": "Avg Memory Usage"', '"icon": "hard-drive", "type": "stat", "unit": "%", "title": "Avg Memory Usage"')::jsonb
WHERE schema::text LIKE '%)::text || ''\%''%' OR schema::text LIKE '%\_name)::text\nFROM otel\_metrics%';
