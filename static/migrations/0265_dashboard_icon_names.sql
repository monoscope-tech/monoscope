-- Templates named three icons no sprite has (smile, cpu, layers), which rendered as blank
-- space. Dashboards created from them keep their own copy of the names. A JSON string can't
-- contain this unescaped text, so the match only ever hits an "icon" field's value.
UPDATE projects.dashboards
SET schema = replace(replace(replace(schema::text,
  '"icon": "smile"', '"icon": "face-smile"'),
  '"icon": "cpu"', '"icon": "microchip"'),
  '"icon": "layers"', '"icon": "layer-group"')::jsonb
WHERE schema::text ~ '"icon": "(smile|cpu|layers)"';
