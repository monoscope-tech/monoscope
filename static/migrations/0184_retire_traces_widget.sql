-- Preserve saved aggregate queries when retiring the bespoke trace widget.
-- Recurse through tabs and groups without depending on a dashboard's layout.
CREATE FUNCTION migrate_trace_widgets(value jsonb) RETURNS jsonb
LANGUAGE plpgsql AS $$
DECLARE
  default_columns CONSTANT jsonb := '[
    {"field":"resource_name","title":"Resource"},
    {"field":"span_name","title":"Span name"},
    {"field":"total_time","title":"Duration","column_type":"duration","unit":"ms"}
  ]'::jsonb;
BEGIN
  IF jsonb_typeof(value) = 'array' THEN
    RETURN COALESCE((SELECT jsonb_agg(migrate_trace_widgets(item) ORDER BY ordinal)
      FROM jsonb_array_elements(value) WITH ORDINALITY AS items(item, ordinal)), '[]'::jsonb);
  ELSIF jsonb_typeof(value) = 'object' THEN
    IF value ->> 'type' = 'traces' THEN
      value := jsonb_set(value, '{type}', '"table"'::jsonb);
      IF jsonb_typeof(value -> 'columns') = 'array' THEN
        value := jsonb_set(value, '{columns}', COALESCE((
          SELECT jsonb_agg(col ORDER BY ordinal)
          FROM jsonb_array_elements(value -> 'columns') WITH ORDINALITY AS cols(col, ordinal)
          WHERE col ->> 'field' IS DISTINCT FROM 'latency_breakdown'
        ), default_columns));
      ELSE
        value := jsonb_set(value, '{columns}', default_columns);
      END IF;
    END IF;
    RETURN COALESCE((SELECT jsonb_object_agg(key, migrate_trace_widgets(val))
      FROM jsonb_each(value) AS fields(key, val)), '{}'::jsonb);
  END IF;
  RETURN value;
END;
$$;

UPDATE projects.dashboards SET schema = migrate_trace_widgets(schema)
WHERE schema::text LIKE '%"traces"%';

DROP FUNCTION migrate_trace_widgets(jsonb);
