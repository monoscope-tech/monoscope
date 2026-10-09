-- Repair the legacy JSON key in saved widgets, including tabs and nested groups.
CREATE FUNCTION migrate_widget_warning_threshold(value jsonb) RETURNS jsonb
LANGUAGE plpgsql AS $$
BEGIN
  IF jsonb_typeof(value) = 'array' THEN
    RETURN COALESCE((SELECT jsonb_agg(migrate_widget_warning_threshold(item) ORDER BY ordinal)
      FROM jsonb_array_elements(value) WITH ORDINALITY AS items(item, ordinal)), '[]'::jsonb);
  ELSIF jsonb_typeof(value) = 'object' THEN
    IF value ? 'arning_threshold' THEN
      IF NOT value ? 'warning_threshold' THEN
        value := value || jsonb_build_object('warning_threshold', value -> 'arning_threshold');
      END IF;
      value := value - 'arning_threshold';
    END IF;
    RETURN COALESCE((SELECT jsonb_object_agg(key, migrate_widget_warning_threshold(val))
      FROM jsonb_each(value) AS fields(key, val)), '{}'::jsonb);
  END IF;
  RETURN value;
END;
$$;

UPDATE projects.dashboards SET schema = migrate_widget_warning_threshold(schema)
WHERE schema::text LIKE '%"arning_threshold"%';

DROP FUNCTION migrate_widget_warning_threshold(jsonb);
