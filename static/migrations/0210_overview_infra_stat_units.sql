-- 0209 repaired the SQL but could miss units when JSONB placed another field
-- between icon/type/title. Update the saved widget objects by their fields.
CREATE FUNCTION repair_overview_stat_units(value jsonb) RETURNS jsonb
LANGUAGE plpgsql AS $$
BEGIN
  IF jsonb_typeof(value) = 'array' THEN
    RETURN COALESCE((SELECT jsonb_agg(repair_overview_stat_units(item) ORDER BY ordinal)
      FROM jsonb_array_elements(value) WITH ORDINALITY AS items(item, ordinal)), '[]'::jsonb);
  ELSIF jsonb_typeof(value) = 'object' THEN
    IF value ->> 'type' = 'stat' AND value ->> 'title' IN ('Avg CPU Utilization', 'Avg Memory Usage') THEN
      value := value || '{"unit":"%"}'::jsonb;
    END IF;
    RETURN COALESCE((SELECT jsonb_object_agg(key, repair_overview_stat_units(val))
      FROM jsonb_each(value) AS fields(key, val)), '{}'::jsonb);
  END IF;
  RETURN value;
END;
$$;

UPDATE projects.dashboards SET schema = repair_overview_stat_units(schema)
WHERE base_template = '_overview.yaml'
  AND (schema::text LIKE '%Avg CPU Utilization%' OR schema::text LIKE '%Avg Memory Usage%');

DROP FUNCTION repair_overview_stat_units(jsonb);
