-- Secured dashboard SQL now carries its owning store. Existing schemas stored SQL
-- as a bare string, so migrate that wire shape before the application decodes it.
-- All historical secured SQL was routed to TimeFusion; the two Endpoint Analytics
-- variables are the known exception because apis.endpoints lives in Postgres.
UPDATE projects.dashboards AS dashboard
SET schema = jsonb_set(
  dashboard.schema,
  '{variables}',
  (
    SELECT jsonb_agg(
      CASE
        WHEN jsonb_typeof(variable -> 'sql') = 'string' THEN
          jsonb_set(
            variable,
            '{sql}',
            jsonb_build_object(
              'source',
              CASE
                WHEN dashboard.base_template = 'endpoint-stats.yaml'
                  AND variable ->> 'key' IN ('host', 'endpointHash')
                  THEN 'postgres'
                ELSE 'timefusion'
              END,
              'statement', variable ->> 'sql'
            )
          )
        ELSE variable
      END
      ORDER BY ordinal
    )
    FROM jsonb_array_elements(dashboard.schema -> 'variables')
      WITH ORDINALITY AS variables(variable, ordinal)
  )
)
WHERE jsonb_typeof(dashboard.schema -> 'variables') = 'array';

UPDATE projects.dashboards AS dashboard
SET schema = jsonb_set(
  dashboard.schema,
  '{constants}',
  (
    SELECT jsonb_agg(
      CASE
        WHEN jsonb_typeof(constant -> 'sql') = 'string' THEN
          jsonb_set(
            constant,
            '{sql}',
            jsonb_build_object(
              'source', 'timefusion',
              'statement', constant ->> 'sql'
            )
          )
        ELSE constant
      END
      ORDER BY ordinal
    )
    FROM jsonb_array_elements(dashboard.schema -> 'constants')
      WITH ORDINALITY AS constants(constant, ordinal)
  )
)
WHERE jsonb_typeof(dashboard.schema -> 'constants') = 'array';
