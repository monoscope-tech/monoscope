-- app_now() as an inlinable SQL function (plpgsql is opaque to the planner and runs per
-- row), so it can back hot-path column defaults. Same semantics as migration 0094.
CREATE OR REPLACE FUNCTION app_now() RETURNS TIMESTAMPTZ AS $$
  SELECT COALESCE(NULLIF(current_setting('app.current_time', true), '')::timestamptz, now())
$$ LANGUAGE sql STABLE;

-- Ingestion stamps these rows from the column default; follow the test clock like
-- the 0094/0095 triggers do. Production is unchanged: app_now() falls back to now().
ALTER TABLE apis.endpoints ALTER COLUMN created_at SET DEFAULT app_now();
ALTER TABLE apis.anomalies ALTER COLUMN created_at SET DEFAULT app_now();
