-- Drop the legacy schema-derivation tables, replaced by the in-memory
-- schema-learning catalog (apis.schema_template / apis.schema_catalog /
-- apis.schema_summary). The new pipeline (Pkg.SchemaLearning) writes only
-- to those tables; nothing remaining inserts into the dropped ones.
--
-- Triggers fired by these tables fan out into apis.anomalies + background
-- jobs; their replacement (per-flush diffing in
-- Pkg.SchemaLearning.Worker.flushDirty) is wired in the same branch.

-- Triggers + procedure first (FK / dependency order).
DROP TRIGGER IF EXISTS endpoint_created_anomaly  ON apis.endpoints;
DROP TRIGGER IF EXISTS shapes_created_anomaly    ON apis.shapes;
DROP TRIGGER IF EXISTS fields_created_anomaly    ON apis.fields;
DROP TRIGGER IF EXISTS format_created_anomaly    ON apis.formats;

-- Tables.
DROP TABLE IF EXISTS apis.facet_summaries CASCADE;
DROP TABLE IF EXISTS apis.formats         CASCADE;
DROP TABLE IF EXISTS apis.fields          CASCADE;
DROP TABLE IF EXISTS apis.shapes          CASCADE;

-- Enums (only safe to drop after their consumer tables are gone).
DROP TYPE IF EXISTS apis.field_category   CASCADE;
DROP TYPE IF EXISTS apis.field_type       CASCADE;
