-- Field-level drift behind a shape anomaly, written by the schema-learning flush.
-- Replaces the new_unique_fields/updated_field_formats columns that lived on
-- apis.shapes (dropped in 0090), which left API-change issues without detail.
ALTER TABLE apis.anomalies
  ADD COLUMN IF NOT EXISTS new_fields      text[] NOT NULL DEFAULT '{}',
  ADD COLUMN IF NOT EXISTS modified_fields text[] NOT NULL DEFAULT '{}';
