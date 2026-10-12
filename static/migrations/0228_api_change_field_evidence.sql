-- Preserve observations before the learned schema is merged again.
ALTER TABLE apis.anomalies
  ADD COLUMN IF NOT EXISTS field_evidence jsonb NOT NULL DEFAULT '[]'::jsonb;
