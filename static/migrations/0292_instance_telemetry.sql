BEGIN;
-- One row per instance: the random id its anonymous usage reports carry, and the last one sent, so an
-- operator can read exactly what left (docs/telemetry.md).
CREATE TABLE IF NOT EXISTS apis.instance_telemetry (
  singleton BOOLEAN PRIMARY KEY DEFAULT TRUE CHECK (singleton),
  instance_id UUID NOT NULL DEFAULT gen_random_uuid(),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  last_sent_at TIMESTAMPTZ,
  last_payload JSONB
);
INSERT INTO apis.instance_telemetry DEFAULT VALUES ON CONFLICT DO NOTHING;
COMMIT;
