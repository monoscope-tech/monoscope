-- Telemetry is written to TimeFusion only, so the SafetyNetReprocess job (which re-drove
-- Postgres-written spans the eager track missed) is gone; its queued rows no longer decode.
DELETE FROM background_jobs WHERE payload->>'tag' = 'SafetyNetReprocess';
