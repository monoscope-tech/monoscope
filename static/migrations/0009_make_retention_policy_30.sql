BEGIN;

-- Remove old policy
SELECT remove_retention_policy('otel_logs_and_spans');

-- Add new 30-day policy
SELECT add_retention_policy('otel_logs_and_spans', INTERVAL '30 days', true);

COMMIT;