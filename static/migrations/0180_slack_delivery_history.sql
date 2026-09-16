ALTER TABLE apis.slack_incident_deliveries
  ADD COLUMN history_cursor TEXT CHECK (history_cursor IS NULL OR history_cursor <> ''),
  ADD COLUMN history_retry_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  ADD COLUMN history_lease_token UUID;

-- Preserve ongoing root pagination. Keep the old columns for rolling workers.
UPDATE apis.slack_incident_deliveries d
SET history_cursor = r.history_cursor, history_retry_at = r.history_retry_at
FROM apis.slack_incident_roots r
WHERE d.root_id = r.id AND d.operation = 'post_root';

CREATE INDEX slack_delivery_history_due ON apis.slack_incident_deliveries(history_retry_at)
  WHERE state IN ('uncertain', 'waiting_root', 'sending');
