ALTER TABLE apis.slack_incident_roots
  ADD COLUMN history_cursor TEXT CHECK (history_cursor IS NULL OR history_cursor <> ''),
  ADD COLUMN history_retry_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  ADD COLUMN history_lease_token UUID;

CREATE INDEX slack_root_history_due ON apis.slack_incident_roots(history_retry_at)
  WHERE message_ts IS NULL;
