BEGIN;

-- A reply that also lands in the channel: the first message of a day in a thread continued across episodes.
ALTER TABLE apis.slack_incident_deliveries ADD COLUMN IF NOT EXISTS reply_broadcast BOOLEAN NOT NULL DEFAULT FALSE;

-- Query-alert issues keyed on the bare monitor id rather than a derived-Show blob.
UPDATE apis.issues
SET target_hash = substring(target_hash FROM '([0-9a-f-]{36})'),
    endpoint_hash = CASE WHEN endpoint_hash = target_hash THEN substring(target_hash FROM '([0-9a-f-]{36})') ELSE endpoint_hash END,
    issue_data = CASE WHEN issue_data ? 'query_id'
      THEN jsonb_set(issue_data, '{query_id}', to_jsonb(substring(issue_data->>'query_id' FROM '([0-9a-f-]{36})')))
      ELSE issue_data END
WHERE issue_type = 'query_alert' AND target_hash LIKE 'QueryMonitorId {unQueryMonitorId = %}';

COMMIT;
