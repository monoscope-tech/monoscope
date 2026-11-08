-- Issue lifecycle actions (acknowledge, assign, comment, ...) reply in the issue's
-- existing Slack threads, one delivery per (root, activity); an ownership change
-- also queues a root edit, so the channel sees who holds the issue.
ALTER TABLE apis.slack_incident_deliveries
  ADD COLUMN activity_id BIGINT REFERENCES apis.issue_activity_log(id) ON DELETE CASCADE,
  DROP CONSTRAINT slack_incident_deliveries_operation_check,
  ADD CONSTRAINT slack_incident_deliveries_operation_check
    CHECK (operation IN ('post_root', 'post_reply', 'update_root', 'post_analysis', 'post_activity')),
  ADD CONSTRAINT slack_incident_activity CHECK (CASE operation
    WHEN 'post_activity' THEN activity_id IS NOT NULL
    WHEN 'update_root' THEN TRUE
    ELSE activity_id IS NULL END),
  DROP CONSTRAINT slack_incident_deliveries_root_id_event_id_operation_key,
  ADD CONSTRAINT slack_incident_delivery_identity UNIQUE NULLS NOT DISTINCT (root_id, event_id, operation, activity_id);
