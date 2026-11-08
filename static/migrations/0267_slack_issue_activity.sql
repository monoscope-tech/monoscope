-- Issue lifecycle actions (acknowledge, assign, comment, ...) reply in the issue's
-- existing Slack threads. Each reply is one delivery per (root, activity).
ALTER TABLE apis.slack_incident_deliveries
  ADD COLUMN activity_id BIGINT REFERENCES apis.issue_activity_log(id) ON DELETE CASCADE,
  DROP CONSTRAINT slack_incident_deliveries_operation_check,
  ADD CONSTRAINT slack_incident_deliveries_operation_check
    CHECK (operation IN ('post_root', 'post_reply', 'update_root', 'post_analysis', 'post_activity')),
  ADD CONSTRAINT slack_incident_activity CHECK ((operation = 'post_activity') = (activity_id IS NOT NULL)),
  DROP CONSTRAINT slack_incident_deliveries_root_id_event_id_operation_key,
  ADD CONSTRAINT slack_incident_delivery_identity UNIQUE NULLS NOT DISTINCT (root_id, event_id, operation, activity_id);
