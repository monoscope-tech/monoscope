CREATE INDEX slack_incident_roots_thread_lookup
  ON apis.slack_incident_roots (team_id, channel_id, message_ts)
  WHERE message_ts IS NOT NULL;
