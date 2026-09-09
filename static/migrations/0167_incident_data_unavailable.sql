ALTER TABLE apis.incident_events DROP CONSTRAINT incident_events_event_kind_check;
ALTER TABLE apis.incident_events ADD CONSTRAINT incident_events_event_kind_check
  CHECK (event_kind IN ('alert', 'observation', 'reminder', 'data_unavailable', 'recovered', 'resolved'));
