ALTER TABLE apis.incident_episodes
  DROP CONSTRAINT incident_episodes_source_kind_check,
  ADD CONSTRAINT incident_episodes_source_kind_check CHECK (source_kind IN ('monitor', 'issue', 'error'));

ALTER TABLE apis.incident_events
  DROP CONSTRAINT incident_events_source_kind_check,
  ADD CONSTRAINT incident_events_source_kind_check CHECK (source_kind IN ('monitor', 'issue', 'error'));
