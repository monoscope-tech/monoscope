ALTER TABLE apis.slack_investigation_progress
  ADD COLUMN stream_state JSONB CHECK (stream_state IS NULL OR jsonb_typeof(stream_state) = 'object');
