ALTER TABLE apis.slack_user_credentials
  ADD COLUMN refresh_state JSONB NOT NULL DEFAULT '{"tag":"RefreshIdle"}'::jsonb,
  ADD CONSTRAINT slack_refresh_state CHECK (COALESCE(
    jsonb_typeof(refresh_state) = 'object'
    AND (
      refresh_state = '{"tag":"RefreshIdle"}'::jsonb
      OR (refresh_state->>'tag' IN ('RefreshClaimed', 'RefreshUncertain')
        AND jsonb_typeof(refresh_state->'contents') = 'array'
        AND jsonb_array_length(refresh_state->'contents') = 2
        AND jsonb_typeof(refresh_state->'contents'->0) = 'string'
        AND jsonb_typeof(refresh_state->'contents'->1) = 'string')
    )
  , FALSE));
