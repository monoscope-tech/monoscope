-- Session/user partial indexes for the Sessions visualization.
-- idx_logs_and_spans_user_id is pre-created for upcoming user-scoped views.

CREATE INDEX IF NOT EXISTS idx_logs_and_spans_session_id
  ON otel_logs_and_spans (project_id, attributes___session___id, timestamp DESC)
  WHERE attributes___session___id IS NOT NULL
    AND attributes___session___id <> '';

CREATE INDEX IF NOT EXISTS idx_logs_and_spans_user_id
  ON otel_logs_and_spans (project_id, attributes___user___id, timestamp DESC)
  WHERE attributes___user___id IS NOT NULL
    AND attributes___user___id <> '';
