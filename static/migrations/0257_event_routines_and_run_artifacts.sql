BEGIN;

ALTER TABLE apis.ai_routines
  ALTER COLUMN interval_minutes DROP NOT NULL,
  ADD COLUMN instructions TEXT,
  DROP CONSTRAINT ai_routine_schedule_kind,
  DROP CONSTRAINT ai_routine_schedule_shape,
  ADD CONSTRAINT ai_routine_schedule_kind CHECK
    (schedule_kind IN ('interval', 'daily', 'weekdays', 'weekly', 'slack_message', 'pull_request')),
  ADD CONSTRAINT ai_routine_schedule_shape CHECK (
    (schedule_kind = 'interval' AND interval_minutes IS NOT NULL AND schedule_hour IS NULL AND schedule_minute IS NULL AND schedule_weekday IS NULL)
    OR (schedule_kind IN ('daily', 'weekdays') AND interval_minutes IS NOT NULL AND schedule_hour IS NOT NULL AND schedule_minute IS NOT NULL AND schedule_weekday IS NULL)
    OR (schedule_kind = 'weekly' AND interval_minutes IS NOT NULL AND schedule_hour IS NOT NULL AND schedule_minute IS NOT NULL AND schedule_weekday IS NOT NULL)
    OR (schedule_kind IN ('slack_message', 'pull_request') AND interval_minutes IS NULL AND next_run_at IS NULL
        AND schedule_hour IS NULL AND schedule_minute IS NULL AND schedule_weekday IS NULL
        AND instructions IS NOT NULL AND length(btrim(instructions)) > 0 AND template_key IS NOT NULL AND template_version IS NOT NULL)
  );

ALTER TABLE apis.ai_routine_runs
  ADD COLUMN source JSONB NOT NULL DEFAULT '{"tag":"ScheduledRun"}',
  ADD COLUMN attempt BIGINT NOT NULL DEFAULT 1 CHECK (attempt > 0),
  ADD COLUMN requested_by UUID REFERENCES users.users(id) ON DELETE SET NULL,
  ADD COLUMN input TEXT,
  ADD COLUMN instructions TEXT,
  ADD COLUMN template_version BIGINT,
  DROP CONSTRAINT ai_routine_runs_routine_id_scheduled_at_key;

CREATE UNIQUE INDEX ai_routine_runs_scheduled_once
  ON apis.ai_routine_runs (routine_id, scheduled_at) WHERE source = '{"tag":"ScheduledRun"}';
CREATE UNIQUE INDEX ai_routine_runs_event_attempt
  ON apis.ai_routine_runs (routine_id, source, attempt) WHERE source <> '{"tag":"ScheduledRun"}';

CREATE TABLE apis.ai_routine_run_artifacts (
  run_id UUID NOT NULL REFERENCES apis.ai_routine_runs(id) ON DELETE CASCADE,
  key TEXT NOT NULL,
  title TEXT NOT NULL,
  content TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (run_id, key)
);

COMMIT;
