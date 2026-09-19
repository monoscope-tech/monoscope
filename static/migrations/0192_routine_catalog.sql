BEGIN;

ALTER TABLE apis.ai_routines
  ADD COLUMN template_key TEXT,
  ADD COLUMN template_version INTEGER,
  ADD COLUMN schedule_kind TEXT NOT NULL DEFAULT 'interval',
  ADD COLUMN schedule_hour SMALLINT,
  ADD COLUMN schedule_minute SMALLINT,
  ADD COLUMN schedule_weekday SMALLINT,
  ADD COLUMN scope JSONB NOT NULL DEFAULT '{}'::jsonb,
  ADD COLUMN destination TEXT NOT NULL DEFAULT 'conversation',
  ADD COLUMN report_when TEXT NOT NULL DEFAULT 'always',
  ADD COLUMN allow_actions BOOLEAN NOT NULL DEFAULT FALSE,
  ADD COLUMN cancelled_at TIMESTAMPTZ,
  ADD CONSTRAINT ai_routine_schedule_kind CHECK (schedule_kind IN ('interval', 'daily', 'weekdays', 'weekly')),
  ADD CONSTRAINT ai_routine_schedule_hour CHECK (schedule_hour IS NULL OR schedule_hour BETWEEN 0 AND 23),
  ADD CONSTRAINT ai_routine_schedule_minute CHECK (schedule_minute IS NULL OR schedule_minute BETWEEN 0 AND 59),
  ADD CONSTRAINT ai_routine_schedule_weekday CHECK (schedule_weekday IS NULL OR schedule_weekday BETWEEN 1 AND 7),
  ADD CONSTRAINT ai_routine_destination CHECK (destination IN ('conversation', 'slack')),
  ADD CONSTRAINT ai_routine_report_when CHECK (report_when IN ('always', 'findings'));

CREATE UNIQUE INDEX ai_routines_project_template
  ON apis.ai_routines (project_id, template_key)
  WHERE template_key IS NOT NULL;

CREATE TABLE apis.ai_routine_runs (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  routine_id UUID NOT NULL REFERENCES apis.ai_routines(id) ON DELETE CASCADE,
  scheduled_at TIMESTAMPTZ NOT NULL,
  started_at TIMESTAMPTZ NOT NULL,
  finished_at TIMESTAMPTZ,
  status TEXT NOT NULL CHECK (status IN ('running', 'succeeded', 'no_findings', 'failed', 'timed_out', 'cancelled')),
  findings JSONB,
  actions JSONB,
  error TEXT,
  UNIQUE (routine_id, scheduled_at)
);

CREATE INDEX ai_routine_runs_recent
  ON apis.ai_routine_runs (routine_id, scheduled_at DESC);

COMMIT;
