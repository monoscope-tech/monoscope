BEGIN;

ALTER TABLE projects.ai_usage
  ADD CONSTRAINT ai_usage_source
  CHECK (source IN ('interactive', 'routine', 'slack_investigation', 'conversation_title'));

ALTER TABLE apis.ai_routine_runs
  ADD COLUMN project_id UUID REFERENCES projects.projects(id) ON DELETE CASCADE,
  ADD COLUMN conversation_id UUID;

UPDATE apis.ai_routine_runs run
SET project_id = routine.project_id,
    conversation_id = routine.conversation_id
FROM apis.ai_routines routine
WHERE routine.id = run.routine_id;

ALTER TABLE apis.ai_routine_runs
  ALTER COLUMN project_id SET NOT NULL,
  ALTER COLUMN conversation_id SET NOT NULL,
  ALTER COLUMN routine_id DROP NOT NULL,
  DROP CONSTRAINT ai_routine_runs_routine_id_fkey,
  ADD CONSTRAINT ai_routine_runs_routine_id_fkey
    FOREIGN KEY (routine_id) REFERENCES apis.ai_routines(id) ON DELETE SET NULL;

CREATE INDEX ai_routine_runs_project_recent
  ON apis.ai_routine_runs (project_id, scheduled_at DESC);

COMMIT;
