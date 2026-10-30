BEGIN;

-- Existing attempts keep their checkpoint identity; future recoveries inherit it.
ALTER TABLE apis.ai_routine_runs
  ADD COLUMN root_run_id UUID REFERENCES apis.ai_routine_runs(id) ON DELETE CASCADE;
UPDATE apis.ai_routine_runs SET root_run_id = id;
ALTER TABLE apis.ai_routine_runs ALTER COLUMN root_run_id SET NOT NULL;

CREATE INDEX ai_routine_runs_request_root ON apis.ai_routine_runs(project_id, root_run_id);
CREATE INDEX ai_routine_runs_source_history ON apis.ai_routine_runs(project_id, source, started_at DESC, id DESC);

COMMIT;
