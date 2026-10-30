BEGIN;

ALTER TABLE apis.ai_routine_runs
  ADD COLUMN policy TEXT CHECK (policy IN ('observe', 'autonomous')),
  ADD CONSTRAINT interactive_agent_run_policy CHECK (
    source->>'tag' <> 'InteractiveRun'
    OR (routine_id IS NULL AND policy IS NOT NULL AND input IS NOT NULL)
  );

COMMIT;
