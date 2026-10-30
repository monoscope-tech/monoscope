BEGIN;

CREATE TABLE apis.ai_agent_operations (
  id UUID PRIMARY KEY,
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  run_id UUID NOT NULL REFERENCES apis.ai_routine_runs(id) ON DELETE CASCADE,
  -- Historical attribution survives account deletion; live access is checked before execution.
  requested_by UUID NOT NULL,
  checkpoint_revision BIGINT NOT NULL CHECK (checkpoint_revision >= 0),
  proposal JSONB NOT NULL CHECK (jsonb_typeof(proposal) = 'object'),
  outcome JSONB NOT NULL CHECK (jsonb_typeof(outcome) = 'object'),
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  UNIQUE (run_id, checkpoint_revision)
);

COMMIT;
