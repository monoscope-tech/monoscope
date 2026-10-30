BEGIN;

-- Existing installations need an explicit adoption of the broader capability policy.
ALTER TABLE apis.ai_routines
  ADD COLUMN policy TEXT NOT NULL DEFAULT 'observe' CHECK (policy IN ('observe', 'autonomous')),
  ADD COLUMN authority_version BIGINT NOT NULL DEFAULT 1 CHECK (authority_version >= 0),
  DROP COLUMN allow_actions,
  DROP COLUMN memory_granted_by;
ALTER TABLE apis.ai_routines
  ALTER COLUMN policy SET DEFAULT 'autonomous',
  ALTER COLUMN authority_version SET DEFAULT 0;

UPDATE apis.ai_routine_runs SET policy = 'observe' WHERE policy IS NULL;
ALTER TABLE apis.ai_routine_runs
  ADD COLUMN authority_version BIGINT NOT NULL DEFAULT 0 CHECK (authority_version >= 0),
  ALTER COLUMN policy SET DEFAULT 'observe',
  ALTER COLUMN policy SET NOT NULL;

COMMIT;
