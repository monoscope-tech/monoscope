BEGIN;

ALTER TABLE projects.issue_fix_jobs ADD COLUMN agent_run_id uuid UNIQUE REFERENCES apis.ai_routine_runs(id);

-- A historical fix job could only be requested by an administrator. Preserve
-- that known ceiling and its original requester; current access is still required.
WITH seeds AS MATERIALIZED (
  SELECT job.run_id, job.requested_by, job.created_at, job.finished_at, job.result, job.failure,
    fix.project_id, gen_random_uuid() AS id, gen_random_uuid() AS conversation_id
  FROM projects.issue_fix_jobs job JOIN projects.issue_fix_runs fix ON fix.id = job.run_id
), conversations AS (
  INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type, title, context)
  SELECT project_id, conversation_id, 'web', 'Code fix', '{}'::jsonb FROM seeds
)
INSERT INTO apis.ai_routine_runs
  (id, root_run_id, project_id, conversation_id, requested_by, input, policy, source,
   scheduled_at, started_at, finished_at, status, permission_ceiling)
SELECT id, id, project_id, conversation_id, requested_by, 'Fix reported issue', '"autonomous"'::jsonb,
  jsonb_build_object('tag', 'FixRun', 'contents', run_id), created_at, created_at,
  CASE WHEN result IS NOT NULL OR failure IS NOT NULL THEN COALESCE(finished_at, created_at) END,
  CASE WHEN result IS NOT NULL THEN 'succeeded' WHEN failure IS NOT NULL THEN 'failed' ELSE 'running' END, 'admin'
FROM seeds;

UPDATE projects.issue_fix_jobs job SET agent_run_id = run.id FROM apis.ai_routine_runs run
WHERE run.source = jsonb_build_object('tag', 'FixRun', 'contents', job.run_id);
ALTER TABLE projects.issue_fix_jobs ALTER COLUMN agent_run_id SET NOT NULL;

CREATE UNIQUE INDEX ai_routine_runs_native_fix ON apis.ai_routine_runs(source) WHERE source->>'tag' = 'FixRun';
ALTER TABLE apis.ai_routine_runs ADD CONSTRAINT native_fix_run_grant CHECK (
  source->>'tag' <> 'FixRun' OR (routine_id IS NULL AND root_run_id = id AND policy = '"autonomous"'::jsonb
    AND permission_ceiling = 'admin' AND input IS NOT NULL)
);

CREATE FUNCTION projects.guard_fix_agent_link() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.agent_run_id IS DISTINCT FROM OLD.agent_run_id THEN
    RAISE EXCEPTION 'Fix run link is immutable' USING ERRCODE = '23514';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM apis.ai_routine_runs run JOIN projects.issue_fix_runs fix ON fix.id = NEW.run_id
    WHERE run.id = NEW.agent_run_id AND run.project_id = fix.project_id AND run.requested_by = NEW.requested_by
      AND run.source = jsonb_build_object('tag', 'FixRun', 'contents', fix.id)) THEN
    RAISE EXCEPTION 'Fix job must retain its own authorized run' USING ERRCODE = '23514';
  END IF;
  RETURN NEW;
END
$$;
CREATE TRIGGER fix_agent_link BEFORE INSERT OR UPDATE OF agent_run_id, requested_by, run_id ON projects.issue_fix_jobs
  FOR EACH ROW EXECUTE FUNCTION projects.guard_fix_agent_link();

CREATE FUNCTION apis.retain_fix_run_grant() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.source->>'tag' = 'FixRun' OR NEW.source->>'tag' = 'FixRun' THEN
    IF (NEW.project_id, NEW.conversation_id, NEW.root_run_id, NEW.source, NEW.policy, NEW.input)
        IS DISTINCT FROM (OLD.project_id, OLD.conversation_id, OLD.root_run_id, OLD.source, OLD.policy, OLD.input)
      OR (NEW.requested_by IS DISTINCT FROM OLD.requested_by AND NEW.requested_by IS NOT NULL) THEN
      RAISE EXCEPTION 'Native fix grant is immutable' USING ERRCODE = '23514';
    END IF;
  END IF;
  RETURN NEW;
END
$$;
CREATE TRIGGER retain_fix_run_grant BEFORE UPDATE ON apis.ai_routine_runs
  FOR EACH ROW EXECUTE FUNCTION apis.retain_fix_run_grant();

CREATE OR REPLACE VIEW projects.issue_fix_run_access AS
SELECT r.id AS run_id, r.project_id, pm.user_id
FROM projects.issue_fix_runs r
JOIN projects.projects p ON p.id = r.project_id
JOIN apis.issues i ON i.id = r.issue_id AND i.project_id = p.id
JOIN projects.code_mappings m ON m.id = (r.setup->>'mappingId')::uuid AND m.project_id = p.id
JOIN projects.git_credentials c ON c.id = m.credential_id AND c.project_id = p.id
JOIN projects.project_members pm ON pm.project_id = p.id
JOIN users.users u ON u.id = pm.user_id
WHERE u.active AND u.deleted_at IS NULL AND p.active AND p.deleted_at IS NULL AND i.archived_at IS NULL AND pm.active AND pm.deleted_at IS NULL AND pm.permission = 'admin'
  AND (lower(p.payment_plan) <> 'free' OR pm.user_id = (
    SELECT user_id FROM projects.project_members WHERE project_id = p.id ORDER BY created_at, id LIMIT 1))
  AND i.service = r.setup->>'service' AND m.service = i.service
  AND m.owner = r.setup->>'owner' AND m.repo = r.setup->>'repo'
  AND c.id = (r.setup->>'credentialId')::uuid AND c.installation_id = (r.setup->>'installationId')::bigint
  AND c.host = 'github' AND c.api_base IS NULL AND c.installation_id IS NOT NULL;

COMMIT;
