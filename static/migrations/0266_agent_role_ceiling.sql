BEGIN;

-- Unknown original grants retain ordinary reads only. New admissions capture their grant.
ALTER TABLE apis.ai_routine_runs ADD COLUMN permission_ceiling projects.project_permissions NOT NULL DEFAULT 'view';

-- PostgreSQL's historic enum ordering differs from the permission hierarchy.
-- A missing member contributes no privileged authority.
CREATE FUNCTION apis.least_permission(VARIADIC permissions projects.project_permissions[])
RETURNS projects.project_permissions LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE AS $$
  SELECT COALESCE(permission, 'view') FROM unnest(permissions) roles(permission)
  ORDER BY apis.evidence_permission_rank(COALESCE(permission, 'view')) LIMIT 1
$$;

CREATE FUNCTION apis.retain_run_permission_ceiling() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.permission_ceiling IS DISTINCT FROM OLD.permission_ceiling THEN
    RAISE EXCEPTION 'Run permission ceiling is immutable' USING ERRCODE = '23514';
  END IF;
  IF NEW.root_run_id <> NEW.id AND NOT EXISTS (
    SELECT 1 FROM apis.ai_routine_runs root WHERE root.id = NEW.root_run_id
      AND root.project_id = NEW.project_id AND root.permission_ceiling = NEW.permission_ceiling
  ) THEN
    RAISE EXCEPTION 'Recovery must retain the original permission ceiling' USING ERRCODE = '23514';
  END IF;
  RETURN NEW;
END
$$;
CREATE TRIGGER run_permission_ceiling BEFORE INSERT OR UPDATE OF permission_ceiling ON apis.ai_routine_runs
  FOR EACH ROW EXECUTE FUNCTION apis.retain_run_permission_ceiling();

COMMIT;
