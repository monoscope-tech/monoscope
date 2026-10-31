BEGIN;

-- Existing evidence is classified from the shared catalog before it is disclosed.
ALTER TABLE apis.ai_conversations ADD COLUMN evidence_permission projects.project_permissions;
ALTER TABLE apis.ai_conversations ALTER COLUMN evidence_permission SET DEFAULT 'view';
ALTER TABLE apis.ai_routine_runs ADD COLUMN evidence_permission projects.project_permissions;
ALTER TABLE apis.ai_routine_runs ALTER COLUMN evidence_permission SET DEFAULT 'view';

-- The enum's historic PostgreSQL order is admin/view/edit, unlike the domain order.
CREATE FUNCTION apis.evidence_permission_rank(projects.project_permissions)
RETURNS integer LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE AS $$
  SELECT CASE $1 WHEN 'view' THEN 0 WHEN 'edit' THEN 1 WHEN 'admin' THEN 2 END
$$;

-- Recreated conversations and new attempts inherit retained evidence restrictions.
-- Updating a scope cannot reduce a previously recorded requirement.
CREATE FUNCTION apis.inherit_evidence_permission() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  SELECT permission INTO NEW.evidence_permission FROM (
    SELECT NEW.evidence_permission AS permission
    UNION ALL SELECT evidence_permission FROM apis.ai_conversations
      WHERE project_id = NEW.project_id AND conversation_id = NEW.conversation_id
    UNION ALL SELECT evidence_permission FROM apis.ai_routine_runs
      WHERE project_id = NEW.project_id AND conversation_id = NEW.conversation_id
  ) scopes ORDER BY apis.evidence_permission_rank(permission) DESC NULLS LAST LIMIT 1;
  RETURN NEW;
END
$$;

CREATE TRIGGER conversation_evidence_permission
  BEFORE INSERT OR UPDATE OF evidence_permission ON apis.ai_conversations
  FOR EACH ROW EXECUTE FUNCTION apis.inherit_evidence_permission();
CREATE TRIGGER run_evidence_permission
  BEFORE INSERT OR UPDATE OF evidence_permission ON apis.ai_routine_runs
  FOR EACH ROW EXECUTE FUNCTION apis.inherit_evidence_permission();

COMMIT;
