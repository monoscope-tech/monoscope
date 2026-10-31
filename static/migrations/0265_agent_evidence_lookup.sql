BEGIN;

CREATE INDEX ai_routine_runs_evidence_scope ON apis.ai_routine_runs
  (project_id, conversation_id, apis.evidence_permission_rank(evidence_permission) DESC NULLS LAST);
CREATE INDEX ai_routine_runs_unclassified_evidence ON apis.ai_routine_runs (project_id, conversation_id)
  WHERE evidence_permission IS NULL;
CREATE INDEX ai_conversations_unclassified_evidence ON apis.ai_conversations (project_id, conversation_id)
  WHERE evidence_permission IS NULL;

-- Each history contributes one indexed candidate, regardless of retained run count.
CREATE FUNCTION apis.conversation_evidence_permission(project uuid, conversation uuid)
RETURNS projects.project_permissions LANGUAGE sql STABLE AS $$
  SELECT permission FROM (
    SELECT evidence_permission AS permission FROM apis.ai_conversations
      WHERE project_id = project AND conversation_id = conversation
    UNION ALL
    (SELECT evidence_permission FROM apis.ai_routine_runs
      WHERE project_id = project AND conversation_id = conversation
      ORDER BY apis.evidence_permission_rank(evidence_permission) DESC NULLS LAST LIMIT 1)
  ) scopes ORDER BY apis.evidence_permission_rank(permission) DESC NULLS LAST LIMIT 1
$$;

CREATE OR REPLACE FUNCTION apis.inherit_evidence_permission() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  SELECT permission INTO NEW.evidence_permission FROM (
    VALUES (NEW.evidence_permission),
      (apis.conversation_evidence_permission(NEW.project_id, NEW.conversation_id))
  ) scopes(permission) ORDER BY apis.evidence_permission_rank(permission) DESC NULLS LAST LIMIT 1;
  RETURN NEW;
END
$$;

COMMIT;
