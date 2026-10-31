BEGIN;

CREATE FUNCTION apis.valid_evidence_audience(audience jsonb)
RETURNS boolean LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE AS $$
  SELECT COALESCE(
    audience IN ('{"tag":"ProjectAudience"}'::jsonb, '{"tag":"NoAudience"}'::jsonb)
    OR (audience->>'tag' = 'PersonalAudience' AND jsonb_typeof(audience->'contents') = 'string'
      AND audience->>'contents' ~ '^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$')
    OR (audience->>'tag' = 'SlackChannelAudience'
      AND jsonb_typeof(audience->'contents') = 'object'
      AND jsonb_typeof(audience #> '{contents,teamId}') = 'string'
      AND length(btrim(audience #>> '{contents,teamId}')) > 0
      AND audience #>> '{contents,channelId}' ~ '^[CG][A-Z0-9]+$')
  , false)
$$;

CREATE FUNCTION apis.meet_evidence_audience(left_scope jsonb, right_scope jsonb)
RETURNS jsonb LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE AS $$
  SELECT CASE
    WHEN left_scope = '{"tag":"ProjectAudience"}'::jsonb THEN right_scope
    WHEN right_scope = '{"tag":"ProjectAudience"}'::jsonb OR left_scope = right_scope THEN left_scope
    ELSE '{"tag":"NoAudience"}'::jsonb END
$$;

CREATE AGGREGATE apis.meet_evidence_audiences(jsonb) (
  SFUNC = apis.meet_evidence_audience, STYPE = jsonb, INITCOND = '{"tag":"ProjectAudience"}'
);

ALTER TABLE apis.ai_conversations ADD COLUMN evidence_audience jsonb NOT NULL DEFAULT '{"tag":"ProjectAudience"}'::jsonb
  CHECK (apis.valid_evidence_audience(evidence_audience));
ALTER TABLE apis.ai_routine_runs ADD COLUMN evidence_audience jsonb NOT NULL DEFAULT '{"tag":"ProjectAudience"}'::jsonb
  CHECK (apis.valid_evidence_audience(evidence_audience));

CREATE INDEX ai_routine_runs_private_audience ON apis.ai_routine_runs (project_id, conversation_id, evidence_audience)
  WHERE evidence_audience <> '{"tag":"ProjectAudience"}'::jsonb;

-- Indexed extremes bound the retained history to two candidates, including conflicts.
CREATE FUNCTION apis.conversation_evidence_audience(project uuid, conversation uuid)
RETURNS jsonb LANGUAGE sql STABLE AS $$
  SELECT apis.meet_evidence_audiences(audience) FROM (
    SELECT evidence_audience AS audience FROM apis.ai_conversations
      WHERE project_id = project AND conversation_id = conversation
    UNION ALL
    (SELECT evidence_audience FROM apis.ai_routine_runs
      WHERE project_id = project AND conversation_id = conversation
        AND evidence_audience <> '{"tag":"ProjectAudience"}'::jsonb
      ORDER BY evidence_audience ASC LIMIT 1)
    UNION ALL
    (SELECT evidence_audience FROM apis.ai_routine_runs
      WHERE project_id = project AND conversation_id = conversation
        AND evidence_audience <> '{"tag":"ProjectAudience"}'::jsonb
      ORDER BY evidence_audience DESC LIMIT 1)
  ) scopes
$$;

CREATE FUNCTION apis.inherit_evidence_audience() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE origin uuid;
BEGIN
  NEW.evidence_audience := apis.meet_evidence_audience(NEW.evidence_audience,
    apis.conversation_evidence_audience(NEW.project_id, NEW.conversation_id));
  IF TG_TABLE_NAME = 'ai_routine_runs' THEN
    SELECT conversation_id INTO origin FROM apis.ai_routines WHERE id = NEW.routine_id AND project_id = NEW.project_id;
    IF origin IS NOT NULL THEN
      NEW.evidence_audience := apis.meet_evidence_audience(NEW.evidence_audience,
        apis.conversation_evidence_audience(NEW.project_id, origin));
    END IF;
  END IF;
  RETURN NEW;
END
$$;

CREATE TRIGGER conversation_evidence_audience BEFORE INSERT OR UPDATE OF evidence_audience ON apis.ai_conversations
  FOR EACH ROW EXECUTE FUNCTION apis.inherit_evidence_audience();
CREATE TRIGGER run_evidence_audience BEFORE INSERT OR UPDATE OF evidence_audience ON apis.ai_routine_runs
  FOR EACH ROW EXECUTE FUNCTION apis.inherit_evidence_audience();

COMMIT;
