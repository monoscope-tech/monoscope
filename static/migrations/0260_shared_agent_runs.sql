BEGIN;

ALTER TABLE apis.ai_routines
  ADD COLUMN requested_by UUID REFERENCES users.users(id);

-- Legacy routines have no recorded authorization. A member must resume them.
UPDATE apis.ai_routines SET active = FALSE, cancelled_at = clock_timestamp()
WHERE requested_by IS NULL;

ALTER TABLE apis.ai_routines
  ADD CONSTRAINT ai_routines_active_requester CHECK (NOT active OR requested_by IS NOT NULL);

ALTER TABLE projects.memory_revisions DROP CONSTRAINT memory_revisions_author_check;
ALTER TABLE projects.memory_revisions ADD CONSTRAINT memory_revisions_author_check
  CHECK (author->>'tag' IN ('Member', 'Agent', 'Conversation', 'Run') AND jsonb_typeof(author->'contents') = 'string');

ALTER TABLE apis.ai_routines
  ALTER COLUMN interval_minutes DROP NOT NULL,
  ADD COLUMN instructions TEXT;

ALTER TABLE apis.ai_routine_runs
  ADD COLUMN source JSONB NOT NULL DEFAULT '{"tag":"ScheduledRun"}',
  ADD COLUMN attempt BIGINT NOT NULL DEFAULT 1 CHECK (attempt > 0),
  ADD COLUMN requested_by UUID REFERENCES users.users(id) ON DELETE SET NULL,
  ADD COLUMN input TEXT,
  ADD COLUMN instructions TEXT,
  ADD COLUMN template_version BIGINT,
  DROP CONSTRAINT ai_routine_runs_routine_id_scheduled_at_key;

CREATE UNIQUE INDEX ai_routine_runs_scheduled_once
  ON apis.ai_routine_runs (routine_id, scheduled_at) WHERE source = '{"tag":"ScheduledRun"}';
CREATE UNIQUE INDEX ai_routine_runs_event_attempt
  ON apis.ai_routine_runs (routine_id, source, attempt) WHERE source <> '{"tag":"ScheduledRun"}';

CREATE TABLE apis.ai_routine_run_artifacts (
  run_id UUID NOT NULL REFERENCES apis.ai_routine_runs(id) ON DELETE CASCADE,
  key TEXT NOT NULL,
  title TEXT NOT NULL,
  content TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (run_id, key)
);

ALTER TABLE apis.ai_routine_runs
  ADD COLUMN policy TEXT CHECK (policy IN ('observe', 'autonomous'));

-- Existing installations need an explicit adoption of the broader capability policy.
ALTER TABLE apis.ai_routines
  ADD COLUMN policy TEXT NOT NULL DEFAULT 'observe' CHECK (policy IN ('observe', 'autonomous')),
  ADD COLUMN authority_version BIGINT NOT NULL DEFAULT 1 CHECK (authority_version >= 0),
  DROP COLUMN allow_actions,
  DROP COLUMN IF EXISTS memory_granted_by;
ALTER TABLE apis.ai_routines
  ALTER COLUMN policy SET DEFAULT 'autonomous',
  ALTER COLUMN authority_version SET DEFAULT 0;

UPDATE apis.ai_routine_runs SET policy = 'observe' WHERE policy IS NULL;
ALTER TABLE apis.ai_routine_runs
  ADD COLUMN authority_version BIGINT NOT NULL DEFAULT 0 CHECK (authority_version >= 0),
  ALTER COLUMN policy SET DEFAULT 'observe',
  ALTER COLUMN policy SET NOT NULL;

-- Existing attempts keep their checkpoint identity; future recoveries inherit it.
ALTER TABLE apis.ai_routine_runs
  ADD COLUMN root_run_id UUID REFERENCES apis.ai_routine_runs(id) ON DELETE CASCADE;
UPDATE apis.ai_routine_runs SET root_run_id = id;
ALTER TABLE apis.ai_routine_runs ALTER COLUMN root_run_id SET NOT NULL;

CREATE INDEX ai_routine_runs_request_root ON apis.ai_routine_runs(project_id, root_run_id);
CREATE INDEX ai_routine_runs_source_history ON apis.ai_routine_runs(project_id, source, started_at DESC, id DESC);

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

CREATE FUNCTION apis.inherit_evidence_permission() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  SELECT permission INTO NEW.evidence_permission FROM (
    VALUES (NEW.evidence_permission),
      (apis.conversation_evidence_permission(NEW.project_id, NEW.conversation_id))
  ) scopes(permission) ORDER BY apis.evidence_permission_rank(permission) DESC NULLS LAST LIMIT 1;
  RETURN NEW;
END
$$;

CREATE TRIGGER conversation_evidence_permission
  BEFORE INSERT OR UPDATE OF evidence_permission ON apis.ai_conversations
  FOR EACH ROW EXECUTE FUNCTION apis.inherit_evidence_permission();
CREATE TRIGGER run_evidence_permission
  BEFORE INSERT OR UPDATE OF evidence_permission ON apis.ai_routine_runs
  FOR EACH ROW EXECUTE FUNCTION apis.inherit_evidence_permission();

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

ALTER TABLE apis.ai_agent_operations
  ADD COLUMN review JSONB NOT NULL DEFAULT '{"tag":"NoReview"}'::jsonb
    CHECK (jsonb_typeof(review) = 'object');

ALTER TABLE apis.ai_routines
  ADD COLUMN incident_team_id TEXT,
  ADD COLUMN incident_channel_id TEXT,
  DROP CONSTRAINT ai_routine_schedule_kind,
  DROP CONSTRAINT ai_routine_schedule_shape,
  ADD CONSTRAINT ai_routine_schedule_kind CHECK
    (schedule_kind IN ('interval', 'daily', 'weekdays', 'weekly', 'slack_message', 'pull_request', 'issue_enrichment', 'incident')),
  ADD CONSTRAINT ai_routine_schedule_shape CHECK (
    (schedule_kind = 'interval' AND interval_minutes IS NOT NULL AND schedule_hour IS NULL AND schedule_minute IS NULL AND schedule_weekday IS NULL)
    OR (schedule_kind IN ('daily', 'weekdays') AND interval_minutes IS NOT NULL AND schedule_hour IS NOT NULL AND schedule_minute IS NOT NULL AND schedule_weekday IS NULL)
    OR (schedule_kind = 'weekly' AND interval_minutes IS NOT NULL AND schedule_hour IS NOT NULL AND schedule_minute IS NOT NULL AND schedule_weekday IS NOT NULL)
    OR (schedule_kind IN ('slack_message', 'pull_request', 'issue_enrichment', 'incident') AND interval_minutes IS NULL AND next_run_at IS NULL
        AND schedule_hour IS NULL AND schedule_minute IS NULL AND schedule_weekday IS NULL
        AND instructions IS NOT NULL AND length(btrim(instructions)) > 0 AND template_version IS NOT NULL
        AND (template_key IS NOT NULL OR schedule_kind = 'incident'))
  ),
  ADD CONSTRAINT ai_routine_incident_channel CHECK (
    (schedule_kind = 'incident' AND template_key IS NULL
      AND incident_team_id IS NOT NULL AND length(btrim(incident_team_id)) > 0
      AND incident_channel_id IS NOT NULL AND incident_channel_id ~ '^[CG][A-Z0-9]+$')
    OR (schedule_kind <> 'incident' AND incident_team_id IS NULL AND incident_channel_id IS NULL)
  );

CREATE UNIQUE INDEX ai_routines_incident_channel
  ON apis.ai_routines (project_id, incident_team_id, incident_channel_id)
  WHERE schedule_kind = 'incident';

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

CREATE FUNCTION apis.valid_agent_policy(policy jsonb) RETURNS boolean
LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE AS $$
  SELECT CASE
    WHEN policy IN ('"observe"'::jsonb, '"autonomous"'::jsonb) THEN TRUE
    WHEN jsonb_typeof(policy) = 'object' AND policy - 'review_selected' = '{}'::jsonb
      AND jsonb_typeof(policy->'review_selected') = 'object'
      AND (policy->'review_selected') - 'classes' - 'approver' = '{}'::jsonb
      AND policy #>> '{review_selected,approver}' IN ('ReviewEditors', 'ReviewAdministrators')
      AND jsonb_typeof(policy #> '{review_selected,classes}') = 'array'
    THEN jsonb_array_length(policy #> '{review_selected,classes}') > 0 AND NOT EXISTS (
      SELECT 1 FROM jsonb_array_elements(policy #> '{review_selected,classes}') entry
      WHERE entry NOT IN ('"knowledge_changes"'::jsonb, '"project_changes"'::jsonb,
        '"external_posts"'::jsonb, '"administrative_changes"'::jsonb))
    ELSE FALSE END
$$;

CREATE FUNCTION apis.meet_agent_policy(left_policy jsonb, right_policy jsonb) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE AS $$
BEGIN
  IF NOT apis.valid_agent_policy(left_policy) OR NOT apis.valid_agent_policy(right_policy) THEN
    RAISE EXCEPTION 'Invalid agent policy' USING ERRCODE = '23514';
  END IF;
  IF left_policy = '"observe"'::jsonb OR right_policy = '"observe"'::jsonb THEN RETURN '"observe"'::jsonb; END IF;
  IF left_policy = '"autonomous"'::jsonb THEN RETURN right_policy; END IF;
  IF right_policy = '"autonomous"'::jsonb THEN RETURN left_policy; END IF;
  RETURN jsonb_build_object('review_selected', jsonb_build_object(
    'classes', (SELECT jsonb_agg(DISTINCT entry ORDER BY entry) FROM jsonb_array_elements(
      (left_policy #> '{review_selected,classes}') || (right_policy #> '{review_selected,classes}')) entry),
    'approver', CASE WHEN 'ReviewAdministrators' IN (left_policy #>> '{review_selected,approver}', right_policy #>> '{review_selected,approver}')
      THEN 'ReviewAdministrators' ELSE 'ReviewEditors' END));
END
$$;

ALTER TABLE apis.ai_routines DROP CONSTRAINT ai_routines_policy_check,
  ALTER COLUMN policy DROP DEFAULT,
  ALTER COLUMN policy TYPE jsonb USING to_jsonb(policy),
  ALTER COLUMN policy SET DEFAULT '"autonomous"'::jsonb,
  ADD CONSTRAINT ai_routines_policy_check CHECK (apis.valid_agent_policy(policy));
ALTER TABLE apis.ai_routine_runs DROP CONSTRAINT ai_routine_runs_policy_check,
  ALTER COLUMN policy DROP DEFAULT,
  ALTER COLUMN policy TYPE jsonb USING to_jsonb(policy),
  ALTER COLUMN policy SET DEFAULT '"observe"'::jsonb,
  ADD CONSTRAINT ai_routine_runs_policy_check CHECK (apis.valid_agent_policy(policy)),
  ADD COLUMN policy_limit jsonb;
UPDATE apis.ai_routine_runs run SET policy_limit = apis.meet_agent_policy(run.policy, COALESCE(
  (SELECT routine.policy FROM apis.ai_routines routine WHERE routine.id = run.routine_id), run.policy));
ALTER TABLE apis.ai_routine_runs ALTER COLUMN policy_limit SET NOT NULL,
  ADD CONSTRAINT ai_routine_runs_policy_limit_check CHECK (apis.valid_agent_policy(policy_limit));

CREATE FUNCTION apis.retain_agent_policy_limit() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE inherited jsonb;
BEGIN
  IF TG_OP = 'UPDATE' THEN
    IF NEW.policy IS DISTINCT FROM OLD.policy THEN
      RAISE EXCEPTION 'The original run policy is immutable' USING ERRCODE = '23514';
    END IF;
    NEW.policy_limit := apis.meet_agent_policy(OLD.policy_limit, NEW.policy_limit);
  ELSE
    NEW.policy_limit := NEW.policy;
    IF NEW.routine_id IS NOT NULL THEN
      SELECT policy INTO inherited FROM apis.ai_routines WHERE id = NEW.routine_id FOR SHARE;
      NEW.policy_limit := apis.meet_agent_policy(NEW.policy_limit, inherited);
    END IF;
    IF NEW.root_run_id <> NEW.id THEN
      SELECT policy_limit INTO inherited FROM apis.ai_routine_runs WHERE id = NEW.root_run_id;
      NEW.policy_limit := apis.meet_agent_policy(NEW.policy_limit, inherited);
    END IF;
  END IF;
  RETURN NEW;
END
$$;
CREATE TRIGGER retain_agent_policy_limit BEFORE INSERT OR UPDATE OF policy, policy_limit ON apis.ai_routine_runs
  FOR EACH ROW EXECUTE FUNCTION apis.retain_agent_policy_limit();

CREATE FUNCTION apis.restrict_active_agent_policies() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.policy IS DISTINCT FROM OLD.policy THEN
    UPDATE apis.ai_routine_runs SET policy_limit = apis.meet_agent_policy(policy_limit, NEW.policy)
      WHERE routine_id = NEW.id AND status IN ('running', 'failed', 'timed_out');
  END IF;
  RETURN NEW;
END
$$;
CREATE TRIGGER restrict_active_agent_policies AFTER UPDATE OF policy ON apis.ai_routines
  FOR EACH ROW EXECUTE FUNCTION apis.restrict_active_agent_policies();

-- Older receipts prove their recorded rule; do not infer historical authority from today's membership.
UPDATE apis.ai_agent_operations SET review = jsonb_set(review, '{contents,1,authority}', review #> '{contents,0,approver}')
  WHERE review->>'tag' = 'ReviewResolved';
CREATE FUNCTION apis.retain_operation_review() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.review IS DISTINCT FROM OLD.review AND NOT (
    OLD.review->>'tag' = 'ReviewPending' AND NEW.review->>'tag' = 'ReviewResolved'
    AND NEW.review #> '{contents,0}' = OLD.review->'contents'
    AND OLD.outcome->>'tag' = 'OperationUnstarted'
  ) THEN RAISE EXCEPTION 'Operation review is immutable after a decision' USING ERRCODE = '23514'; END IF;
  RETURN NEW;
END
$$;
CREATE TRIGGER retain_operation_review BEFORE UPDATE ON apis.ai_agent_operations
  FOR EACH ROW EXECUTE FUNCTION apis.retain_operation_review();

ALTER TABLE apis.ai_routine_runs
  ADD CONSTRAINT interactive_agent_run_policy CHECK (
    source->>'tag' NOT IN ('InteractiveRun', 'ConversationRun')
    OR (routine_id IS NULL AND policy IS NOT NULL AND input IS NOT NULL)
  ),
  ADD CONSTRAINT member_conversation_run_actor CHECK (
    source->>'tag' <> 'ConversationRun' OR requested_by IS NOT NULL
  );

COMMIT;
