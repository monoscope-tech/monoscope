BEGIN;

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

COMMIT;
