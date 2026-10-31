BEGIN;

-- Legacy requests stay unbound; current identities cannot prove their original destination.
ALTER TABLE apis.ai_routine_runs ADD COLUMN command_origin jsonb,
  ADD CHECK (command_origin IS NULL OR (
    source->>'tag' = 'TriggeredRun' AND source #>> '{contents,tag}' = 'SlackCommand'
    AND jsonb_typeof(command_origin) = 'object'
    AND jsonb_typeof(command_origin->'team_id') = 'string'
    AND length(btrim(command_origin->>'team_id')) > 0
    AND jsonb_typeof(command_origin->'channel_id') = 'string'
    AND command_origin->>'channel_id' ~ '^[CGD][A-Z0-9]+$'
    AND jsonb_typeof(command_origin->'slack_user_id') = 'string'
    AND length(btrim(command_origin->>'slack_user_id')) > 0
  ) IS TRUE);

CREATE FUNCTION apis.retain_command_origin() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.command_origin IS DISTINCT FROM OLD.command_origin THEN
    RAISE EXCEPTION 'Command origin is immutable' USING ERRCODE = '23514';
  END IF;
  IF NEW.root_run_id <> NEW.id AND NOT EXISTS (
    SELECT 1 FROM apis.ai_routine_runs root WHERE root.id = NEW.root_run_id
      AND root.project_id = NEW.project_id AND root.command_origin IS NOT DISTINCT FROM NEW.command_origin
  ) THEN
    RAISE EXCEPTION 'Recovery must retain the original command origin' USING ERRCODE = '23514';
  END IF;
  RETURN NEW;
END
$$;
CREATE TRIGGER retain_command_origin BEFORE INSERT OR UPDATE ON apis.ai_routine_runs
  FOR EACH ROW EXECUTE FUNCTION apis.retain_command_origin();

COMMIT;
