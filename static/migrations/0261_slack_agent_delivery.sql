BEGIN;

-- Keep the existing store and Slack keys during the transition to shared runs.
ALTER TABLE apis.slack_turn_checkpoints
  DROP CONSTRAINT slack_turn_checkpoints_pkey;

ALTER TABLE apis.slack_turn_checkpoints
  ALTER COLUMN message_ts DROP NOT NULL,
  ADD COLUMN run_id UUID REFERENCES apis.ai_routine_runs(id) ON DELETE CASCADE,
  ADD CONSTRAINT slack_turn_checkpoints_legacy_turn
    UNIQUE (project_id, conversation_id, user_id, message_ts),
  ADD CONSTRAINT agent_checkpoint_identity
    UNIQUE NULLS NOT DISTINCT (project_id, conversation_id, user_id, message_ts, run_id),
  ADD CONSTRAINT agent_checkpoint_source
    CHECK (num_nonnulls(message_ts, run_id) = 1);

CREATE INDEX slack_events_message_identity ON apis.slack_events
  (team_id, (payload #>> '{event,channel}'), (payload #>> '{event,user}'),
   (payload #>> '{event,ts}'), received_at, id)
  WHERE payload #>> '{event,type}' IN ('message', 'app_mention')
    AND payload #>> '{event,subtype}' IS NULL
    AND payload #>> '{event,bot_id}' IS NULL;

ALTER TABLE apis.ai_routines ADD COLUMN incident_enabled_at TIMESTAMPTZ;
UPDATE apis.ai_routines SET incident_enabled_at = created_at WHERE schedule_kind = 'incident';
ALTER TABLE apis.ai_routines ADD CONSTRAINT ai_routine_incident_activation
  CHECK ((schedule_kind = 'incident') = (incident_enabled_at IS NOT NULL));

ALTER TABLE apis.slack_incident_deliveries
  DROP CONSTRAINT slack_incident_deliveries_operation_check,
  ADD COLUMN agent_run_id UUID REFERENCES apis.ai_routine_runs(id) ON DELETE CASCADE,
  ADD COLUMN agent_payload JSONB,
  ADD CONSTRAINT slack_incident_deliveries_operation_check
    CHECK (operation IN ('post_root', 'post_reply', 'update_root', 'post_analysis')),
  ADD CONSTRAINT slack_incident_agent_report CHECK (
    (operation = 'post_analysis' AND agent_run_id IS NOT NULL AND agent_payload IS NOT NULL AND jsonb_typeof(agent_payload) = 'object')
    OR (operation <> 'post_analysis' AND agent_run_id IS NULL AND agent_payload IS NULL)
  );

CREATE UNIQUE INDEX background_jobs_incident_investigation
  ON background_jobs (payload) WHERE payload ->> 'tag' = 'InvestigateIncident';

-- Workspace installation and personal evidence consent share expiring OAuth receipts.
ALTER TABLE apis.slack_install_requests
  ADD COLUMN team_id TEXT,
  ADD COLUMN slack_user_id TEXT,
  ADD COLUMN code_verifier TEXT,
  ADD COLUMN consumed_at TIMESTAMPTZ,
  ADD CONSTRAINT slack_oauth_purpose CHECK (
    (team_id IS NULL AND slack_user_id IS NULL AND code_verifier IS NULL)
    OR (team_id IS NOT NULL AND team_id <> '' AND slack_user_id IS NOT NULL AND slack_user_id <> ''
      AND code_verifier IS NOT NULL AND code_verifier ~ '^[A-Za-z0-9._~-]{43,128}$' AND NOT onboarding)
  );

CREATE TABLE apis.slack_user_credentials (
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  user_id UUID NOT NULL REFERENCES users.users(id) ON DELETE CASCADE,
  team_id TEXT NOT NULL,
  slack_user_id TEXT NOT NULL,
  access_token TEXT NOT NULL CHECK (access_token LIKE 'v1:%'),
  refresh_token TEXT CHECK (refresh_token LIKE 'v1:%'),
  expires_at TIMESTAMPTZ,
  scopes TEXT[] NOT NULL,
  PRIMARY KEY (project_id, user_id, team_id, slack_user_id),
  FOREIGN KEY (team_id, slack_user_id) REFERENCES apis.slack_identities(team_id, slack_user_id) ON DELETE CASCADE,
  CONSTRAINT slack_user_token_lifetime CHECK ((refresh_token IS NULL) = (expires_at IS NULL)),
  CONSTRAINT slack_user_read_scopes CHECK (
    cardinality(scopes) > 0 AND scopes <@ ARRAY[
      'search:read.public', 'search:read.private', 'search:read.im', 'search:read.mpim',
      'channels:history', 'groups:history', 'im:history', 'mpim:history'
    ]::TEXT[]
  )
);

ALTER TABLE apis.slack_user_credentials
  ADD COLUMN refresh_state JSONB NOT NULL DEFAULT '{"tag":"RefreshIdle"}'::jsonb,
  ADD CONSTRAINT slack_refresh_state CHECK (COALESCE(
    jsonb_typeof(refresh_state) = 'object'
    AND (
      refresh_state = '{"tag":"RefreshIdle"}'::jsonb
      OR (refresh_state->>'tag' IN ('RefreshClaimed', 'RefreshUncertain')
        AND jsonb_typeof(refresh_state->'contents') = 'array'
        AND jsonb_array_length(refresh_state->'contents') = 2
        AND jsonb_typeof(refresh_state->'contents'->0) = 'string'
        AND jsonb_typeof(refresh_state->'contents'->1) = 'string')
    )
  , FALSE));

DO $$ DECLARE constraint_name text; BEGIN
  FOR constraint_name IN SELECT conname FROM pg_constraint
    WHERE conrelid = 'apis.slack_reply_publications'::regclass
      AND confrelid = 'apis.slack_reply_batches'::regclass
  LOOP
    EXECUTE format('ALTER TABLE apis.slack_reply_publications DROP CONSTRAINT %I', constraint_name);
  END LOOP;
END $$;

ALTER TABLE apis.slack_reply_batches
  DROP CONSTRAINT slack_reply_batches_pkey,
  ALTER COLUMN message_ts DROP NOT NULL,
  ADD COLUMN run_id uuid UNIQUE REFERENCES apis.ai_routine_runs(id) ON DELETE CASCADE,
  ADD UNIQUE (project_id, conversation_id, user_id, message_ts),
  ADD UNIQUE (project_id, conversation_id, user_id, run_id),
  ADD CHECK ((message_ts IS NULL) <> (run_id IS NULL));

ALTER TABLE apis.slack_reply_publications
  ALTER COLUMN message_ts DROP NOT NULL,
  ALTER COLUMN thread_ts DROP NOT NULL,
  ADD COLUMN run_id uuid,
  ADD UNIQUE (run_id, part),
  ADD CHECK ((message_ts IS NULL) <> (run_id IS NULL)),
  ADD FOREIGN KEY (project_id, conversation_id, user_id, message_ts)
    REFERENCES apis.slack_reply_batches(project_id, conversation_id, user_id, message_ts) ON DELETE CASCADE,
  ADD FOREIGN KEY (project_id, conversation_id, user_id, run_id)
    REFERENCES apis.slack_reply_batches(project_id, conversation_id, user_id, run_id) ON DELETE CASCADE;

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

ALTER TABLE apis.slack_investigation_progress
  ADD COLUMN stream_state JSONB CHECK (stream_state IS NULL OR jsonb_typeof(stream_state) = 'object');

COMMIT;
