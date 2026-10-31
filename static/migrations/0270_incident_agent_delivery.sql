BEGIN;

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

COMMIT;
