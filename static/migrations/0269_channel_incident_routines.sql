BEGIN;

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

COMMIT;
