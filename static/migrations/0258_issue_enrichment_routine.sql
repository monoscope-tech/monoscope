BEGIN;

ALTER TABLE apis.ai_routines
  DROP CONSTRAINT ai_routine_schedule_kind,
  DROP CONSTRAINT ai_routine_schedule_shape,
  ADD CONSTRAINT ai_routine_schedule_kind CHECK
    (schedule_kind IN ('interval', 'daily', 'weekdays', 'weekly', 'slack_message', 'pull_request', 'issue_enrichment')),
  ADD CONSTRAINT ai_routine_schedule_shape CHECK (
    (schedule_kind = 'interval' AND interval_minutes IS NOT NULL AND schedule_hour IS NULL AND schedule_minute IS NULL AND schedule_weekday IS NULL)
    OR (schedule_kind IN ('daily', 'weekdays') AND interval_minutes IS NOT NULL AND schedule_hour IS NOT NULL AND schedule_minute IS NOT NULL AND schedule_weekday IS NULL)
    OR (schedule_kind = 'weekly' AND interval_minutes IS NOT NULL AND schedule_hour IS NOT NULL AND schedule_minute IS NOT NULL AND schedule_weekday IS NOT NULL)
    OR (schedule_kind IN ('slack_message', 'pull_request', 'issue_enrichment') AND interval_minutes IS NULL AND next_run_at IS NULL
        AND schedule_hour IS NULL AND schedule_minute IS NULL AND schedule_weekday IS NULL
        AND instructions IS NOT NULL AND length(btrim(instructions)) > 0 AND template_key IS NOT NULL AND template_version IS NOT NULL)
  );

COMMIT;
