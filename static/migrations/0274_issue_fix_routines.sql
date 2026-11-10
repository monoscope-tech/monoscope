BEGIN;
-- Autofix is an event routine. Its fix runs attach to it, so its policy bounds them and pausing it stops new ones.
ALTER TABLE apis.ai_routines
  DROP CONSTRAINT ai_routine_schedule_kind,
  DROP CONSTRAINT ai_routine_schedule_shape,
  ADD CONSTRAINT ai_routine_schedule_kind CHECK
    (schedule_kind IN ('interval', 'daily', 'weekdays', 'weekly', 'slack_message', 'pull_request', 'issue_enrichment', 'issue_fix', 'incident')),
  ADD CONSTRAINT ai_routine_schedule_shape CHECK (
    (schedule_kind = 'interval' AND interval_minutes IS NOT NULL AND schedule_hour IS NULL AND schedule_minute IS NULL AND schedule_weekday IS NULL)
    OR (schedule_kind IN ('daily', 'weekdays') AND interval_minutes IS NOT NULL AND schedule_hour IS NOT NULL AND schedule_minute IS NOT NULL AND schedule_weekday IS NULL)
    OR (schedule_kind = 'weekly' AND interval_minutes IS NOT NULL AND schedule_hour IS NOT NULL AND schedule_minute IS NOT NULL AND schedule_weekday IS NOT NULL)
    OR (schedule_kind IN ('slack_message', 'pull_request', 'issue_enrichment', 'issue_fix', 'incident') AND interval_minutes IS NULL AND next_run_at IS NULL
        AND schedule_hour IS NULL AND schedule_minute IS NULL AND schedule_weekday IS NULL
        AND instructions IS NOT NULL AND length(btrim(instructions)) > 0 AND template_version IS NOT NULL
        AND (template_key IS NOT NULL OR schedule_kind = 'incident'))
  );

ALTER TABLE apis.ai_routine_runs DROP CONSTRAINT native_fix_run_grant,
  ADD CONSTRAINT native_fix_run_grant CHECK (
    source->>'tag' <> 'FixRun' OR (root_run_id = id AND policy = '"autonomous"'::jsonb
      AND permission_ceiling = 'admin' AND input IS NOT NULL)
  );

COMMIT;
