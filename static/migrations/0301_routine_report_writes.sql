ALTER TABLE apis.ai_routines DROP CONSTRAINT ai_routine_report_when,
  ADD CONSTRAINT ai_routine_report_when CHECK (report_when IN ('always', 'findings', 'writes'));
