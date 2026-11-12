-- Report routines can email their result to project members.
ALTER TABLE apis.ai_routines DROP CONSTRAINT ai_routine_destination,
  ADD CONSTRAINT ai_routine_destination CHECK (destination IN ('conversation', 'slack', 'email'));
