-- A delivery can have a new step ID, but an execution receipt stays immutable.
CREATE UNIQUE INDEX issue_fix_steps_execution ON projects.issue_fix_steps ((step->'contents'->>'id'))
  WHERE step->>'tag' = 'Observe';
