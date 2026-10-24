ALTER TABLE projects.pr_review_usage
  ADD CONSTRAINT agent_model_usage_requires_attempt
  CHECK (model_call_id IS NULL OR attempt_id IS NOT NULL);
