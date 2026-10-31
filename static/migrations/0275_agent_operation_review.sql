BEGIN;

ALTER TABLE apis.ai_agent_operations
  ADD COLUMN review JSONB NOT NULL DEFAULT '{"tag":"NoReview"}'::jsonb
    CHECK (jsonb_typeof(review) = 'object');

COMMIT;
