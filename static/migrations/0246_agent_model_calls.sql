BEGIN;
-- Started calls remain attributable even when the worker loses its usage receipt.
CREATE TABLE projects.agent_model_calls (
  id uuid PRIMARY KEY,
  attempt_id uuid NOT NULL REFERENCES projects.pr_review_attempts(id) ON DELETE CASCADE,
  receipt jsonb NOT NULL,
  started_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE (id, attempt_id)
);
CREATE INDEX agent_model_calls_attempt ON projects.agent_model_calls (attempt_id);
ALTER TABLE projects.pr_review_usage
  ADD COLUMN model_call_id uuid UNIQUE,
  ADD FOREIGN KEY (model_call_id, attempt_id) REFERENCES projects.agent_model_calls(id, attempt_id);

ALTER VIEW projects.agent_usage_activity RENAME TO agent_usage_activity_receipts;
CREATE VIEW projects.agent_usage_activity AS
SELECT r.*, 0::bigint AS unreported_calls FROM projects.agent_usage_activity_receipts r
UNION ALL
SELECT a.project_id, a.run_id, a.fix_run_id, a.billing_provider, NULLIF(a.customer_id, ''),
  1, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  1
FROM projects.agent_model_calls c JOIN projects.pr_review_attempts a ON a.id = c.attempt_id
WHERE NOT EXISTS (SELECT 1 FROM projects.pr_review_usage u WHERE u.model_call_id = c.id);
COMMIT;
