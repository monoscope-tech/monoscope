-- Agent workspace lifetime is separate from verifier execution allocation.
-- Missing closure receipts remain unknown, including attempts lost before boot.
BEGIN;
CREATE TABLE projects.agent_allocations (
  id uuid PRIMARY KEY,
  attempt_id uuid NOT NULL REFERENCES projects.pr_review_attempts(id) ON DELETE CASCADE,
  receipt jsonb NOT NULL,
  recorded_at timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX agent_allocations_attempt ON projects.agent_allocations (attempt_id);
CREATE OR REPLACE VIEW projects.agent_usage_activity
  (project_id, run_id, fix_run_id, billing_provider, customer_id,
   calls, failed_calls, unpriced_calls, late_attributed_calls, unreported_attempts, cost_nanousd,
   checks, vcpu_ms, memory_mib_ms, log_bytes, explanations, render_ms, media_bytes, allocations, cleanup_failures, unreported_allocations) AS
SELECT project_id, run_id, fix_run_id, billing_provider, NULLIF(customer_id, ''),
  1::bigint, (NOT (receipt->>'succeeded')::boolean)::int,
  (cost_nanousd IS NULL)::int, (attempt_id IS NULL)::int, 0::bigint, COALESCE(cost_nanousd, 0),
  0::bigint, 0::numeric, 0::numeric, 0::numeric, 0::bigint, 0::numeric, 0::numeric, 0::bigint, 0::bigint, 0::bigint
FROM projects.pr_review_usage
UNION ALL
SELECT a.project_id, a.run_id, a.fix_run_id, a.billing_provider, NULLIF(a.customer_id, ''),
  0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
FROM projects.pr_review_attempts a
WHERE NOT EXISTS (SELECT 1 FROM projects.pr_review_usage u WHERE u.attempt_id = a.id)
UNION ALL
SELECT project_id, run_id, fix_run_id, billing_provider, NULLIF(customer_id, ''),
  0, 0, 0, 0, 0, 0, 1,
  (receipt->>'durationMs')::numeric * (receipt->>'cpus')::numeric,
  (receipt->>'durationMs')::numeric * (receipt->>'memoryMiB')::numeric,
  (receipt->>'logBytes')::numeric, 0, 0, 0, 0, 0, 0
FROM projects.pr_review_checks
UNION ALL
SELECT a.project_id, a.run_id, a.fix_run_id, a.billing_provider, NULLIF(a.customer_id, ''),
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  (j.explanation->>'renderMs')::numeric, (j.explanation->>'mediaBytes')::numeric, 0, 0, 0
FROM projects.issue_fix_jobs j
JOIN projects.pr_review_attempts a ON a.id = (j.result->>'attemptId')::uuid AND a.fix_run_id = j.run_id
WHERE j.media_retained_at IS NOT NULL
UNION ALL
SELECT a.project_id, a.run_id, a.fix_run_id, a.billing_provider, NULLIF(a.customer_id, ''),
  0, 0, 0, 0, 0, 0, 0,
  (v.receipt->>'durationMs')::numeric * (v.receipt->>'cpus')::numeric,
  (v.receipt->>'durationMs')::numeric * (v.receipt->>'memoryMiB')::numeric,
  0, 0, 0, 0, 1, (v.receipt->>'outcome' = 'release_failed')::int, 0
FROM projects.agent_allocations v JOIN projects.pr_review_attempts a ON a.id = v.attempt_id
UNION ALL
SELECT a.project_id, a.run_id, a.fix_run_id, a.billing_provider, NULLIF(a.customer_id, ''),
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
FROM projects.pr_review_attempts a
WHERE NOT EXISTS (SELECT 1 FROM projects.agent_allocations v WHERE v.attempt_id = a.id);
COMMIT;
