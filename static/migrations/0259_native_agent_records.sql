BEGIN;

-- Internal model-cost estimates. These rows do not enter billable AI usage.
CREATE TABLE projects.pr_review_usage (
  id uuid PRIMARY KEY,
  run_id uuid NOT NULL REFERENCES projects.pr_review_runs(id) ON DELETE CASCADE,
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  billing_provider text NOT NULL,
  customer_id text,
  receipt jsonb NOT NULL CHECK (jsonb_typeof(receipt) = 'object'),
  cost_nanousd numeric(20, 0) CHECK (cost_nanousd >= 0),
  created_at timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX pr_review_usage_project_run ON projects.pr_review_usage (project_id, run_id);
CREATE INDEX pr_review_usage_customer ON projects.pr_review_usage (billing_provider, customer_id, created_at)
  WHERE customer_id IS NOT NULL;

CREATE TABLE projects.pr_review_checks (
  id uuid PRIMARY KEY,
  run_id uuid NOT NULL REFERENCES projects.pr_review_runs(id) ON DELETE CASCADE,
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  billing_provider text NOT NULL,
  customer_id text,
  receipt jsonb NOT NULL CHECK (jsonb_typeof(receipt) = 'object'),
  created_at timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX pr_review_checks_project_run ON projects.pr_review_checks (project_id, run_id);

CREATE TABLE projects.pr_review_knowledge (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  run_id uuid NOT NULL REFERENCES projects.pr_review_runs(id) ON DELETE CASCADE,
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  owner text NOT NULL,
  repo text NOT NULL,
  service text NOT NULL,
  revision text NOT NULL,
  summary text NOT NULL,
  evidence_keys text[] NOT NULL,
  window_start timestamptz NOT NULL,
  window_end timestamptz NOT NULL,
  evidence jsonb NOT NULL CHECK (jsonb_typeof(evidence) = 'array'),
  created_at timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX pr_review_knowledge_scope ON projects.pr_review_knowledge
  (project_id, owner, repo, service, created_at DESC);

CREATE TABLE projects.issue_fix_runs (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  issue_id uuid NOT NULL REFERENCES apis.issues(id) ON DELETE CASCADE,
  setup jsonb NOT NULL CHECK (jsonb_typeof(setup) = 'object'),
  billing_provider text NOT NULL,
  customer_id text,
  version bigint NOT NULL DEFAULT 0 CHECK (version >= 0),
  created_at timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX issue_fix_runs_project_issue ON projects.issue_fix_runs (project_id, issue_id, created_at DESC);
CREATE TABLE projects.issue_fix_steps (
  id uuid PRIMARY KEY,
  run_id uuid NOT NULL REFERENCES projects.issue_fix_runs(id) ON DELETE CASCADE,
  version bigint NOT NULL CHECK (version > 0),
  step jsonb NOT NULL CHECK (jsonb_typeof(step) = 'object'),
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE (run_id, version)
);

-- A delivery can have a new step ID, but an execution receipt stays immutable.
CREATE UNIQUE INDEX issue_fix_steps_execution ON projects.issue_fix_steps ((step->'contents'->>'id'))
  WHERE step->>'tag' = 'Observe';

CREATE TABLE projects.pr_review_attempts (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  run_id uuid NOT NULL REFERENCES projects.pr_review_runs(id) ON DELETE CASCADE,
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  billing_provider text NOT NULL,
  customer_id text,
  provider text NOT NULL,
  model_spec text NOT NULL,
  started_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE (id, run_id, project_id)
);
CREATE INDEX pr_review_attempts_project_run ON projects.pr_review_attempts (project_id, run_id);
ALTER TABLE projects.pr_review_usage ADD COLUMN attempt_id uuid,
  ADD FOREIGN KEY (attempt_id, run_id, project_id) REFERENCES projects.pr_review_attempts (id, run_id, project_id);
ALTER TABLE projects.pr_review_checks ADD COLUMN attempt_id uuid,
  ADD FOREIGN KEY (attempt_id, run_id, project_id) REFERENCES projects.pr_review_attempts (id, run_id, project_id);

CREATE INDEX pr_review_usage_attempt ON projects.pr_review_usage (attempt_id) WHERE attempt_id IS NOT NULL;
CREATE INDEX pr_review_checks_attempt ON projects.pr_review_checks (attempt_id) WHERE attempt_id IS NOT NULL;

-- Keep legacy review columns for rolling deployments; fix identities are distinct.
ALTER TABLE projects.pr_review_attempts
  ALTER COLUMN run_id DROP NOT NULL,
  ADD COLUMN fix_run_id uuid REFERENCES projects.issue_fix_runs(id) ON DELETE CASCADE,
  ADD CHECK (num_nonnulls(run_id, fix_run_id) = 1),
  ADD UNIQUE (id, fix_run_id, project_id);
CREATE INDEX pr_review_attempts_project_fix ON projects.pr_review_attempts (project_id, fix_run_id) WHERE fix_run_id IS NOT NULL;
ALTER TABLE projects.pr_review_usage
  ALTER COLUMN run_id DROP NOT NULL,
  ADD COLUMN fix_run_id uuid REFERENCES projects.issue_fix_runs(id) ON DELETE CASCADE,
  ADD CHECK (num_nonnulls(run_id, fix_run_id) = 1),
  ADD CHECK (fix_run_id IS NULL OR attempt_id IS NOT NULL),
  ADD FOREIGN KEY (attempt_id, fix_run_id, project_id) REFERENCES projects.pr_review_attempts (id, fix_run_id, project_id);
ALTER TABLE projects.pr_review_checks
  ALTER COLUMN run_id DROP NOT NULL,
  ADD COLUMN fix_run_id uuid REFERENCES projects.issue_fix_runs(id) ON DELETE CASCADE,
  ADD CHECK (num_nonnulls(run_id, fix_run_id) = 1),
  ADD CHECK (fix_run_id IS NULL OR attempt_id IS NOT NULL),
  ADD FOREIGN KEY (attempt_id, fix_run_id, project_id) REFERENCES projects.pr_review_attempts (id, fix_run_id, project_id);
CREATE INDEX pr_review_usage_project_fix ON projects.pr_review_usage (project_id, fix_run_id) WHERE fix_run_id IS NOT NULL;
CREATE INDEX pr_review_checks_project_fix ON projects.pr_review_checks (project_id, fix_run_id) WHERE fix_run_id IS NOT NULL;

CREATE TABLE projects.issue_fix_jobs (
  run_id uuid PRIMARY KEY REFERENCES projects.issue_fix_runs(id) ON DELETE CASCADE,
  requested_by uuid NOT NULL REFERENCES users.users(id) ON DELETE CASCADE,
  lease_owner uuid,
  lease_until timestamptz,
  result jsonb CHECK (jsonb_typeof(result) = 'object'),
  investigation jsonb CHECK (jsonb_typeof(investigation) = 'object'),
  failure text,
  created_at timestamptz NOT NULL DEFAULT now(),
  finished_at timestamptz,
  CHECK ((lease_owner IS NULL) = (lease_until IS NULL)),
  CHECK ((result IS NULL) = (investigation IS NULL)),
  CHECK (result IS NULL OR (lease_owner IS NULL AND failure IS NULL))
);
ALTER TABLE projects.pr_review_knowledge
  ALTER COLUMN run_id DROP NOT NULL,
  ADD COLUMN fix_run_id uuid REFERENCES projects.issue_fix_runs(id) ON DELETE CASCADE,
  ADD CHECK (num_nonnulls(run_id, fix_run_id) = 1);
CREATE INDEX pr_review_knowledge_fix ON projects.pr_review_knowledge (fix_run_id) WHERE fix_run_id IS NOT NULL;

ALTER TABLE projects.issue_fix_jobs ADD COLUMN explanation jsonb;
ALTER TABLE projects.issue_fix_jobs ADD CONSTRAINT issue_fix_explanation_requires_result
  CHECK (explanation IS NULL OR result IS NOT NULL);

ALTER TABLE projects.issue_fix_jobs ADD COLUMN verified_result jsonb;
ALTER TABLE projects.issue_fix_jobs ADD CONSTRAINT issue_fix_checkpoint_requires_investigation
  CHECK (verified_result IS NULL OR investigation IS NOT NULL);

ALTER TABLE projects.issue_fix_jobs ADD COLUMN checkpoint_investigation jsonb
  CHECK (jsonb_typeof(checkpoint_investigation) = 'object');
UPDATE projects.issue_fix_jobs SET checkpoint_investigation = investigation WHERE verified_result IS NOT NULL;
ALTER TABLE projects.issue_fix_jobs DROP CONSTRAINT issue_fix_checkpoint_requires_investigation;
ALTER TABLE projects.issue_fix_jobs ADD CONSTRAINT issue_fix_checkpoint_pair
  CHECK ((verified_result IS NULL) = (checkpoint_investigation IS NULL));

ALTER TABLE projects.issue_fix_jobs ADD COLUMN media_retained_at timestamptz;
ALTER TABLE projects.issue_fix_jobs ADD CONSTRAINT issue_fix_retained_media_requires_explanation
  CHECK (media_retained_at IS NULL OR (result IS NOT NULL AND explanation IS NOT NULL));

-- Agent workspace lifetime is separate from verifier execution allocation.
-- Missing closure receipts remain unknown, including attempts lost before boot.

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

ALTER TABLE projects.issue_fix_jobs
  ADD COLUMN review_attempt_id uuid REFERENCES projects.pr_review_attempts(id),
  ADD COLUMN review_rationale text,
  ADD CONSTRAINT issue_fix_review_pair CHECK (
    (review_attempt_id IS NULL AND review_rationale IS NULL) OR
    (review_attempt_id IS NOT NULL AND review_rationale IS NOT NULL AND verified_result IS NOT NULL
      AND length(btrim(review_rationale)) BETWEEN 1 AND 1200)
  );

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

ALTER TABLE projects.pr_review_usage
  ADD CONSTRAINT agent_model_usage_requires_attempt
  CHECK (model_call_id IS NULL OR attempt_id IS NOT NULL);

CREATE OR REPLACE VIEW projects.issue_fix_run_access AS
SELECT r.id AS run_id, r.project_id, pm.user_id
FROM projects.issue_fix_runs r
JOIN projects.projects p ON p.id = r.project_id
JOIN apis.issues i ON i.id = r.issue_id AND i.project_id = p.id
JOIN projects.code_mappings m ON m.id = (r.setup->>'mappingId')::uuid AND m.project_id = p.id
JOIN projects.git_credentials c ON c.id = m.credential_id AND c.project_id = p.id
JOIN projects.project_members pm ON pm.project_id = p.id
JOIN users.users u ON u.id = pm.user_id
WHERE u.active AND u.deleted_at IS NULL AND p.deleted_at IS NULL AND i.archived_at IS NULL AND pm.active AND pm.deleted_at IS NULL AND pm.permission = 'admin'
  AND i.service = r.setup->>'service' AND m.service = i.service
  AND m.owner = r.setup->>'owner' AND m.repo = r.setup->>'repo'
  AND c.id = (r.setup->>'credentialId')::uuid AND c.installation_id = (r.setup->>'installationId')::bigint
  AND c.host = 'github' AND c.api_base IS NULL AND c.installation_id IS NOT NULL;

COMMIT;
