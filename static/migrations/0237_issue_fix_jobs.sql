BEGIN;
CREATE VIEW projects.issue_fix_run_access AS
SELECT r.id AS run_id, r.project_id, pm.user_id
FROM projects.issue_fix_runs r
JOIN projects.projects p ON p.id = r.project_id
JOIN apis.issues i ON i.id = r.issue_id AND i.project_id = p.id
JOIN projects.code_mappings m ON m.id = (r.setup->>'mappingId')::uuid AND m.project_id = p.id
JOIN projects.git_credentials c ON c.id = m.credential_id AND c.project_id = p.id
JOIN projects.project_members pm ON pm.project_id = p.id
WHERE p.deleted_at IS NULL AND i.archived_at IS NULL AND pm.active AND pm.deleted_at IS NULL AND pm.permission = 'admin'
  AND i.service = r.setup->>'service' AND m.service = i.service
  AND m.owner = r.setup->>'owner' AND m.repo = r.setup->>'repo'
  AND c.id = (r.setup->>'credentialId')::uuid AND c.installation_id = (r.setup->>'installationId')::bigint
  AND c.host = 'github' AND c.api_base IS NULL AND c.installation_id IS NOT NULL;
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
COMMIT;
