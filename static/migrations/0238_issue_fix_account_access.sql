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
