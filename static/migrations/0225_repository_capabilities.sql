ALTER TABLE projects.repositories ADD COLUMN source_enabled BOOLEAN NOT NULL DEFAULT true;

-- NULL service scopes previously allowed identical duplicate mappings.
DELETE FROM projects.code_mappings a USING projects.code_mappings b
WHERE (a.project_id, a.credential_id, a.owner, a.repo, a.path_prefix) =
      (b.project_id, b.credential_id, b.owner, b.repo, b.path_prefix)
  AND a.service IS NOT DISTINCT FROM b.service
  AND (a.updated_at, a.id) < (b.updated_at, b.id);

ALTER TABLE projects.code_mappings
    DROP CONSTRAINT code_mappings_project_id_service_path_prefix_key,
    ADD CONSTRAINT code_mappings_repository_scope_key UNIQUE NULLS NOT DISTINCT
        (project_id, credential_id, owner, repo, service, path_prefix);
