ALTER TABLE projects.git_credentials
  ADD CONSTRAINT git_credentials_project_id_id_key UNIQUE (project_id, id);

ALTER TABLE projects.repositories
  ADD COLUMN credential_id UUID,
  ADD CONSTRAINT repositories_account_project_fk
    FOREIGN KEY (project_id, credential_id) REFERENCES projects.git_credentials (project_id, id)
    ON DELETE SET NULL (credential_id);

-- Prefer the account explicitly used for source context when one is known.
WITH accounts AS (
  SELECT DISTINCT ON (r.id) r.id, m.credential_id
  FROM projects.repositories r
  JOIN projects.code_mappings m ON m.project_id = r.project_id AND m.owner = r.owner AND m.repo = r.repo
  JOIN projects.git_credentials c ON c.id = m.credential_id AND c.host = r.host AND c.api_base IS NOT DISTINCT FROM r.api_base
  ORDER BY r.id, m.updated_at DESC, m.id
)
UPDATE projects.repositories r SET credential_id = a.credential_id FROM accounts a WHERE r.id = a.id;
