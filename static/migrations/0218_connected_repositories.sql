CREATE TABLE projects.repositories (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  host TEXT NOT NULL,
  api_base TEXT,
  owner TEXT NOT NULL,
  repo TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE NULLS NOT DISTINCT (project_id, host, api_base, owner, repo)
);

INSERT INTO projects.repositories (project_id, host, api_base, owner, repo)
SELECT project_id, host, api_base, owner, repo FROM projects.git_sync
UNION
SELECT m.project_id, c.host, c.api_base, m.owner, m.repo
FROM projects.code_mappings m
JOIN projects.git_credentials c ON c.id = m.credential_id;
