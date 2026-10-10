CREATE FUNCTION projects.git_identity_name(host TEXT, name TEXT) RETURNS TEXT
LANGUAGE SQL IMMUTABLE STRICT AS $$ SELECT CASE WHEN host = 'github' THEN lower(name) ELSE name END $$;

-- Keep the newest grant, preferring an authorized App installation over a token.
CREATE TEMP TABLE github_credential_merges AS
SELECT id, first_value(id) OVER (
  PARTITION BY project_id, api_base, lower(account)
  ORDER BY (installation_id IS NOT NULL) DESC, updated_at DESC, id
) AS keep_id FROM projects.git_credentials WHERE host = 'github';

UPDATE projects.code_mappings m SET credential_id = c.keep_id
FROM github_credential_merges c WHERE m.credential_id = c.id AND c.id <> c.keep_id;
UPDATE projects.repositories r SET credential_id = c.keep_id
FROM github_credential_merges c WHERE r.credential_id = c.id AND c.id <> c.keep_id;
DELETE FROM projects.git_credentials c USING github_credential_merges m
WHERE c.id = m.id AND m.id <> m.keep_id;
DROP TABLE github_credential_merges;

CREATE TEMP TABLE github_sync_merges AS
SELECT id, first_value(id) OVER (
  PARTITION BY project_id, api_base, lower(owner), lower(repo)
  ORDER BY updated_at DESC, id
) AS keep_id FROM projects.git_sync WHERE host = 'github';

-- Preserve colliding dashboard files as local copies rather than deleting them.
WITH files AS (
  SELECT d.id, m.keep_id, row_number() OVER (
    PARTITION BY m.keep_id, d.file_path ORDER BY d.updated_at DESC, d.id
  ) AS rank
  FROM projects.dashboards d JOIN github_sync_merges m ON m.id = d.git_sync_id
)
UPDATE projects.dashboards d
SET git_sync_id = NULL, file_path = NULL, file_sha = NULL
FROM files f WHERE d.id = f.id AND d.file_path IS NOT NULL AND f.rank > 1;

UPDATE projects.dashboards d SET git_sync_id = m.keep_id
FROM github_sync_merges m WHERE d.git_sync_id = m.id AND m.id <> m.keep_id;

DELETE FROM projects.git_sync s USING github_sync_merges m
WHERE s.id = m.id AND m.id <> m.keep_id;
DROP TABLE github_sync_merges;

WITH repositories AS (
  SELECT id, row_number() OVER (
    PARTITION BY project_id, api_base, lower(owner), lower(repo)
    ORDER BY (credential_id IS NOT NULL) DESC, created_at DESC, id
  ) AS rank FROM projects.repositories WHERE host = 'github'
)
DELETE FROM projects.repositories r USING repositories m WHERE r.id = m.id AND m.rank > 1;

UPDATE projects.git_credentials SET account = lower(account) WHERE host = 'github';
UPDATE projects.repositories SET owner = lower(owner), repo = lower(repo) WHERE host = 'github';
UPDATE projects.git_sync SET owner = lower(owner), repo = lower(repo) WHERE host = 'github';
UPDATE projects.code_mappings m SET owner = lower(m.owner), repo = lower(m.repo)
FROM projects.git_credentials c WHERE c.id = m.credential_id AND c.host = 'github';

ALTER TABLE projects.git_credentials ADD CONSTRAINT git_credentials_canonical_account
  CHECK (account = projects.git_identity_name(host, account));
ALTER TABLE projects.repositories ADD CONSTRAINT repositories_canonical_name
  CHECK (owner = projects.git_identity_name(host, owner) AND repo = projects.git_identity_name(host, repo));
ALTER TABLE projects.git_sync ADD CONSTRAINT git_sync_canonical_name
  CHECK (owner = projects.git_identity_name(host, owner) AND repo = projects.git_identity_name(host, repo));
