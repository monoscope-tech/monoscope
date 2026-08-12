-- Git integration stops meaning "GitHub".
--
-- 0124 named both tables after the one host they could talk to, and the code hardcoded
-- api.github.com in every call. GitLab (SaaS and self-hosted), Gitea/Forgejo and Bitbucket
-- Cloud all expose the same six operations we need — read a tree, read a file, write a file,
-- list repositories, report a default branch, sign a webhook — so the host becomes a column
-- and the dialect becomes a case expression. See plans/07-git-hosts.md.
--
-- Two new columns on each table:
--
--   * `host`     — which REST dialect to speak. 'github' for every existing row, which is what
--                  they are.
--   * `api_base` — the *API base* of a self-hosted install, already normalised (so a read must
--                  not append /api/v4 or /api/v1 again — the form does that once, on the way
--                  in). NULL means the host's SaaS. Gitea has no SaaS and so cannot be NULL;
--                  Bitbucket has no self-hosted and so must be. Those two rules need the
--                  host's semantics, so they live in `mkGitConn`, not here.
--
-- The tables are renamed to match: a `github_sync` row pointing at a GitLab project is the
-- kind of lying name that costs an afternoon two years from now.
BEGIN;

ALTER TABLE projects.github_sync RENAME TO git_sync;
ALTER TABLE projects.github_credentials RENAME TO git_credentials;

ALTER TABLE projects.git_sync
    ADD COLUMN IF NOT EXISTS host     TEXT NOT NULL DEFAULT 'github',
    ADD COLUMN IF NOT EXISTS api_base TEXT;

ALTER TABLE projects.git_credentials
    ADD COLUMN IF NOT EXISTS host     TEXT NOT NULL DEFAULT 'github',
    ADD COLUMN IF NOT EXISTS api_base TEXT;

-- `last_tree_sha` was a GitHub tree sha. GitLab's tree listing has no tree sha and
-- Bitbucket's directory listing has no equivalent, so the marker becomes the resolved head
-- *commit* id — which every host can report, and which answers the only question the column
-- was ever asked ("has anything changed since the last pull?"). Blob shas stay separate.
ALTER TABLE projects.git_sync RENAME COLUMN last_tree_sha TO last_revision;

-- Drop the old auth checks before adding host-aware ones. PostgreSQL keeps constraint names
-- across a table rename, so these are still spelled github_*.
ALTER TABLE projects.git_sync DROP CONSTRAINT IF EXISTS github_sync_auth_check;
ALTER TABLE projects.git_credentials DROP CONSTRAINT IF EXISTS github_credentials_auth_check;

-- Only GitHub has an App installation. Every other host authenticates with a token the user
-- created, so an installation_id on one of them is a row nothing could act on.
ALTER TABLE projects.git_sync
    ADD CONSTRAINT git_sync_host_check
        CHECK (host IN ('github', 'gitlab', 'bitbucket', 'gitea')),
    ADD CONSTRAINT git_sync_auth_check
        CHECK (
            (host = 'github' AND (installation_id IS NOT NULL OR access_token IS NOT NULL))
            OR (host <> 'github' AND installation_id IS NULL AND access_token IS NOT NULL)
        );

ALTER TABLE projects.git_credentials
    ADD CONSTRAINT git_credentials_host_check
        CHECK (host IN ('github', 'gitlab', 'bitbucket', 'gitea')),
    ADD CONSTRAINT git_credentials_auth_check
        CHECK (
            (host = 'github' AND (installation_id IS NOT NULL OR access_token IS NOT NULL))
            OR (host <> 'github' AND installation_id IS NULL AND access_token IS NOT NULL)
        );

-- An account name is only unique within a host: `acme` on GitHub and `acme` on GitLab are two
-- different grants, and before this a project could hold only one of them.
ALTER TABLE projects.git_credentials DROP CONSTRAINT IF EXISTS github_credentials_project_id_account_key;
ALTER TABLE projects.git_credentials DROP CONSTRAINT IF EXISTS git_credentials_project_id_host_account_key;
ALTER TABLE projects.git_credentials ADD CONSTRAINT git_credentials_project_id_host_account_key
    UNIQUE (project_id, host, account);

-- Inbound webhooks resolve a repository to a sync row by name. Two hosts can both have an
-- `acme/config`, so the lookup has to include the host or a push to one would sync the other.
CREATE INDEX IF NOT EXISTS git_sync_host_repo_idx ON projects.git_sync (host, owner, repo);

ALTER INDEX IF EXISTS projects.github_credentials_project_idx RENAME TO git_credentials_project_idx;

COMMIT;
