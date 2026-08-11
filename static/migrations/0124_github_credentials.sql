-- Source repositories for code context, separated from the config-sync repository.
--
-- 0123 hung a code mapping off `projects.github_sync`, which is `UNIQUE (project_id)` and is
-- the repo monoscope syncs dashboard/monitor YAML with. A project monitors many services in
-- many repos, so that made every repo but the config one unreachable — and `fetchSnippet`
-- loaded the project's single sync row and refused any mapping that did not point at it.
--
-- The two are different things and now say so:
--
--   * `github_sync`      — one repo per project, monoscope's own YAML. Untouched.
--   * `github_credentials` — many per project. A GitHub App installation (or a PAT) scoped to
--     an account. An installation already grants access to every repo in that account, which
--     is why auth belongs here and not on a per-repo row.
--   * `code_mappings`    — now names the repo itself (owner/repo/ref), authorised by a
--     credential. Adding the tenth service repo costs a mapping, not an integration.
BEGIN;

CREATE TABLE IF NOT EXISTS projects.github_credentials (
    id              UUID        PRIMARY KEY DEFAULT gen_random_uuid(),
    project_id      UUID        NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
    -- The org/user login the installation covers. One credential per account per project:
    -- a second row for the same account is the same grant twice.
    account         TEXT        NOT NULL,
    installation_id BIGINT,
    access_token    TEXT, -- encrypted PAT, for accounts without the App
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    UNIQUE (project_id, account),
    CONSTRAINT github_credentials_auth_check CHECK (installation_id IS NOT NULL OR access_token IS NOT NULL)
);

CREATE INDEX IF NOT EXISTS github_credentials_project_idx ON projects.github_credentials (project_id);

ALTER TABLE projects.code_mappings
    ADD COLUMN IF NOT EXISTS credential_id UUID REFERENCES projects.github_credentials (id) ON DELETE CASCADE,
    ADD COLUMN IF NOT EXISTS owner TEXT NOT NULL DEFAULT '',
    ADD COLUMN IF NOT EXISTS repo  TEXT NOT NULL DEFAULT '',
    -- Branch or commit sha to read at. The span's own revision wins when it carries one; this
    -- is the fallback for telemetry that does not report where it was built from.
    ADD COLUMN IF NOT EXISTS ref   TEXT NOT NULL DEFAULT 'main';

-- Carry any mapping made under 0123 across: its repo was the config repo, so mint a credential
-- from that row's grant and copy the coordinates onto the mapping.
INSERT INTO projects.github_credentials (project_id, account, installation_id, access_token)
SELECT DISTINCT gs.project_id, gs.owner, gs.installation_id, gs.access_token
FROM projects.github_sync gs
WHERE EXISTS (SELECT 1 FROM projects.code_mappings cm WHERE cm.github_sync_id = gs.id)
ON CONFLICT (project_id, account) DO NOTHING;

UPDATE projects.code_mappings cm
SET credential_id = gc.id, owner = gs.owner, repo = gs.repo, ref = gs.branch
FROM projects.github_sync gs
JOIN projects.github_credentials gc ON gc.project_id = gs.project_id AND gc.account = gs.owner
WHERE cm.github_sync_id = gs.id;

-- A mapping whose repo cannot be reconstructed could only ever have rendered
-- "no longer linked"; dropping it is what the reader already saw.
DELETE FROM projects.code_mappings WHERE credential_id IS NULL;

ALTER TABLE projects.code_mappings
    ALTER COLUMN credential_id SET NOT NULL,
    DROP COLUMN IF EXISTS github_sync_id;

COMMIT;
