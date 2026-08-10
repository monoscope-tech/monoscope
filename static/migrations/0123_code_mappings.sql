-- Code mappings: how a stack-frame path becomes a file in a repository.
--
-- This is Sentry's "code mapping" and Datadog's "source code integration" in one table. A
-- frame says `/srv/app/services/checkout.py`; the repo says `services/checkout.py`; nothing
-- can bridge those two without being told, because the build that produced the frame is not
-- the checkout that holds the source.
--
-- Multiple rows per project, matched longest-prefix-first, rather than one root per project:
-- a project with two services in two repos is the normal case, and a single global root
-- breaks on the first one. `service` narrows a mapping to one service when two repos would
-- otherwise both claim a prefix; NULL means "any service", which is the common case.
--
-- The repo, its branch and its credentials live in `projects.github_sync` — the GitHub App
-- installation is already there for dashboard sync, and a second OAuth app to read blobs
-- from the same repo would be a worse version of the one we have.
BEGIN;

CREATE TABLE IF NOT EXISTS projects.code_mappings (
    id              UUID        PRIMARY KEY DEFAULT gen_random_uuid(),
    project_id      UUID        NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
    github_sync_id  UUID        NOT NULL REFERENCES projects.github_sync (id) ON DELETE CASCADE,
    service         TEXT,
    -- Leading segment of the stack-frame path to strip. '' matches every frame, which is the
    -- right default for a service whose frames are already repo-relative.
    path_prefix     TEXT        NOT NULL DEFAULT '',
    -- Directory inside the repo the stripped path is relative to. '' is the repo root.
    source_root     TEXT        NOT NULL DEFAULT '',
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    -- One mapping per (project, service, prefix): a second row for the same frame path would
    -- make which repo answers depend on row order.
    UNIQUE (project_id, service, path_prefix)
);

CREATE INDEX IF NOT EXISTS code_mappings_project_idx ON projects.code_mappings (project_id);

COMMIT;
