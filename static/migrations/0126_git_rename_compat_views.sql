-- Read/write compatibility for the 0125 rename, so a deployed release that still says
-- `github_sync` keeps working until the release that says `git_sync` is live.
--
-- 0125 renamed both tables. The migration runner applies on start-up, so the schema moves the
-- moment any instance boots — including one running the *previous* build, whose every query
-- still names the old tables. Between those two moments GitHub sync is broken: the settings
-- page 500s and the sync job errors.
--
-- These views close that window. They are simple single-table views with no aggregates, so
-- PostgreSQL makes them auto-updatable — INSERT/UPDATE/DELETE and `RETURNING *` all behave as
-- they did against the tables. `last_revision` is aliased back to its old name, which is the
-- only column 0125 renamed.
--
-- Drop them in a follow-up once the multi-host release has been live long enough that no
-- instance is running older code. They are compatibility scaffolding, not schema.
BEGIN;

CREATE OR REPLACE VIEW projects.github_sync AS
SELECT
    id,
    project_id,
    owner,
    repo,
    branch,
    access_token,
    installation_id,
    path_prefix,
    webhook_secret,
    last_revision AS last_tree_sha,
    sync_enabled,
    created_at,
    updated_at
FROM projects.git_sync;

CREATE OR REPLACE VIEW projects.github_credentials AS
SELECT
    id,
    project_id,
    account,
    installation_id,
    access_token,
    created_at,
    updated_at
FROM projects.git_credentials;

COMMIT;
