-- Remove the 0125 rename scaffolding. NOT YET SAFE TO APPLY — see below.
--
-- 0126 recreated `projects.github_sync` and `projects.github_credentials` as auto-updatable
-- views over the renamed tables, so a release still running pre-0125 code keeps working across
-- the rename. This drops them.
--
-- It lives in `static/migrations-pending/` rather than `static/migrations/` on purpose: the
-- migration runner applies everything in the migrations directory at start-up, so simply
-- writing this file into place would drop the views out from under whatever is currently
-- deployed. That is precisely how the GitHub sync broke during the 0125 work.
--
-- PRECONDITION: every running instance is on the multi-host release (0125 or later). Once
-- that holds, verify nothing reads the old names and then move this file:
--
--   grep -rn 'github_sync\|github_credentials' src/
--     -> should return only `type GitHubSyncId = UUIDId "github_sync"`, which is a Haskell
--        phantom-type tag, not a table reference.
--
--   git mv static/migrations-pending/0128_drop_git_rename_compat_views.sql static/migrations/
--
-- Renumbered 0127 -> 0128: live-tail claimed 0127 (and a second 0126) while this sat here.
-- Prod carries three 0125_* and two 0126_* from parallel branches, so check
-- `SELECT filename FROM schema_migrations ORDER BY filename DESC` before activating.
BEGIN;

DROP VIEW IF EXISTS projects.github_sync;
DROP VIEW IF EXISTS projects.github_credentials;

COMMIT;
