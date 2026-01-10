BEGIN;
  DROP INDEX IF EXISTS apis.idx_issues_unresolved;
  DROP INDEX IF EXISTS apis.idx_issues_unique_open;
COMMIT;