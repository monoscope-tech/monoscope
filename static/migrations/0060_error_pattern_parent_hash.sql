-- Two-tier error fingerprinting: add parent_hash (broad, span-agnostic) and is_framework
-- to both error_patterns and issues. Enables cross-route rollup of framework/transport
-- errors into a single issue instead of fragmenting by span name.
--
-- parent_hash is NULLABLE on issues because non-error issue types (api_change, query_alert,
-- log_pattern, ...) have no parent hash concept. For error_patterns it is always computed,
-- but we keep it nullable there too for consistency and simpler upsert semantics.

ALTER TABLE apis.error_patterns
  ADD COLUMN IF NOT EXISTS parent_hash text,
  ADD COLUMN IF NOT EXISTS is_framework boolean NOT NULL DEFAULT false;

CREATE INDEX IF NOT EXISTS idx_error_patterns_parent_hash
  ON apis.error_patterns(project_id, parent_hash)
  WHERE parent_hash IS NOT NULL;

ALTER TABLE apis.issues
  ADD COLUMN IF NOT EXISTS parent_hash text,
  ADD COLUMN IF NOT EXISTS is_framework boolean NOT NULL DEFAULT false;

CREATE INDEX IF NOT EXISTS idx_issues_parent_hash
  ON apis.issues(project_id, parent_hash)
  WHERE parent_hash IS NOT NULL;
