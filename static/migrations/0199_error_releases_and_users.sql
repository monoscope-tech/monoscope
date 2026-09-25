-- Releases come from the OTel resource attribute service.version.
-- resolved_in_release: "resolve in next release" — occurrences still reporting this release do not regress.
ALTER TABLE apis.error_patterns
  ADD COLUMN IF NOT EXISTS first_release TEXT,
  ADD COLUMN IF NOT EXISTS last_release TEXT,
  ADD COLUMN IF NOT EXISTS last_release_at TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS resolved_in_release TEXT,
  ADD COLUMN IF NOT EXISTS users_count BIGINT NOT NULL DEFAULT 0;

-- Distinct affected users per error (user.id / user.email / client.address), which also
-- lists them. users_count is maintained from the rows this table newly accepts.
CREATE TABLE IF NOT EXISTS apis.error_pattern_users (
  project_id UUID NOT NULL,
  error_id UUID NOT NULL REFERENCES apis.error_patterns(id) ON DELETE CASCADE,
  user_key TEXT NOT NULL,
  first_seen TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (project_id, error_id, user_key)
);
