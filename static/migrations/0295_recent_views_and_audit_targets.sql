-- What each person last opened in a project, one row per thing: Home's "Jump back in".
-- High-volume and overwritten in place, so it lives apart from the append-only audit log.
CREATE TABLE IF NOT EXISTS projects.recent_views (
  user_id    UUID NOT NULL,
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  kind       TEXT NOT NULL,
  target     TEXT NOT NULL,
  title      TEXT NOT NULL,
  url        TEXT NOT NULL,
  viewed_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (user_id, project_id, kind, target)
);
CREATE INDEX IF NOT EXISTS idx_recent_views_user ON projects.recent_views (user_id, project_id, viewed_at DESC);

-- Audit entries name who acted (a person, an AI agent, an API key or the system) and what they acted on.
ALTER TABLE projects.audit_log
  ADD COLUMN IF NOT EXISTS actor_kind TEXT NOT NULL DEFAULT 'user',
  ADD COLUMN IF NOT EXISTS target_kind TEXT,
  ADD COLUMN IF NOT EXISTS target_id TEXT,
  ADD COLUMN IF NOT EXISTS target_title TEXT;
