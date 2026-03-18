CREATE TABLE IF NOT EXISTS projects.audit_log (
  id         BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  event      TEXT NOT NULL,
  actor_id   UUID,
  actor_email TEXT,
  metadata   JSONB,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS idx_audit_log_project_created ON projects.audit_log (project_id, created_at DESC);
