BEGIN;

-- Track notification test history
CREATE TABLE IF NOT EXISTS apis.notification_test_history (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  issue_type TEXT NOT NULL,
  channel TEXT NOT NULL,
  target TEXT,
  status TEXT NOT NULL,
  error TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_notification_test_history_project_created ON apis.notification_test_history(project_id, created_at DESC);

-- Add last_used_at tracking for integrations
ALTER TABLE apis.slack ADD COLUMN IF NOT EXISTS last_used_at TIMESTAMPTZ;
ALTER TABLE apis.discord ADD COLUMN IF NOT EXISTS last_used_at TIMESTAMPTZ;

COMMIT;
