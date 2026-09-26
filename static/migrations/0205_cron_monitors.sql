-- Cron monitors: a job checks in with a span or log named 'cron.checkin' carrying
-- monitor.slug and monitor.status (ok | error). A missed or failed check-in opens a 'cron' issue.
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'cron';
CREATE TABLE IF NOT EXISTS apis.cron_monitors (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  slug TEXT NOT NULL,
  name TEXT NOT NULL,
  interval_secs INT NOT NULL,
  grace_secs INT NOT NULL DEFAULT 300,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  last_checkin_at TIMESTAMPTZ,
  last_status TEXT,
  next_eval_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (project_id, slug)
);
CREATE INDEX IF NOT EXISTS idx_cron_monitors_due ON apis.cron_monitors (next_eval_at);
