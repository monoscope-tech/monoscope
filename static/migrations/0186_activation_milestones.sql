-- A project-level activation funnel. The fixed milestone vocabulary intentionally keeps
-- behavioral and personal data out of this table; its only purpose is aggregate conversion
-- and elapsed-time measurement from project creation to first value.
CREATE TABLE IF NOT EXISTS projects.activation_milestones (
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  milestone TEXT NOT NULL CHECK (milestone IN (
    'ingest_verified',
    'dashboard_created',
    'monitor_created',
    'notification_test_sent'
  )),
  occurred_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (project_id, milestone)
);

CREATE INDEX IF NOT EXISTS activation_milestones_milestone_occurred_at_idx
  ON projects.activation_milestones (milestone, occurred_at);
