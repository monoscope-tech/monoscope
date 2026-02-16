BEGIN;

-- Add PagerDuty to notification channel enum
ALTER TYPE notification_channel_enum ADD VALUE IF NOT EXISTS 'pagerduty';

-- Project-level PagerDuty integration (used for @everyone fallback)
CREATE TABLE IF NOT EXISTS apis.pagerduty (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID UNIQUE NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  integration_key TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_pagerduty_project_id ON apis.pagerduty(project_id);
SELECT manage_updated_at('apis.pagerduty');

-- Team-level PagerDuty integration keys
ALTER TABLE projects.teams ADD COLUMN IF NOT EXISTS pagerduty_services TEXT[] DEFAULT '{}';

COMMIT;
