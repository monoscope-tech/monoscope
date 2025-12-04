BEGIN;

CREATE TABLE IF NOT EXISTS projects.teams (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  name VARCHAR(255) NOT NULL,
  handle VARCHAR(255) NOT NULL,
  description TEXT,
  members UUID[] DEFAULT '{}',
  notify_emails TEXT[] DEFAULT '{}',
  slack_channels TEXT[] DEFAULT '{}', 
  discord_channels TEXT[] DEFAULT '{}',
  phone_numbers TEXT[] DEFAULT '{}',
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  created_by UUID NOT NULL REFERENCES users.users(id) ON DELETE SET NULL,
  deleted_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
  UNIQUE(project_id, handle)
);
SELECT manage_updated_at('projects.teams');

ALTER TABLE monitors.query_monitors ADD COLUMN IF NOT EXISTS teams UUID[] DEFAULT '{}';

COMMIT;