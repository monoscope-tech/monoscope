-- Items agents and routines put on a project's Home page. One row per (project, kind, title):
-- a routine that flags the same thing again refreshes it rather than stacking duplicates.
CREATE TABLE IF NOT EXISTS apis.home_highlights (
  id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id      UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  kind            TEXT NOT NULL CHECK (kind IN ('issue', 'improvement', 'insight')),
  title           TEXT NOT NULL,
  detail          TEXT NOT NULL DEFAULT '',
  link            TEXT,
  source          TEXT NOT NULL,
  conversation_id UUID,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
  expires_at      TIMESTAMPTZ NOT NULL,
  dismissed_at    TIMESTAMPTZ,
  UNIQUE (project_id, kind, title)
);
CREATE INDEX IF NOT EXISTS idx_home_highlights_live ON apis.home_highlights (project_id, expires_at DESC) WHERE dismissed_at IS NULL;

-- The Slack Connect channel Monoscope's own workspace shares with a customer project,
-- and every invite sent to it (Slack accepts one recipient per inviteShared call).
CREATE TABLE IF NOT EXISTS projects.slack_connect_channels (
  project_id   UUID PRIMARY KEY REFERENCES projects.projects(id) ON DELETE CASCADE,
  channel_id   TEXT NOT NULL,
  channel_name TEXT NOT NULL,
  team_id      TEXT,
  created_at   TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE TABLE IF NOT EXISTS projects.slack_connect_invites (
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  email      TEXT NOT NULL,
  invite_id  TEXT,
  error      TEXT,
  invited_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (project_id, email)
);
