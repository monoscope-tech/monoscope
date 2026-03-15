BEGIN;

CREATE TABLE IF NOT EXISTS apis.command_palette_recents (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  user_id UUID NOT NULL REFERENCES users.users(id) ON DELETE CASCADE,
  item_type TEXT NOT NULL,
  label TEXT NOT NULL,
  url TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (project_id, user_id, url)
);

CREATE INDEX IF NOT EXISTS idx_cmd_palette_recents_lookup
  ON apis.command_palette_recents (project_id, user_id, created_at DESC);

COMMIT;
