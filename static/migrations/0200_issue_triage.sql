-- Triage on every issue type: an owner, and archive windows that end on their own.
ALTER TABLE apis.issues
  ADD COLUMN IF NOT EXISTS assignee_id UUID REFERENCES users.users(id) ON DELETE SET NULL,
  ADD COLUMN IF NOT EXISTS archived_until TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS archive_until_escalating BOOLEAN NOT NULL DEFAULT false;
