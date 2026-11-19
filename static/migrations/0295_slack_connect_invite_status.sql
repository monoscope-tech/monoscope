-- Where each Slack Connect invite stands, said outright instead of read from which columns are set.
-- "Already in the channel" used to be stored as invite_id = ''.
ALTER TABLE projects.slack_connect_invites ADD COLUMN IF NOT EXISTS status TEXT;
UPDATE projects.slack_connect_invites
SET status = CASE WHEN error IS NOT NULL THEN 'failed' WHEN COALESCE(invite_id, '') = '' THEN 'already_member' ELSE 'invited' END,
    invite_id = CASE WHEN error IS NULL THEN NULLIF(invite_id, '') END;
ALTER TABLE projects.slack_connect_invites
  ALTER COLUMN status SET NOT NULL,
  ADD CONSTRAINT slack_connect_invites_status_ck CHECK (
    (status = 'invited' AND invite_id IS NOT NULL AND error IS NULL)
    OR (status = 'already_member' AND invite_id IS NULL AND error IS NULL)
    OR (status = 'failed' AND invite_id IS NULL AND error IS NOT NULL));
