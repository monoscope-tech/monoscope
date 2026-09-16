ALTER TABLE apis.slack_investigation_progress
  ADD COLUMN user_id UUID REFERENCES users.users(id) ON DELETE CASCADE,
  ADD COLUMN slack_user_id TEXT,
  ADD COLUMN publication_id UUID NOT NULL DEFAULT gen_random_uuid(),
  ALTER COLUMN progress_ts DROP NOT NULL;
CREATE UNIQUE INDEX slack_investigation_progress_publication
  ON apis.slack_investigation_progress(publication_id);
