BEGIN;

ALTER TABLE apis.ai_routines
  ADD COLUMN running_since TIMESTAMPTZ;

-- The sidebar now lists untitled issue conversations too.
DROP INDEX apis.ai_conversations_project_updated;
CREATE INDEX ai_conversations_project_updated
  ON apis.ai_conversations (project_id, updated_at DESC);

COMMIT;
