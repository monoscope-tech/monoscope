BEGIN;

ALTER TABLE apis.ai_conversations ADD COLUMN title TEXT;

CREATE INDEX ai_conversations_project_updated
  ON apis.ai_conversations (project_id, updated_at DESC)
  WHERE title IS NOT NULL;

CREATE TABLE apis.ai_routines (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL,
  conversation_id UUID NOT NULL,
  interval_minutes BIGINT NOT NULL CHECK (interval_minutes BETWEEN 5 AND 10080),
  timezone TEXT NOT NULL DEFAULT 'UTC',
  active BOOLEAN NOT NULL DEFAULT TRUE,
  next_run_at TIMESTAMPTZ,
  last_run_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  FOREIGN KEY (project_id, conversation_id)
    REFERENCES apis.ai_conversations(project_id, conversation_id) ON DELETE CASCADE,
  UNIQUE (project_id, conversation_id)
);

SELECT manage_updated_at('apis.ai_routines');
CREATE INDEX ai_routines_due ON apis.ai_routines (next_run_at) WHERE active;

COMMIT;
