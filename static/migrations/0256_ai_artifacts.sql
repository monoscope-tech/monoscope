BEGIN;

CREATE TABLE apis.ai_artifacts (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL,
  conversation_id UUID NOT NULL,
  key TEXT NOT NULL CHECK (key ~ '^[a-z0-9][a-z0-9-]{0,63}$'),
  title TEXT NOT NULL,
  content TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  FOREIGN KEY (project_id, conversation_id)
    REFERENCES apis.ai_conversations(project_id, conversation_id) ON DELETE CASCADE,
  UNIQUE (project_id, conversation_id, key)
);

COMMIT;
