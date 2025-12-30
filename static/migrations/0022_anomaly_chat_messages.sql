BEGIN;

-- Generic AI conversations table that can be used across the app
CREATE TABLE IF NOT EXISTS apis.ai_conversations (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  conversation_id UUID NOT NULL,  -- Can hold issue_id, trace_id, or any contextual ID
  conversation_type TEXT NOT NULL,  -- 'anomaly', 'trace', 'log_explorer', 'dashboard', etc.
  context JSONB DEFAULT NULL,  -- Initial context for the conversation (error details, stack trace, etc.)
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS apis.ai_chat_messages (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  conversation_id UUID NOT NULL,
  role TEXT NOT NULL CHECK (role IN ('user', 'assistant', 'system')),
  content TEXT NOT NULL,
  widgets JSONB DEFAULT NULL,  -- Array of widget configs for AI-generated visualizations
  metadata JSONB DEFAULT NULL,  -- Additional metadata (tokens used, model, etc.)
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_ai_conversations_conv ON apis.ai_conversations(conversation_id);
CREATE INDEX IF NOT EXISTS idx_ai_conversations_project ON apis.ai_conversations(project_id);
CREATE INDEX IF NOT EXISTS idx_ai_chat_messages_conv ON apis.ai_chat_messages(conversation_id, created_at);
CREATE INDEX IF NOT EXISTS idx_ai_chat_messages_project ON apis.ai_chat_messages(project_id);

COMMIT;
