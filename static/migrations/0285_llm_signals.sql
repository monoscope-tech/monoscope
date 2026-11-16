-- Signals described in plain language and the conversations a model judged against them.
CREATE TABLE IF NOT EXISTS apis.llm_signals (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
  name TEXT NOT NULL CHECK (length(name) BETWEEN 1 AND 80),
  description TEXT NOT NULL CHECK (length(description) BETWEEN 1 AND 2000),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS llm_signals_project_idx ON apis.llm_signals (project_id);

CREATE TABLE IF NOT EXISTS apis.llm_signal_matches (
  signal_id UUID NOT NULL REFERENCES apis.llm_signals (id) ON DELETE CASCADE,
  conversation_key TEXT NOT NULL,
  matched BOOLEAN NOT NULL,
  reason TEXT NOT NULL DEFAULT '',
  evaluated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (signal_id, conversation_key)
);
