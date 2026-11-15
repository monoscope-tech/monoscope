-- The page a copilot request was asked from: the agent reads it as context and may navigate the user.
ALTER TABLE apis.ai_routine_runs ADD COLUMN IF NOT EXISTS page_context jsonb;
