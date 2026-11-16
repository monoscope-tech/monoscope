-- The floating assistant hides a conversation the requester dismissed until they ask it something new.
ALTER TABLE apis.ai_conversations ADD COLUMN IF NOT EXISTS dock_dismissed_at timestamptz;
