ALTER TABLE projects.replay_sessions ADD COLUMN IF NOT EXISTS event_file_count INTEGER NOT NULL DEFAULT 0;
