-- Whether the repository is private on its host; NULL until connect or analysis reads it.
-- Production evidence in pull request comments defaults on for private repositories only.
ALTER TABLE projects.repositories ADD COLUMN IF NOT EXISTS private BOOLEAN;
