-- YAML defaults may normalize on import; compare local edits against the imported form.
ALTER TABLE projects.repository_resources ADD COLUMN local_baseline JSONB;
