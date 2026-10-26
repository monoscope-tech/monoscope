CREATE TABLE projects.rum_segments (
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  name text NOT NULL CHECK (length(btrim(name)) BETWEEN 1 AND 100),
  filters jsonb NOT NULL,
  PRIMARY KEY (project_id, name)
);
