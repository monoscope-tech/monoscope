BEGIN;
CREATE TABLE projects.memory_pages (
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  page_id uuid NOT NULL,
  path text NOT NULL CHECK (
    length(path) BETWEEN 1 AND 512
    AND path ~ '^[A-Za-z0-9_.-]{1,120}(/[A-Za-z0-9_.-]{1,120}){0,15}$'
    AND path !~ '(^|/)[.]{1,2}(/|$)'
  ),
  PRIMARY KEY (project_id, page_id),
  UNIQUE (project_id, path)
);
INSERT INTO projects.memory_pages (project_id, page_id, path)
SELECT DISTINCT project_id, page_id, page_id::text FROM projects.memory_revisions;
COMMIT;
