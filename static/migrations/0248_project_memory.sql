BEGIN;
CREATE TABLE projects.memory_revisions (
  project_id uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  page_id uuid NOT NULL,
  version bigint NOT NULL CHECK (version BETWEEN 1 AND 4294967295),
  document jsonb NOT NULL CHECK (
    jsonb_typeof(document->'title') = 'string' AND length(btrim(document->>'title')) BETWEEN 1 AND 120
    AND jsonb_typeof(document->'body') = 'string' AND length(btrim(document->>'body')) BETWEEN 1 AND 16000
  ),
  author jsonb NOT NULL CHECK (author->>'tag' IN ('Member', 'Agent') AND jsonb_typeof(author->'contents') = 'string'),
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (project_id, page_id, version)
);
CREATE INDEX memory_revisions_project_updated ON projects.memory_revisions (project_id, created_at DESC);
COMMIT;
