BEGIN;
ALTER TABLE projects.memory_revisions
  ADD CONSTRAINT memory_revisions_page_identity_fkey
  FOREIGN KEY (project_id, page_id) REFERENCES projects.memory_pages (project_id, page_id) ON DELETE CASCADE;
COMMIT;
