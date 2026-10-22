BEGIN;
ALTER TABLE projects.memory_revisions
  ADD CONSTRAINT memory_document_fields CHECK (document ?& ARRAY['title', 'body']),
  ADD CONSTRAINT memory_author_fields CHECK (author ?& ARRAY['tag', 'contents']);
COMMIT;
