BEGIN;
ALTER TABLE projects.memory_revisions DROP CONSTRAINT memory_revisions_author_check;
ALTER TABLE projects.memory_revisions ADD CONSTRAINT memory_revisions_author_check
  CHECK (author->>'tag' IN ('Member', 'Agent', 'Conversation') AND jsonb_typeof(author->'contents') = 'string');
COMMIT;
