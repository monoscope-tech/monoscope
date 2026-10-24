BEGIN;
ALTER TABLE apis.ai_routines ADD COLUMN memory_granted_by uuid REFERENCES users.users(id) ON DELETE SET NULL;
COMMIT;
