BEGIN;

UPDATE apis.ai_routines
SET allow_actions = TRUE
WHERE template_key IS NULL;

COMMIT;
