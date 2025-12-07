BEGIN;

ALTER TABLE apis.issues
ADD COLUMN first_trace_id TEXT,
ADD COLUMN recent_trace_id TEXT;

ALTER TABLE apis.errors
ADD COLUMN first_trace_id TEXT,
ADD COLUMN recent_trace_id TEXT;

COMMIT;