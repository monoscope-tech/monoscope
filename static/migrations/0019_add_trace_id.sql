BEGIN;

ALTER TABLE apis.issues
ADD COLUMN first_trace_id TEXT,
ADD COLUMN recent_trace_id TEXT;

ALTER TABLE apis.errors
ADD COLUMN first_trace_id TEXT,
ADD COLUMN recent_trace_id TEXT;


ALTER TABLE apis.issues
DROP COLUMN IF EXISTS first_trace_id,
DROP COLUMN IF EXISTS recent_trace_id;

CREATE INDEX idx_errors_trace_ids
ON apis.errors (recent_trace_id, first_trace_id);

COMMIT;