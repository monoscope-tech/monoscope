-- apis.notification_digest_queue has a FK (issue_id) REFERENCES apis.issues(id)
-- ON DELETE CASCADE, but no index on the referencing column. Any DELETE on
-- apis.issues forces a seq scan of notification_digest_queue per row to find
-- cascade targets — a 23k-row issues cleanup stalled for 24 min before being
-- cancelled. Adding the index makes cascade deletes O(log n) instead of O(n).
-- Created CONCURRENTLY in prod on 2026-04-24; this migration makes dev/staging match.
CREATE INDEX IF NOT EXISTS idx_ndq_issue_id ON apis.notification_digest_queue(issue_id);
