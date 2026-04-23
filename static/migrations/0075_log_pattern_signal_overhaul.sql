-- Log pattern rate-change signal overhaul.
--
-- Problem: the rate-change detector was producing thousands of issues per project
-- (6.7k in 7 days on talstack, 0% acked) because it lacked a baseline volume floor,
-- a persistence gate, and a post-ack cooldown. This migration adds the state
-- columns those features need; the detector enforces the new floors in Haskell.
BEGIN;

-- 1. Persistence gate: require an anomaly to be seen in 2 consecutive detection
-- runs before firing an issue. First detection writes the pending direction and
-- timestamp; a second matching detection within the TTL promotes to an issue.
-- A mismatching or absent second detection clears the pending state (recovered).
ALTER TABLE apis.log_patterns
  ADD COLUMN IF NOT EXISTS pending_anomaly_direction TEXT,
  ADD COLUMN IF NOT EXISTS pending_anomaly_detected_at TIMESTAMPTZ;

-- 2. Acknowledgment cooldown: when a user acks a rate-change issue we suppress
-- re-firing for 24h. Done in Haskell (index predicates can't use NOW() since it
-- is STABLE not IMMUTABLE). The detector queries the most recent acked issue
-- for the (project, target_hash, issue_type) triple before inserting; if its
-- cooldown_until is in the future, detection is skipped entirely. The open-row
-- unique index from migration 0032 is unchanged.
ALTER TABLE apis.issues
  ADD COLUMN IF NOT EXISTS cooldown_until TIMESTAMPTZ;

-- Fast lookup for the cooldown check: most recent acked row per (project,
-- target, type). Partial index keeps it tiny — only rows with a cooldown set.
CREATE INDEX IF NOT EXISTS idx_issues_cooldown_lookup
  ON apis.issues (project_id, target_hash, issue_type, acknowledged_at DESC)
  WHERE cooldown_until IS NOT NULL;

COMMIT;
