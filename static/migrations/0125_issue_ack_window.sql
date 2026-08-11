-- Acknowledging an issue now means "I've got this — stop notifying me", and it
-- carries an explicit end: `acknowledged_until`. A far-future value (year 2132,
-- matching monitors' indefinite mute sentinel) encodes an indefinite ack; a
-- nearer one is a timed ack, after which the sweep clears the ack columns and
-- the issue returns to the Inbox and can notify again.
--
-- This supersedes `cooldown_until`, which expressed the same idea but only for
-- log_pattern_rate_change issues and only for a fixed 24h. The column is left in
-- place (unread) so a rollback doesn't lose data.
ALTER TABLE apis.issues ADD COLUMN IF NOT EXISTS acknowledged_until TIMESTAMPTZ;

-- Existing acks had no expiry, so they become indefinite ones.
UPDATE apis.issues
SET acknowledged_until = TIMESTAMPTZ '2132-08-31 00:00:00+00'
WHERE acknowledged_at IS NOT NULL AND acknowledged_until IS NULL;

-- Drives the ack-expiry sweep (every 10 min, global).
CREATE INDEX IF NOT EXISTS idx_issues_ack_expiry
  ON apis.issues (acknowledged_until)
  WHERE acknowledged_at IS NOT NULL AND archived_at IS NULL;

-- Drives the detectors' "is this signal still silenced?" lookup.
CREATE INDEX IF NOT EXISTS idx_issues_ack_silenced
  ON apis.issues (project_id, target_hash, issue_type, acknowledged_until DESC)
  WHERE acknowledged_until IS NOT NULL;
