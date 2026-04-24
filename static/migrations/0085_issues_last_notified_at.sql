-- Per-issue notification dedup. Mirrors apis.error_patterns.last_notified_at
-- (migration 0033) so log_pattern / log_pattern_rate_change / runtime_exception
-- issue notifications use the same atomic-claim pattern as the error-pattern
-- path: UPDATE ... WHERE last_notified_at IS NULL OR last_notified_at < now() - interval 'N hours'.
-- Without this, every hourly tick re-notifies every open issue.
ALTER TABLE apis.issues
  ADD COLUMN IF NOT EXISTS last_notified_at timestamptz;

-- Backfill: treat every pre-existing open issue as already-notified so the
-- next tick does not fire on historical backlog. Same strategy migration 0073
-- used when introducing error-pattern rate limiting.
UPDATE apis.issues
SET last_notified_at = now()
WHERE last_notified_at IS NULL;
