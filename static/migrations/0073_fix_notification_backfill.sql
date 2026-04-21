-- 0073: Fix error-notification orphans + add per-project rate limiting + digest overflow.
--
-- Context: BackgroundJobs.notifyErrorSubscriptions previously gated "new error" notifications on
-- `created_at >= now - 60 minutes`. Any pattern that slipped past the window (job lag, channel not
-- yet configured, transient Slack failure) became permanently unreachable with last_notified_at=NULL.
-- This migration:
--   1. Backfills those orphans so the upcoming bounded sweep does not fire on history.
--   2. Adds a sliding-window rate-limit counter per project.
--   3. Adds a digest queue for rate-limited overflow and for log_pattern_rate_change issues.

-- 1. Idempotent backfill. Marks anything older than 1h that was never notified
--    as notified-now. Uses the existing partial index idx_error_patterns_new_recent.
UPDATE apis.error_patterns
SET last_notified_at = now()
WHERE last_notified_at IS NULL
  AND state <> 'resolved'
  AND created_at < now() - interval '1 hour';

-- 2. Notification rate-limit bookkeeping. One row per (project, hour bucket).
CREATE TABLE IF NOT EXISTS apis.notification_rate_limit (
  project_id   uuid        NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  window_start timestamptz NOT NULL,
  count        integer     NOT NULL DEFAULT 0,
  PRIMARY KEY (project_id, window_start)
);

CREATE INDEX IF NOT EXISTS idx_notification_rate_limit_recent
  ON apis.notification_rate_limit (project_id, window_start DESC);

-- 3. Digest queue for rate-limited overflow and low-signal issue types.
CREATE TABLE IF NOT EXISTS apis.notification_digest_queue (
  id               uuid        PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id       uuid        NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  error_pattern_id uuid        REFERENCES apis.error_patterns(id) ON DELETE CASCADE,
  issue_id         uuid        REFERENCES apis.issues(id) ON DELETE CASCADE,
  reason           text        NOT NULL, -- 'rate_limit' | 'log_pattern_rate_change' | 'log_pattern'
  title            text        NOT NULL DEFAULT '',
  created_at       timestamptz NOT NULL DEFAULT now(),
  sent_at          timestamptz
);

CREATE INDEX IF NOT EXISTS idx_notification_digest_queue_pending
  ON apis.notification_digest_queue (project_id, created_at)
  WHERE sent_at IS NULL;
