-- Back off retries for failed usage chunks so a permanently-broken project
-- (canceled sub, deleted customer, bad sub_item_id) stops re-logging every
-- daily ReportUsage tick. attempt_count is bumped on every failure; the
-- daily drain query in pendingUsageSubmissions skips failed rows whose
-- last_attempt_at is within LEAST(attempt_count, 14) days. Once the upstream
-- issue is fixed the next due tick picks the chunk up — no manual reset.
ALTER TABLE projects.usage_report_submissions
  ADD COLUMN IF NOT EXISTS attempt_count    int         NOT NULL DEFAULT 0,
  ADD COLUMN IF NOT EXISTS last_attempt_at  timestamptz;
