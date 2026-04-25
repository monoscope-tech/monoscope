-- Add event-occurrence window timestamps to apis.daily_usage.
-- Previously, the table only stored created_at (job-run time), so a catch-up
-- batch covering 15 months of events, inserted today, was counted as "this
-- month's" usage. window_start/window_end record when events actually occurred.
--
-- Nullable so existing rows are not invalidated by the DDL; new code always
-- writes them. Rows without window_start are pre-migration legacy and are
-- excluded from getTotalUsage / getDailyUsageBreakdown queries.
ALTER TABLE apis.daily_usage
  ADD COLUMN IF NOT EXISTS window_start TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS window_end   TIMESTAMPTZ;

CREATE INDEX IF NOT EXISTS idx_apis_daily_usage_project_window
  ON apis.daily_usage (project_id, window_start)
  WHERE window_start IS NOT NULL;

-- Backfill from usage_report_submissions.
-- Each recordUsageWindow call inserts N submission chunks all sharing the same
-- window_start/window_end. Match daily_usage rows by project_id and created_at
-- proximity (daily_usage row is written within seconds of window_end).
UPDATE apis.daily_usage du
SET
  window_start = sub.window_start,
  window_end   = sub.window_end
FROM (
  SELECT DISTINCT ON (project_id, window_end)
    project_id, window_start, window_end
  FROM projects.usage_report_submissions
  ORDER BY project_id, window_end, created_at
) sub
WHERE du.project_id = sub.project_id
  AND du.window_start IS NULL
  AND du.created_at BETWEEN sub.window_end - interval '5 minutes'
                        AND sub.window_end + interval '5 minutes';

-- Purge daily_usage rows whose window spans more than 30 days.
-- These are catch-up batches caused by the billing job not running for extended
-- periods — our fault, not customers'. Deleting resets the billing page display
-- to zero for affected projects; accurate hourly rows accumulate going forward.
DELETE FROM apis.daily_usage
WHERE window_end - window_start > interval '30 days';

-- Belt-and-suspenders: also purge any remaining pending submission chunks
-- spanning >30 days (should already be empty from the manual Step 0 run).
DELETE FROM projects.usage_report_submissions
WHERE status != 'submitted'
  AND window_end - window_start > interval '30 days';
