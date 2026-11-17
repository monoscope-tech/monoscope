-- A provider 4xx (other than 408/429) fails identically on every retry. The drain retried 'failed'
-- rows oldest-first, so 2.9k such chunks from April-July filled every 600s run and the 6.7k
-- pending chunks behind them were never attempted. 'rejected' is terminal: kept for audit and
-- manual reconcile, never re-sent.
ALTER TABLE projects.usage_report_submissions
  DROP CONSTRAINT urs_status_enum,
  DROP CONSTRAINT urs_failed_iff_error,
  ADD CONSTRAINT urs_status_enum CHECK (status IN ('pending', 'submitted', 'failed', 'rejected')),
  ADD CONSTRAINT urs_failed_iff_error CHECK ((status IN ('failed', 'rejected')) = (last_error IS NOT NULL));

UPDATE projects.usage_report_submissions
SET status = 'rejected'
WHERE status = 'failed'
  AND last_error ~ 'statusCode = 4[0-9][0-9]'
  AND last_error !~ 'statusCode = 4(08|29)';
