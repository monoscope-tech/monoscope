-- Reports are AI routines now (#882): their history is each routine's dated documents. The old report
-- tables and the retired DailyReports/WeeklyReports jobs go; no code reads either.
DELETE FROM background_jobs WHERE payload->>'tag' IN ('DailyReports', 'WeeklyReports');
DROP TABLE IF EXISTS apis.report_previews;
DROP TABLE IF EXISTS apis.reports;
