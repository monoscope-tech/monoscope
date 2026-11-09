-- Jobs that outlive background_jobs' 10-minute timeout (workspace builds). odd-jobs uses one timeout
-- per table for both the kill timer and stale-lock recovery, so long jobs need their own table.
CREATE TABLE IF NOT EXISTS background_jobs_long (LIKE background_jobs INCLUDING ALL);

CREATE OR REPLACE FUNCTION public.notify_job_monitor_for_background_jobs_long()
RETURNS trigger AS $$
BEGIN
  PERFORM pg_notify(
    'jobs_created_background_jobs_long',
    json_build_object('id', NEW.id, 'run_at', NEW.run_at, 'locked_at', NEW.locked_at)::text
  );
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_notify_job_monitor_for_background_jobs_long ON background_jobs_long;
CREATE TRIGGER trg_notify_job_monitor_for_background_jobs_long AFTER INSERT ON background_jobs_long
  FOR EACH ROW EXECUTE PROCEDURE notify_job_monitor_for_background_jobs_long();

-- Admission caps each node at its own VM slots, so a lease records the node holding it.
ALTER TABLE projects.pr_review_threads ADD COLUMN IF NOT EXISTS lease_node text;
