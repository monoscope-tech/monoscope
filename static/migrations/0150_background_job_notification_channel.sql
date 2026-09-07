-- Match odd-jobs pgEventName for the public background_jobs table.
CREATE OR REPLACE FUNCTION public.notify_job_monitor_for_background_jobs()
RETURNS trigger AS $$
BEGIN
  PERFORM pg_notify(
    'jobs_created_background_jobs',
    json_build_object('id', NEW.id, 'run_at', NEW.run_at, 'locked_at', NEW.locked_at)::text
  );
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
