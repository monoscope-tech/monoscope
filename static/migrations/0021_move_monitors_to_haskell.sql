CREATE OR REPLACE PROCEDURE monitors.check_triggered_query_monitors(job_id int, config jsonb)
LANGUAGE PLPGSQL AS $$
BEGIN
    INSERT INTO background_jobs (run_at, status, payload) 
    VALUES (NOW(), 'queued', jsonb_build_object('tag', 'QueryMonitorsCheck'));
    RAISE NOTICE 'Background job queued for job_id: %', job_id;
EXCEPTION
    WHEN OTHERS THEN
        RAISE EXCEPTION 'Failed to queue background job: %', SQLERRM;
END;
$$;