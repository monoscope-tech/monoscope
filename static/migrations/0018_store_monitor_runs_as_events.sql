CREATE OR REPLACE PROCEDURE monitors.check_triggered_query_monitors(job_id int, config jsonb)
LANGUAGE PLPGSQL AS $$
DECLARE
    monitor_rec RECORD;
    value NUMERIC;
    start TIMESTAMP;
    finish TIMESTAMP;
    status TEXT;
    duration_ns BIGINT;
BEGIN
    FOR monitor_rec IN
        SELECT *
        FROM monitors.query_monitors
        WHERE deactivated_at IS NULL
          AND log_query_as_sql IS NOT NULL
          AND log_query_as_sql != ''
    LOOP
        BEGIN
            -- Compute actual value from the SQL
            RAISE NOTICE 'Executing SQL: %', monitor_rec.log_query_as_sql;

            start := clock_timestamp();
            EXECUTE monitor_rec.log_query_as_sql INTO value;
            finish := clock_timestamp();
             RAISE NOTICE 'value SQL: %', value;
            -- Calculate duration in nanoseconds
            duration_ns := EXTRACT(EPOCH FROM (finish - start)) * 1000000000;
            
            -- Check if monitor was triggered
            IF (NOT monitor_rec.trigger_less_than AND 
                ((monitor_rec.warning_threshold IS NOT NULL AND monitor_rec.warning_threshold <= value)
                 OR monitor_rec.alert_threshold <= value))
               OR
               (monitor_rec.trigger_less_than AND 
                ((monitor_rec.warning_threshold IS NOT NULL AND monitor_rec.warning_threshold >= value)
                 OR monitor_rec.alert_threshold >= value))
            THEN
                -- Determine status
                IF (NOT monitor_rec.trigger_less_than AND monitor_rec.alert_threshold <= value)
                   OR (monitor_rec.trigger_less_than AND monitor_rec.alert_threshold >= value)
                THEN
                    status := 'Alerting';
                ELSIF monitor_rec.warning_threshold IS NOT NULL AND
                     ((NOT monitor_rec.trigger_less_than AND monitor_rec.warning_threshold <= value)
                      OR (monitor_rec.trigger_less_than AND monitor_rec.warning_threshold >= value))
                THEN
                    status := 'Warning';
                ELSE
                    status := 'Normal'; 
                END IF;

                -- Insert into otel_logs_and_spans as an alert
                INSERT INTO otel_logs_and_spans (
                    project_id,
                    kind,
                    timestamp,
                    name,
                    duration,
                    summary,
                    status_message,
                    context___trace_id,
                    body
                )
                VALUES (
                    monitor_rec.project_id,      
                    'alert',                     
                    start,
                    COALESCE(monitor_rec.alert_config->>'title', 'Untitled Monitor'),
                    duration_ns,  -- Duration in nanoseconds
                    ARRAY['Query monitor triggered', COALESCE(monitor_rec.alert_config->>'title', 'Untitled Monitor')]::TEXT[],
                    status,
                    monitor_rec.id::text,         
                    jsonb_build_object('value', value, 'monitor_id', monitor_rec.id)
                );

                -- Also queue the job
                INSERT INTO background_jobs (run_at, status, payload)
                VALUES (
                    NOW(),
                    'queued',
                    jsonb_build_object(
                        'tag', 'QueryMonitorsTriggered',
                        'contents', ARRAY[monitor_rec.id]
                    )
                );
            END IF;
            
        EXCEPTION WHEN OTHERS THEN
            RAISE NOTICE 'Error processing monitor %: %', monitor_rec.id, SQLERRM;
        END;
    END LOOP;
END;
$$;