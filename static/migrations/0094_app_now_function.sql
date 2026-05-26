-- Unified time testing: app_now() function for controllable time in
-- triggers and stored procedures. Companion module: src/Pkg/TestClock.hs.
--
-- app_now() reads the custom GUC 'app.current_time'. When set (test
-- connections, via syncConnectionTime), it returns that time. When unset
-- (production), it falls back to NOW(). Tests can therefore fast-forward
-- time even inside triggers / DDL defaults / scheduled-job procs, which
-- the Haskell-side Time effect alone can't reach.
CREATE OR REPLACE FUNCTION app_now() RETURNS TIMESTAMPTZ AS $$
BEGIN
  RETURN COALESCE(
    NULLIF(current_setting('app.current_time', true), '')::timestamptz,
    NOW()
  );
END;
$$ LANGUAGE plpgsql STABLE;


-- set_updated_at: swap current_timestamp → app_now() so triggered
-- updated_at stamps follow the controllable clock in tests.
-- (Original definition: migration 0001.)
CREATE OR REPLACE FUNCTION set_updated_at() RETURNS trigger AS $$
BEGIN
  IF (
    NEW IS DISTINCT FROM OLD AND
    NEW.updated_at IS NOT DISTINCT FROM OLD.updated_at
  ) THEN NEW.updated_at := app_now();
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


-- new_anomaly_proc: swap now() → app_now() while preserving the
-- one-hour batching delay introduced in migration 0049.
CREATE OR REPLACE FUNCTION apis.new_anomaly_proc() RETURNS trigger AS $$
DECLARE
  anomaly_type apis.anomaly_type;
  anomaly_action apis.anomaly_action;
  should_record_anomaly BOOLEAN := true;
  existing_job_id INT;
  existing_target_hashes JSONB;
BEGIN
  IF TG_WHEN <> 'AFTER' THEN
    RAISE EXCEPTION 'apis.new_anomaly_proc() may only run as an AFTER trigger';
  END IF;

  anomaly_type := TG_ARGV[0];
  anomaly_action := TG_ARGV[1];

  IF array_length(TG_ARGV, 1) >= 3 AND TG_ARGV[2] = 'skip_anomaly_record' THEN
    should_record_anomaly := false;
  END IF;

  IF should_record_anomaly THEN
    INSERT INTO apis.anomalies (
      project_id, anomaly_type, action, target_hash
    ) VALUES (
      NEW.project_id, anomaly_type, anomaly_action, NEW.hash
    ) ON CONFLICT (project_id, target_hash) DO NOTHING;
  END IF;

  SELECT id, payload->'targetHashes'
  INTO existing_job_id, existing_target_hashes
  FROM background_jobs
  WHERE payload->>'tag' = 'NewAnomaly'
    AND payload->>'projectId' = NEW.project_id::TEXT
    AND payload->>'anomalyType' = anomaly_type::TEXT
    AND status = 'queued'
  ORDER BY run_at ASC
  LIMIT 1;

  IF existing_job_id IS NOT NULL THEN
    UPDATE background_jobs SET payload = jsonb_build_object(
      'tag', 'NewAnomaly',
      'projectId', NEW.project_id,
      'createdAt', to_jsonb(NEW.created_at),
      'anomalyType', anomaly_type::TEXT,
      'anomalyAction', anomaly_action::TEXT,
      'targetHashes', existing_target_hashes || to_jsonb(NEW.hash)
    ) WHERE id = existing_job_id;
  ELSE
    INSERT INTO background_jobs (run_at, status, payload)
    VALUES (
      app_now() + INTERVAL '1 hour',
      'queued',
      jsonb_build_object(
        'tag', 'NewAnomaly',
        'projectId', NEW.project_id,
        'createdAt', to_jsonb(NEW.created_at),
        'anomalyType', anomaly_type::TEXT,
        'anomalyAction', anomaly_action::TEXT,
        'targetHashes', jsonb_build_array(NEW.hash)
      )
    );
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


-- check_triggered_query_monitors: swap NOW() → app_now() so monitor
-- evaluation jobs schedule against the controllable clock.
-- (Original definition: migration 0021.)
CREATE OR REPLACE PROCEDURE monitors.check_triggered_query_monitors(job_id int, config jsonb)
LANGUAGE PLPGSQL AS $$
BEGIN
    INSERT INTO background_jobs (run_at, status, payload)
    VALUES (app_now(), 'queued', jsonb_build_object('tag', 'QueryMonitorsCheck'));
    RAISE NOTICE 'Background job queued for job_id: %', job_id;
EXCEPTION
    WHEN OTHERS THEN
        RAISE EXCEPTION 'Failed to queue background job: %', SQLERRM;
END;
$$;


-- check_tests_to_trigger: swap NOW() → app_now() for the procedure's
-- internal curr_time variable.
-- (Original definition: migration 0001.)
CREATE OR REPLACE PROCEDURE tests.check_tests_to_trigger(job_id int, config JSONB) LANGUAGE PLPGSQL AS $$
DECLARE
    collection_record RECORD;
    curr_time TIMESTAMPTZ := app_now();
    next_run_at TIMESTAMPTZ;
    num_schedules INT;
BEGIN
    FOR collection_record IN
        SELECT id, schedule, last_run
        FROM tests.collections
        WHERE is_scheduled AND
              (last_run IS NULL
               OR (
                   (last_run + schedule)::timestamptz < (curr_time + INTERVAL '10 minutes')::timestamptz
                  )
              )
    LOOP
        num_schedules := 10 / (EXTRACT(EPOCH FROM collection_record.schedule) / 60);

        FOR i IN 0..num_schedules LOOP
            next_run_at := curr_time + (i * collection_record.schedule || ' minutes')::INTERVAL;
            IF next_run_at < collection_record.last_run THEN
                CONTINUE;
            END IF;

            INSERT INTO background_jobs (run_at, status, payload)
            VALUES (curr_time + (i * collection_record.schedule || ' minutes')::INTERVAL, 'queued',
                    jsonb_build_object('tag', 'RunCollectionTests', 'contents', collection_record.id));
        END LOOP;

        IF next_run_at < collection_record.last_run THEN
            UPDATE tests.collections
            SET last_run = next_run_at
            WHERE id = collection_record.id;
        END IF;
    END LOOP;
END;
$$;
