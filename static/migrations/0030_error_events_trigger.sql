BEGIN;

-- Trigger function to create error_events from error_data JSONB on errors table
-- The error_data column contains ATError structure with event-specific details
CREATE OR REPLACE FUNCTION apis.create_error_event_proc() RETURNS trigger AS $$
BEGIN
  IF TG_WHEN <> 'AFTER' THEN
    RAISE EXCEPTION 'apis.create_error_event_proc() may only run as an AFTER trigger';
  END IF;

  IF NEW.error_data IS NOT NULL AND NEW.error_data != '{}'::jsonb THEN
    INSERT INTO apis.error_events (
      project_id,
      occurred_at,
      target_hash,
      exception_type,
      message,
      stack_trace,
      service_name,
      environment,
      request_method,
      request_path,
      endpoint_hash,
      trace_id,
      span_id,
      parent_span_id,
      user_id,
      user_email,
      user_ip,
      session_id,
      sample_rate
    ) VALUES (
      NEW.project_id,
      COALESCE((NEW.error_data->>'when')::timestamptz, NOW()),
      NEW.hash,
      COALESCE(NEW.error_data->>'root_exception_type', NEW.error_data->>'error_type', NEW.error_type),
      COALESCE(NEW.error_data->>'root_exception_message', NEW.error_data->>'message', NEW.message),
      COALESCE(NEW.error_data->>'stack_trace', NEW.stacktrace),
      COALESCE(NEW.error_data->>'service_name', NEW.service, 'unknown'),
      COALESCE(NEW.error_data->>'environment', NEW.environment),
      NEW.error_data->>'request_method',
      NEW.error_data->>'request_path',
      NEW.error_data->>'endpoint_hash',
      NEW.error_data->>'trace_id',
      NEW.error_data->>'span_id',
      NEW.error_data->>'parent_span_id',
      NEW.error_data->>'user_id',
      NEW.error_data->>'user_email',
      -- Handle user_ip carefully - convert to INET if valid, otherwise NULL
      CASE
        WHEN NEW.error_data->>'user_ip' IS NOT NULL
             AND NEW.error_data->>'user_ip' != ''
        THEN (NEW.error_data->>'user_ip')::inet
        ELSE NULL
      END,
      NEW.error_data->>'session_id',
      1.0 -- default sample rate
    );
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER error_insert_create_event AFTER INSERT ON apis.errors FOR EACH ROW EXECUTE PROCEDURE apis.create_error_event_proc();
CREATE TRIGGER error_update_create_event AFTER UPDATE OF error_data ON apis.errors FOR EACH ROW EXECUTE PROCEDURE apis.create_error_event_proc();

COMMIT;
