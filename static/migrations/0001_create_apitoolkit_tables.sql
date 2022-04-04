BEGIN;

CREATE EXTENSION IF NOT EXISTS citext;
CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE;
CREATE EXTENSION IF NOT EXISTS timescaledb_toolkit;
CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
CREATE EXTENSION IF NOT EXISTS hstore;


-- create schemas
CREATE SCHEMA IF NOT EXISTS users;
CREATE SCHEMA IF NOT EXISTS projects;
CREATE SCHEMA IF NOT EXISTS apis;


-----------------------------------------------------------------------
-- HELPER FUNCTIONS AND DOMAIN. 
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION manage_updated_at(_tbl regclass) RETURNS VOID AS $$
BEGIN
  EXECUTE format('CREATE TRIGGER set_updated_at BEFORE UPDATE ON %s
FOR EACH ROW EXECUTE PROCEDURE set_updated_at()', _tbl);
END;

-- create function to automatically set updated at in trigger
$$ LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION set_updated_at() RETURNS trigger AS $$
BEGIN
  IF (
    NEW IS DISTINCT FROM OLD AND
    NEW.updated_at IS NOT DISTINCT FROM OLD.updated_at
  ) THEN
    NEW.updated_at := current_timestamp;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


DO $$ BEGIN
  CREATE DOMAIN email AS citext
    CHECK ( value ~ '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$' );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

-----------------------------------------------------------------------
-- USERS TABLE 
-- query patterns:
-- -- 
-----------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS users.users
(
  id UUID NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  deleted_at TIMESTAMP WITH TIME ZONE,
  active BOOL NOT NULL DEFAULT 't',
  first_name VARCHAR(100) NOT NULL DEFAULT '',
  last_name VARCHAR(100) NOT NULL DEFAULT '',
  display_image_url TEXT NOT NULL DEFAULT '',
  email email NOT NULL UNIQUE
);
SELECT manage_updated_at('users.users');

-----------------------------------------------------------------------
-- PROJECT PERSISTENT SESSION 
-----------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS users.persistent_sessions
(
  id UUID NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  user_id  UUID NOT NULL REFERENCES users.users ON DELETE CASCADE,
  session_data JSONB NOT NULL DEFAULT '{}' 
);
SELECT manage_updated_at('users.persistent_sessions');


CREATE TABLE IF NOT EXISTS projects.projects
(
  id UUID NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  deleted_at TIMESTAMP WITH TIME ZONE,
  active BOOL NOT NULL DEFAULT 't',
  title TEXT NOT NULL DEFAULT '',
  description TEXT NOT NULL DEFAULT '',
  -- FIXME: Should this hosts field be removed? We can get the list of hosts by querying the hosts in endpoints.
  -- FIXME: And it doesn't seem as straightforward how this hosts in the projects will be kept up to date efficiently.
  hosts TEXT[] NOT NULL DEFAULT '{}'
);
SELECT manage_updated_at('projects.projects');


-----------------------------------------------------------------------
-- PROJECT MEMBERS table 
-- query patterns:
-----------------------------------------------------------------------

CREATE TYPE projects.project_permissions AS ENUM ('admin', 'view', 'edit');
CREATE TABLE IF NOT EXISTS projects.project_members
(
  id UUID NOT NULL DEFAULT gen_random_uuid(),
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  deleted_at TIMESTAMP WITH TIME ZONE,
  active BOOL NOT NULL DEFAULT 't',
  project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
  user_id UUID NOT NULL REFERENCES users.users (id) ON DELETE CASCADE ON UPDATE CASCADE,
  permission projects.project_permissions NOT NULL DEFAULT 'view',
  PRIMARY KEY (project_id, user_id)
);
SELECT manage_updated_at('projects.project_members');

-----------------------------------------------------------------------
-- PROJECT API KEYS table 
-- query patterns:
-- -- API keys in a project
-----------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS projects.project_api_keys
(
  id UUID NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  deleted_at TIMESTAMP WITH TIME ZONE,
  active BOOL NOT NULL DEFAULT 't',
  project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
  title TEXT NOT NULL DEFAULT '',
  key_prefix TEXT NOT NULL DEFAULT ''
);
SELECT manage_updated_at('projects.project_api_keys');
CREATE INDEX IF NOT EXISTS idx_projects_project_api_keys_project_id ON projects.project_api_keys(project_id);

-----------------------------------------------------------------------
-- ENDPOINTS table 
-- -- Used to build the endpoint_vm which is used to display endpoint list view and endpoint stats
-----------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS apis.endpoints
(
    id uuid NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    project_id uuid NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
    url_path text NOT NULL DEFAULT ''::text,
    url_params jsonb NOT NULL DEFAULT '{}'::jsonb,
    method text NOT NULL DEFAULT 'GET'::text,
    -- Hosts is a unique set of hosts. Implemented originally as an array
    -- but then switched implementation to hstore for performance reasons
    -- https://gist.github.com/semaperepelitsa/66527f35f5127ed8dbb95974e68139b7
    -- hosts text[] NOT NULL DEFAULT '{}'::text[],
    -- a side effect of using hstore is that we can't automatically map to haskell records with ease, 
    -- so we have to always write the queries ourselves, and map them to an array.
    -- TODO: update the endpoint_request_stats materialized view to support getting keys only if needed.
    -- TODO: Although I don't think we store the hosts in the materialized view.
    hosts hstore NOT NULL DEFAULT ''::hstore,
    UNIQUE(project_id, url_path, method)
);
SELECT manage_updated_at('apis.endpoints');
CREATE INDEX IF NOT EXISTS idx_apis_endpoints_project_id ON apis.endpoints(project_id);

-----------------------------------------------------------------------
-- SHAPES table 
-----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS apis.shapes
(
    id uuid NOT NULL DEFAULT gen_random_uuid()  PRIMARY KEY,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    project_id uuid NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
    endpoint_id uuid NOT NULL,
    
    query_params_keypaths text[] NOT NULL DEFAULT '{}'::TEXT[],    
    request_body_keypaths text[] NOT NULL DEFAULT '{}'::TEXT[],    
    response_body_keypaths text[] NOT NULL DEFAULT '{}'::TEXT[],    
    request_headers_keypaths text[] NOT NULL DEFAULT '{}'::TEXT[],    
    response_headers_keypaths text[] NOT NULL DEFAULT '{}'::TEXT[],

    -- A shape is the combination of all the keypaths accross all the different fields
    UNIQUE(project_id, endpoint_id, query_params_keypaths, request_body_keypaths, response_body_keypaths, request_headers_keypaths, response_headers_keypaths)
);
SELECT manage_updated_at('apis.shapes');
CREATE INDEX IF NOT EXISTS idx_apis_shapes_project_id ON apis.shapes(project_id);

-----------------------------------------------------------------------
-- FIELDS table 
-----------------------------------------------------------------------
CREATE TYPE apis.field_type AS ENUM ('unknown','string','number','bool','object', 'list', 'null', 'number_list', 'string_list');
CREATE TYPE apis.field_category AS ENUM ('path_param','query_param', 'request_header','response_header', 'request_body', 'response_body');
CREATE TABLE IF NOT EXISTS apis.fields
(
    id uuid NOT NULL DEFAULT gen_random_uuid()  PRIMARY KEY,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    project_id uuid NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
    endpoint_id uuid NOT NULL,
    key text  NOT NULL DEFAULT ''::text,
    field_type apis.field_type NOT NULL DEFAULT 'unknown'::apis.field_type,
    field_type_override text,
    format text NOT NULL DEFAULT 'none'::text,
    format_override text NOT NULL DEFAULT '',
    description text NOT NULL DEFAULT ''::text,
    key_path text[] NOT NULL DEFAULT '{}'::text[],
    key_path_str text NOT NULL DEFAULT '',
    field_category apis.field_category NOT NULL DEFAULT 'request_body'::apis.field_category,
    UNIQUE (project_id, endpoint_id, field_category, key_path_str, format)
);
SELECT manage_updated_at('apis.fields');
CREATE INDEX IF NOT EXISTS idx_apis_fields_project_id ON apis.fields(project_id);
-----------------------------------------------------------------------
-- FORMATS table 
-----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS apis.formats
(
  id uuid NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  deleted_at TIMESTAMP WITH TIME ZONE,
  project_id uuid NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
  field_id uuid NOT NULL REFERENCES apis.fields (id) ON DELETE CASCADE,
  field_type apis.field_type NOT NULL DEFAULT 'unknown'::apis.field_type,
  field_format text NOT NULL DEFAULT '',
  examples text[] NOT NULL DEFAULT '{}'::text[],
  UNIQUE (project_id, field_id, field_format)
);
SELECT manage_updated_at('apis.formats');

-----------------------------------------------------------------------
-- ANOMALIES table 
-----------------------------------------------------------------------
CREATE TYPE apis.anomaly_type AS ENUM ('unknown', 'field', 'endpoint','shape', 'format');
CREATE TYPE apis.anomaly_action AS ENUM ('unknown', 'created');
CREATE TABLE IF NOT EXISTS apis.anomalies
(
  id uuid NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  project_id uuid NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
  acknowleged_at TIMESTAMP WITH TIME ZONE,
  acknowleged_by UUID REFERENCES users.users (id), -- user who acknowleges the anomaly
  anomaly_type apis.anomaly_type NOT NULL DEFAULT 'unknown'::apis.anomaly_type,
  action apis.anomaly_action NOT NULL DEFAULT 'unknown'::apis.anomaly_action,
  target_id uuid,
  archived_at TIMESTAMP WITH TIME ZONE
);
SELECT manage_updated_at('apis.anomalies');



CREATE FUNCTION apis.new_anomaly_proc() RETURNS trigger AS $$
DECLARE 
	anomaly_type apis.anomaly_type;
	anomaly_action apis.anomaly_action;
BEGIN
    IF TG_WHEN <> 'AFTER' THEN
        RAISE EXCEPTION 'apis.new_anomaly_proc() may only run as an AFTER trigger';
    END IF;
	anomaly_type := TG_ARGV[0];
	anomaly_action := TG_ARGV[1];
    INSERT INTO apis.anomalies (project_id, anomaly_type, action, target_id) VALUES (NEW.project_id, anomaly_type, anomaly_action, NEW.id);
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS fields_created_anomaly ON apis.fields;
CREATE TRIGGER fields_created_anomaly AFTER INSERT ON apis.fields FOR EACH ROW EXECUTE PROCEDURE apis.new_anomaly_proc('field', 'created');

DROP TRIGGER IF EXISTS format_created_anomaly ON apis.formats;
CREATE TRIGGER format_created_anomaly AFTER INSERT ON apis.formats FOR EACH ROW EXECUTE PROCEDURE apis.new_anomaly_proc('format', 'created');

DROP TRIGGER IF EXISTS endpoint_created_anomaly ON apis.endpoints;
CREATE TRIGGER endpoint_created_anomaly AFTER INSERT ON apis.endpoints FOR EACH ROW EXECUTE PROCEDURE apis.new_anomaly_proc('endpoint', 'created');

DROP TRIGGER IF EXISTS shapes_created_anomaly ON apis.shapes;
CREATE TRIGGER shapes_created_anomaly AFTER INSERT ON apis.shapes FOR EACH ROW EXECUTE PROCEDURE apis.new_anomaly_proc('shape', 'created');

-----------------------------------------------------------------------
-- Single function to generate fields and formats within postgres while
-- reducing the round trips.
-----------------------------------------------------------------------

CREATE OR REPLACE FUNCTION apis.create_field_and_formats(
  i_project_id UUID, i_endpoint_id UUID, i_key TEXT, i_field_type apis.field_type, i_field_type_override TEXT, 
  i_format TEXT, i_format_override TEXT, i_description TEXT, i_key_path TEXT[], i_key_path_str TEXT, 
  i_field_category apis.field_category, i_examples TEXT[], i_examples_max_count INT 
)
RETURNS TABLE (fmt_id UUID, f_id UUID) AS $$
BEGIN
  return query
  with returned_fields AS (
    INSERT INTO apis.fields (project_id, endpoint_id, key, field_type, field_type_override, format, format_override, description, key_path, key_path_str, field_category)
      VALUES(i_project_id, i_endpoint_id, i_key, i_field_type, i_field_type_override, i_format, i_format_override, i_description, i_key_path, i_key_path_str, i_field_category)
      ON CONFLICT (project_id, endpoint_id, field_category, key_path_str,format) DO NOTHING
    RETURNING project_id, id, field_type, format,i_examples
  ), current_fields AS (
    SELECT * FROM returned_fields
      UNION ALL
	SELECT project_id, id, field_type, format,i_examples
     FROM apis.fields
     WHERE  project_id=i_project_id AND endpoint_id=i_endpoint_id AND key_path_str=i_key_path_str AND format=i_format -- only executed if no INSERT
    LIMIT  1
  )
 INSERT INTO apis.formats (project_id, field_id, field_type, field_format, examples)
  SELECT * from current_fields
		ON CONFLICT (project_id, field_id, field_format)
		DO
			UPDATE SET 
				examples = ARRAY(SELECT DISTINCT e from unnest(apis.formats.examples || excluded.examples) as e order by e limit 5)
	RETURNING id, field_id;

END;
$$ LANGUAGE plpgsql;

----------------------------------------------------------------------------------------------------------
-- apis.request_dumps table holds a timeseries dump of all requests that come into the backend.
-- We rely on it heavily, for ploting time series graphs to show changes in shapes and endpoint fields over time.
----------------------------------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS apis.request_dumps
(
    id uuid NOT NULL DEFAULT gen_random_uuid(),
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    project_id uuid NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
    host text NOT NULL DEFAULT '',
    url_path text NOT NULL DEFAULT '',
    raw_url text NOT NULL DEFAULT '',
    path_params jsonb NOT NULL DEFAULT '{}',
    method text NOT NULL DEFAULT '',
    referer text NOT NULL DEFAULT '',
    proto_major int NOT NULL DEFAULT 0,
    proto_minor int NOT NULL DEFAULT 0,
    duration interval,
    status_code int NOT NULL DEFAULT 0,

    query_params jsonb NOT NULL DEFAULT '{}'::jsonb,
    request_body jsonb NOT NULL DEFAULT '{}'::jsonb,
    response_body jsonb NOT NULL DEFAULT '{}'::jsonb,
    request_headers jsonb NOT NULL DEFAULT '{}'::jsonb,
    response_headers jsonb NOT NULL DEFAULT '{}'::jsonb,

    query_params_keypaths text[] NOT NULL DEFAULT '{}'::TEXT[],    
    request_body_keypaths text[] NOT NULL DEFAULT '{}'::TEXT[],    
    response_body_keypaths text[] NOT NULL DEFAULT '{}'::TEXT[],    
    request_headers_keypaths text[] NOT NULL DEFAULT '{}'::TEXT[],    
    response_headers_keypaths text[] NOT NULL DEFAULT '{}'::TEXT[],

    shape_id uuid NOT NULL REFERENCES apis.shapes (id),
    
    format_ids uuid[] NOT NULL DEFAULT '{}'::UUID[],
    field_ids uuid[] NOT NULL DEFAULT '{}'::UUID[],

    PRIMARY KEY(project_id,created_at,id)
);
SELECT manage_updated_at('apis.request_dumps');
SELECT create_hypertable('apis.request_dumps', 'created_at');
CREATE INDEX IF NOT EXISTS idx_apis_request_dumps_project_id ON apis.request_dumps(project_id, created_at);
CREATE INDEX IF NOT EXISTS idx_apis_request_dumps_shape_id ON apis.request_dumps(shape_id, created_at);
CREATE INDEX IF NOT EXISTS idx_apis_request_dumps_format_ids ON apis.request_dumps(format_ids, created_at);
CREATE INDEX IF NOT EXISTS idx_apis_request_dumps_field_ids ON apis.request_dumps(field_ids, created_at);
CREATE INDEX IF NOT EXISTS idx_apis_request_dumps_endpointss ON apis.request_dumps(url_path,method, created_at);


-- Create a view that tracks endpoint related statistic points from the request dump table.
DROP MATERIALIZED VIEW IF EXISTS apis.endpoint_request_stats;
CREATE MATERIALIZED VIEW apis.endpoint_request_stats AS 
    WITH request_dump_stats as (
        SELECT
            project_id, url_path, method,
            percentile_agg(EXTRACT(epoch FROM duration)) as agg,
            sum(EXTRACT(epoch FROM duration))  as total_time,
            count(1)  as total_requests,
            sum(sum(EXTRACT(epoch FROM duration))) OVER (partition by project_id) as total_time_proj,
            sum(count(*)) OVER (partition by project_id) as total_requests_proj
        FROM apis.request_dumps
        where created_at > NOW() - interval '14' day
        GROUP BY project_id, url_path, method
    )
    SELECT 	
        enp.id endpoint_id,
        rds.project_id,
        rds.url_path, rds.method,
        coalesce(approx_percentile(0,    agg)/1000000, 0) min,
        coalesce(approx_percentile(0.50, agg)/1000000, 0) p50,
        coalesce(approx_percentile(0.75, agg)/1000000, 0) p75,
        coalesce(approx_percentile(0.90, agg)/1000000, 0) p90,
        coalesce(approx_percentile(0.95, agg)/1000000, 0) p95,
        coalesce(approx_percentile(0.99, agg)/1000000, 0) p99,
        coalesce(approx_percentile(1,    agg)/1000000, 0) max,
        CAST (total_time/1000000 AS FLOAT8) total_time,
        CAST (total_time_proj/1000000  AS FLOAT8) total_time_proj,
        CAST (total_requests AS INT),
        CAST (total_requests_proj AS INT)
    FROM request_dump_stats rds
    JOIN apis.endpoints enp on (rds.project_id=enp.project_id AND rds.method=enp.method AND rds.url_path=enp.url_path);
CREATE INDEX IF NOT EXISTS idx_apis_endpoint_request_stats_project_id ON apis.endpoint_request_stats(project_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_endpoint_request_stats_endpoint_id ON apis.endpoint_request_stats(endpoint_id);

-- Create a view that tracks project request related statistic points from the request dump table.
DROP MATERIALIZED VIEW IF EXISTS apis.project_request_stats;
CREATE MATERIALIZED VIEW apis.project_request_stats AS 
  WITH request_dump_stats as (
      SELECT
          project_id,
          percentile_agg(EXTRACT(epoch FROM duration)) as agg,
          sum(EXTRACT(epoch FROM duration))  as total_time,
          count(1)  as total_requests
      FROM apis.request_dumps
      where created_at > NOW() - interval '14' day
      GROUP BY project_id
    )
  SELECT 	
    rds.project_id,
    coalesce(approx_percentile(0,    agg)/1000000, 0) min,
    coalesce(approx_percentile(0.50, agg)/1000000, 0) p50,
    coalesce(approx_percentile(0.75, agg)/1000000, 0) p75,
    coalesce(approx_percentile(0.90, agg)/1000000, 0) p90,
    coalesce(approx_percentile(0.95, agg)/1000000, 0) p95,
    coalesce(approx_percentile(0.99, agg)/1000000, 0) p99,
    coalesce(approx_percentile(1,    agg)/1000000, 0) max,
    CAST    (total_time/1000000   AS FLOAT8) total_time,
    CAST    (total_requests       AS INT) total_requests,
    (select count(*) from apis.endpoints enp where rds.project_id=enp.project_id) total_endpoints,
    (select count(*) from apis.endpoints enp where rds.project_id=enp.project_id and created_at<=NOW()::DATE-7) total_endpoints_last_week,
    (select count(*) from apis.shapes    sh  where rds.project_id=sh.project_id) total_shapes,
    (select count(*) from apis.shapes    sh  where rds.project_id=sh.project_id and created_at<=NOW()::DATE-7) total_shapes_last_week,
    (select count(*) from apis.anomalies ann where rds.project_id=ann.project_id) total_anomalies,
    (select count(*) from apis.anomalies ann where rds.project_id=ann.project_id and created_at<=NOW()::DATE-7) total_anomalies_last_week,
    (select count(*) from apis.anomalies ann where rds.project_id=ann.project_id) total_fields,
    (select count(*) from apis.anomalies ann where rds.project_id=ann.project_id and created_at<=NOW()::DATE-7) total_fields_last_week
  FROM request_dump_stats rds;
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_project_request_stats_project_id ON apis.project_request_stats(project_id);

-- Create a view that tracks project request related statistic points from the request dump table.
DROP MATERIALIZED VIEW IF EXISTS apis.project_requests_by_endpoint_per_min;
CREATE MATERIALIZED VIEW apis.project_requests_by_endpoint_per_min AS 
  SELECT
      project_id,
      json_agg(json_build_array(timeB,enp, count))::text ts_text
  FROM
      (
          SELECT
              time_bucket('1 minute', created_at) as timeB,
              concat_ws(' ', method, url_path)::text enp,
              count(id) count,
              project_id
          FROM
              apis.request_dumps
          where
              created_at > NOW() - interval '14' day --and project_id=?
          GROUP BY
              project_id,
              timeB,
              method,
              url_path
      ) ts_data
      GROUP BY project_id;
CREATE INDEX IF NOT EXISTS idx_apis_project_requests_by_endpoint_per_min_project_id ON apis.project_requests_by_endpoint_per_min(project_id);


-- TODO: Create triggers to create new anomalies when new fields, endpoints and shapes are created.


CREATE MATERIALIZED VIEW IF NOT EXISTS apis.anomalies_vm 
    AS	SELECT 
    an.id, an.created_at, an.updated_at, an.project_id, an.acknowleged_at,an.acknowleged_by, an.anomaly_type, an.action, an.target_id,
    shapes.id shape_id,

    fields.id field_id,
    fields.key field_key,
    fields.key_path_str field_key_path_str,
    fields.field_category field_category,
    fields.format field_format,
    
    formats.id format_id,
    formats.field_type format_type,
    formats.examples format_examples,

    endpoints.id endpoint_id,
    endpoints.method endpoint_method,
    endpoints.url_path endpoint_url_path,

    an.archived_at,
    (
      SELECT
          json_agg(json_build_array(timeB, count))
      from
          (
              SELECT
                  time_bucket('1 minute', created_at) as timeB,
                  count(id) count
              FROM
                  apis.request_dumps
              where
                  created_at > NOW() - interval '14' day
                  AND project_id = project_id
                  AND CASE
                      WHEN anomaly_type = 'endpoint' THEN 
                          method = method
                          AND url_path = url_path
                      WHEN anomaly_type = 'shape' THEN
                          shape_id = target_id
                      WHEN anomaly_type = 'format' THEN
                          target_id = ANY(format_ids)
                  END
              GROUP BY
                  timeB
          ) ts 
    )::text ts
	FROM apis.anomalies an
	LEFT JOIN apis.formats on target_id=formats.id
	LEFT JOIN apis.fields on (fields.id=target_id OR fields.id=formats.field_id) 
	LEFT JOIN apis.shapes on target_id=shapes.id
	LEFT JOIN apis.endpoints 
      ON (endpoints.id = target_id 
          OR endpoints.id = fields.endpoint_id 
          OR endpoints.id = shapes.endpoint_id
          );
CREATE UNIQUE INDEX idx_apis_anomaly_vm_id ON apis.anomalies_vm (id);
CREATE INDEX idx_apis_anomaly_vm_project_id ON apis.anomalies_vm (project_id);
CREATE INDEX idx_apis_anomaly_vm_project_id_endpoint_id ON apis.anomalies_vm (project_id, endpoint_id);



-- REFRESH VIEWS
-- CREATE OR REPLACE FUNCTION apis.refresh_anomalies_vm()
--     RETURNS TRIGGER
--     LANGUAGE PLPGSQL
-- AS
-- $$
-- BEGIN
--     REFRESH MATERIALIZED VIEW CONCURRENTLY apis.anomalies_vm;
--     RETURN NEW;
-- END;
-- $$;

-- README: commented out in favour of refreshing every 5mins, because it includes time series data
-- CREATE TRIGGER anomaly_update 
--   AFTER UPDATE OR INSERT ON apis.anomalies
--   FOR EACH STATEMENT EXECUTE PROCEDURE apis.refresh_anomalies_vm();




CREATE OR REPLACE PROCEDURE apis.refresh_request_dump_views_every_5mins(job_id int, config jsonb) LANGUAGE PLPGSQL AS
$$
BEGIN
  RAISE NOTICE 'Executing action % with config %', job_id, config;
  REFRESH MATERIALIZED VIEW CONCURRENTLY apis.endpoint_request_stats;
  REFRESH MATERIALIZED VIEW CONCURRENTLY apis.project_request_stats;
  REFRESH MATERIALIZED VIEW CONCURRENTLY apis.project_requests_by_endpoint_per_min;
END
$$;
-- Refresg view every 5mins
SELECT add_job('apis.refresh_request_dump_views_every_5mins','5min');

CREATE OR REPLACE PROCEDURE apis.refresh_request_dump_views_every_2mins(job_id int, config jsonb) LANGUAGE PLPGSQL AS
$$
BEGIN
  RAISE NOTICE 'Executing action % with config %', job_id, config;
  REFRESH MATERIALIZED VIEW CONCURRENTLY apis.anomalies_vm;
END
$$;
-- Refresg view every 5mins
SELECT add_job('apis.refresh_request_dump_views_every_2mins','2min');

COMMIT;
