BEGIN;

CREATE EXTENSION IF NOT EXISTS citext;
CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE;
CREATE EXTENSION IF NOT EXISTS timescaledb_toolkit;
CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
CREATE EXTENSION IF NOT EXISTS hstore;
CREATE EXTENSION IF NOT EXISTS tablefunc;
-- CREATE EXTENSION IF NOT EXISTS pg_cron;


-- create schemas
CREATE SCHEMA IF NOT EXISTS users;
CREATE SCHEMA IF NOT EXISTS projects;
CREATE SCHEMA IF NOT EXISTS apis;


-----------------------------------------------------------------------
-- HELPER FUNCTIONS AND DOMAIN. 
-----------------------------------------------------------------------
-----------------------------------------------------------------------
--make manage_updated_at reusable
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION manage_updated_at(_tbl regclass) RETURNS VOID AS $$
BEGIN
  EXECUTE format('CREATE TRIGGER set_updated_at BEFORE UPDATE ON %s
FOR EACH ROW EXECUTE PROCEDURE set_updated_at()', _tbl);
  EXCEPTION
    WHEN others THEN null;
END;
$$ LANGUAGE plpgsql;

--------------------------------------------------------------------

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

-- end -------------------------------------------------------------

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
  id                UUID         NOT  NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  created_at        TIMESTAMP    WITH TIME ZONE    NOT               NULL    DEFAULT current_timestamp,
  updated_at        TIMESTAMP    WITH TIME ZONE    NOT               NULL    DEFAULT current_timestamp,
  deleted_at        TIMESTAMP    WITH TIME ZONE,
  active            BOOL         NOT  NULL DEFAULT 't',
  first_name        VARCHAR(100) NOT  NULL DEFAULT '',
  last_name         VARCHAR(100) NOT  NULL DEFAULT '',
  display_image_url TEXT         NOT  NULL DEFAULT '',
  email             email        NOT  NULL UNIQUE,
  -- Is sudo is a rough work around to mark users who will be able to see and access all projects.
  is_sudo           BOOL         NOT  NULL DEFAULT 'f'
);
SELECT manage_updated_at('users.users');

-----------------------------------------------------------------------
-- PROJECT PERSISTENT SESSION 
-----------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS users.persistent_sessions
(
  id           UUID      NOT  NULL DEFAULT    gen_random_uuid() PRIMARY KEY,
  created_at   TIMESTAMP WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
  updated_at   TIMESTAMP WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
  user_id      UUID      NOT  NULL REFERENCES users.users       ON      DELETE  CASCADE,
  session_data JSONB     NOT  NULL DEFAULT    '{}'
);
SELECT manage_updated_at('users.persistent_sessions');


CREATE TABLE IF NOT EXISTS projects.projects
(
  id          UUID      NOT  NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  created_at  TIMESTAMP WITH TIME ZONE    NOT               NULL    DEFAULT current_timestamp,
  updated_at  TIMESTAMP WITH TIME ZONE    NOT               NULL    DEFAULT current_timestamp,
  deleted_at  TIMESTAMP WITH TIME ZONE,
  active      BOOL      NOT  NULL DEFAULT 't',
  title       TEXT      NOT  NULL DEFAULT '',
  description TEXT      NOT  NULL DEFAULT '',
  -- We originally stored the hosts lists under projects table, 
  -- but realised that it is immensely difficult to keep the hosts up to date under projects,
  -- when we don't update projects with requests data. So we store hosts under the endpoints now.
  -- The hosts can be queried from endpoints when needed.
  payment_plan TEXT NOT NULL DEFAULT 'Free'
);
SELECT manage_updated_at('projects.projects');

-----------------------------------------------------------------------
-- PROJECT MEMBERS table 
-- query patterns:
-----------------------------------------------------------------------

CREATE TYPE projects.project_permissions AS ENUM ('admin', 'view', 'edit');
CREATE TABLE IF NOT EXISTS projects.project_members
(
  id         UUID                         NOT          NULL DEFAULT    gen_random_uuid(),
  created_at TIMESTAMP                    WITH         TIME ZONE       NOT               NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP                    WITH         TIME ZONE       NOT               NULL DEFAULT current_timestamp,
  deleted_at TIMESTAMP                    WITH         TIME ZONE,
  active     BOOL                         NOT          NULL DEFAULT    't',
  project_id UUID                         NOT          NULL REFERENCES projects.projects (id) ON      DELETE CASCADE,
  user_id    UUID                         NOT          NULL REFERENCES users.users       (id) ON      DELETE CASCADE ON UPDATE CASCADE,
  permission projects.project_permissions NOT          NULL DEFAULT    'view',
  PRIMARY    KEY                          (project_id, user_id)
);
SELECT manage_updated_at('projects.project_members');

-----------------------------------------------------------------------
-- PROJECT API KEYS table 
-- query patterns:
-- -- API keys in a project
-----------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS projects.project_api_keys
(
  id         UUID      NOT  NULL DEFAULT    gen_random_uuid() PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
  deleted_at TIMESTAMP WITH TIME ZONE,
  active     BOOL      NOT  NULL DEFAULT    't',
  project_id UUID      NOT  NULL REFERENCES projects.projects (id)    ON      DELETE CASCADE,
  title      TEXT      NOT  NULL DEFAULT    '',
  key_prefix TEXT      NOT  NULL DEFAULT    ''
);
SELECT manage_updated_at('projects.project_api_keys');
CREATE INDEX IF NOT EXISTS idx_projects_project_api_keys_project_id ON projects.project_api_keys(project_id);

------------------------------------------------------------------------
-- Swagger JSON table
-- query patterns:
-- -- a project's swagger uploads history
-- -- generate a projects recent swagger
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS apis.swagger_jsons
(
  id           UUID      NOT  NULL DEFAULT    gen_random_uuid() PRIMARY KEY,
  created_at   TIMESTAMP WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
  updated_at   TIMESTAMP WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
  created_by   UUID                           NOT          NULL REFERENCES users.users       (id) ON      DELETE CASCADE ON UPDATE CASCADE,
  project_id   UUID      NOT  NULL REFERENCES projects.projects (id)    ON      DELETE CASCADE,
  swagger_json JSONB     NOT  NULL
);

SELECT manage_updated_at('apis.swagger_jsons');
CREATE INDEX IF NOT EXISTS idx_swagger_jsons_project_id ON apis.swagger_jsons(project_id);

-----------------------------------------------------------------------
-- ENDPOINTS table 
-- -- Used to build the endpoint_vm which is used to display endpoint list view and endpoint stats
-----------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS apis.endpoints
(
    id         uuid      NOT  NULL DEFAULT    gen_random_uuid() PRIMARY KEY,
    created_at TIMESTAMP WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
    updated_at TIMESTAMP WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
    project_id uuid      NOT  NULL REFERENCES projects.projects (id)    ON      DELETE CASCADE,
    url_path   text      NOT  NULL DEFAULT    ''::text,
    url_params jsonb     NOT  NULL DEFAULT    '{}'::jsonb,
    method     text      NOT  NULL DEFAULT    'GET'::text,
    -- Hosts is a unique set of hosts. Implemented originally as an array
    -- but then switched implementation to hstore for performance reasons
    -- https://gist.github.com/semaperepelitsa/66527f35f5127ed8dbb95974e68139b7
    -- hosts text[] NOT NULL DEFAULT '{}'::text[],
    -- a side effect of using hstore is that we can't automatically map to haskell records with ease, 
    -- so we have to always write the queries ourselves, and map them to an array.
    -- TODO: update the endpoint_request_stats materialized view to support getting keys only if needed.
    -- TODO: Although I don't think we store the hosts in the materialized view.
    hosts hstore NOT NULL DEFAULT ''::hstore,

    -- the hash will represent an xxhash of the project_id and method and the url_path
    -- it will be used as the main identifier for endpoints since it can be generated deterministically
    -- without having to check the value from the database.
    hash text NOT NULL DEFAULT ''::text,
    UNIQUE(project_id, url_path, method)
);
SELECT manage_updated_at('apis.endpoints');
CREATE INDEX IF NOT EXISTS idx_apis_endpoints_project_id ON apis.endpoints(project_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_endpoints_hash ON apis.endpoints(hash);

-----------------------------------------------------------------------
-- SHAPES table 
-----------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS apis.shapes
(
    id                        uuid      NOT  NULL DEFAULT    gen_random_uuid() PRIMARY KEY,
    created_at                TIMESTAMP WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
    updated_at                TIMESTAMP WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
    approved_on               TIMESTAMP WITH TIME ZONE                                 DEFAULT NULL,
    project_id                uuid      NOT  NULL REFERENCES projects.projects (id)    ON      DELETE CASCADE,
    endpoint_hash             text      NOT  NULL,
    
    query_params_keypaths     text[]    NOT  NULL DEFAULT    '{}'::TEXT[],
    request_body_keypaths     text[]    NOT  NULL DEFAULT    '{}'::TEXT[],
    response_body_keypaths    text[]    NOT  NULL DEFAULT    '{}'::TEXT[],
    request_headers_keypaths  text[]    NOT  NULL DEFAULT    '{}'::TEXT[],
    response_headers_keypaths text[]    NOT  NULL DEFAULT    '{}'::TEXT[],

    -- shape hash is a hash of all the key paths put together into a single sorted list and hashed
    -- We skip the request headers from the shape hash, since a lot of sources might add unnecessary hashes at anytime
    -- the final hash is the hash as described above, but with the endpoint hash prepended to it. 
    -- This opens up prefix search possibilities as well, for shapes.
    hash text NOT NULL DEFAULT ''::TEXT,
    field_hashes              text[]    NOT  NULL DEFAULT    '{}'::TEXT[],
    -- All fields which are showing up for the first time on this endpoint
    new_unique_fields  text[] NOT NULL DEFAULT '{}'::TEXT[],
    -- All fields which were usually sent for all other requests on this endpoint, but are no longer being sent.
    deleted_fields     text[] NOT NULL DEFAULT '{}'::TEXT[],
    -- All fields associated with this shape which are updates
    updated_field_formats     text[] NOT NULL DEFAULT '{}'::TEXT[],
    status_code               int DEFAULT 0
);
SELECT manage_updated_at('apis.shapes');
CREATE INDEX IF NOT EXISTS idx_apis_shapes_project_id ON apis.shapes(project_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_shapes_hash ON apis.shapes(hash);

-----------------------------------------------------------------------
-- FIELDS table 
-----------------------------------------------------------------------

CREATE TYPE apis.field_type AS ENUM ('unknown','string','number','bool','object', 'list', 'null');
CREATE TYPE apis.field_category AS ENUM ('path_param','query_param', 'request_header','response_header', 'request_body', 'response_body');
CREATE TABLE IF NOT EXISTS apis.fields
(
    id                  uuid            NOT  NULL DEFAULT    gen_random_uuid() PRIMARY KEY,
    created_at          TIMESTAMP       WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
    updated_at          TIMESTAMP       WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
    project_id          uuid            NOT  NULL REFERENCES projects.projects (id)    ON      DELETE CASCADE,
    endpoint_hash       text            NOT  NULL,
    key                 text            NOT  NULL DEFAULT    ''::text,
    field_type          apis.field_type NOT  NULL DEFAULT    'unknown'::apis.field_type,
    field_type_override text,
    -- I'm forgetting what this format is. But I think it should be the mask of the field format. 
    -- And should sort of mirror the formats table?
    format              text            NOT  NULL DEFAULT    'none'::text,
    format_override     text            NOT  NULL DEFAULT    '',
    description         text            NOT  NULL DEFAULT    ''::text,
    key_path        text            NOT  NULL DEFAULT    '',
    field_category apis.field_category NOT NULL DEFAULT 'request_body'::apis.field_category,

    -- the hash of a field is the <hash of the endpoint> + <the hash of <field_category>,<key_path_str>,<field_type>>
    hash text NOT NULL DEFAULT ''::TEXT
);
SELECT manage_updated_at('apis.fields');
CREATE INDEX IF NOT EXISTS idx_apis_fields_project_id ON apis.fields(project_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_fields_hash ON apis.fields(hash);


-----------------------------------------------------------------------
-- FORMATS table 
-----------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS apis.formats
(
  id           uuid            NOT  NULL DEFAULT    gen_random_uuid() PRIMARY KEY,
  created_at   TIMESTAMP       WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
  updated_at   TIMESTAMP       WITH TIME ZONE       NOT               NULL    DEFAULT current_timestamp,
  deleted_at   TIMESTAMP       WITH TIME ZONE,
  project_id   uuid            NOT  NULL REFERENCES projects.projects (id)    ON      DELETE CASCADE,
  field_hash   TEXT            NOT  NULL,
  field_type   apis.field_type NOT  NULL DEFAULT    'unknown'::apis.field_type,
  field_format text            NOT  NULL DEFAULT    '',
  examples     text[]          NOT  NULL DEFAULT    '{}'::text[],

  -- hash for formats will be the <field hash> + <field_format hash> 
  hash         text            NOT  NULL DEFAULT    ''::TEXT,

  UNIQUE (project_id, field_hash, field_format)
);
SELECT manage_updated_at('apis.formats');
CREATE INDEX IF NOT EXISTS idx_apis_formats_project_id ON apis.formats(project_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_formats_hash ON apis.formats(hash);

-----------------------------------------------------------------------
-- ANOMALIES table 
-----------------------------------------------------------------------

CREATE TYPE apis.anomaly_type AS ENUM ('unknown', 'field', 'endpoint','shape', 'format');
CREATE TYPE apis.anomaly_action AS ENUM ('unknown', 'created');
CREATE TABLE IF NOT EXISTS apis.anomalies
(
  id             uuid                NOT        NULL        DEFAULT    gen_random_uuid() PRIMARY KEY,
  created_at     TIMESTAMP           WITH       TIME        ZONE       NOT               NULL    DEFAULT current_timestamp,
  updated_at     TIMESTAMP           WITH       TIME        ZONE       NOT               NULL    DEFAULT current_timestamp,
  project_id     uuid                NOT        NULL        REFERENCES projects.projects (id)    ON      DELETE CASCADE,
  acknowleged_at TIMESTAMP           WITH       TIME        ZONE,
  acknowleged_by UUID                REFERENCES users.users (id),      -- user who acknowleges the anomaly
  anomaly_type   apis.anomaly_type   NOT        NULL        DEFAULT    'unknown'::apis.anomaly_type,
  action         apis.anomaly_action NOT        NULL        DEFAULT    'unknown'::apis.anomaly_action,
  target_hash    text,
  archived_at    TIMESTAMP           WITH       TIME        ZONE
);
SELECT manage_updated_at('apis.anomalies');
CREATE INDEX IF NOT EXISTS idx_apis_anomalies_project_id ON apis.anomalies(project_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_anomalies_target_hash ON apis.anomalies(target_hash);


CREATE OR REPLACE FUNCTION apis.new_anomaly_proc() RETURNS trigger AS $$
DECLARE 
	anomaly_type apis.anomaly_type;
	anomaly_action apis.anomaly_action;
BEGIN
    IF TG_WHEN <> 'AFTER' THEN
        RAISE EXCEPTION 'apis.new_anomaly_proc() may only run as an AFTER trigger';
    END IF;
	anomaly_type := TG_ARGV[0];
	anomaly_action := TG_ARGV[1];
    INSERT INTO apis.anomalies (project_id, anomaly_type, action, target_hash) VALUES (NEW.project_id, anomaly_type, anomaly_action, NEW.hash);
    INSERT INTO background_jobs (run_at, status, payload) VALUES (now() + INTERVAL '2 minutes', 'queued',  jsonb_build_object('tag', 'NewAnomaly', 'contents', json_build_array(NEW.project_id, NEW.created_at, anomaly_type::text, anomaly_action::text, NEW.hash)));
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

----------------------------------------------------------------------------------------------------------
-- apis.request_dumps table holds a timeseries dump of all requests that come into the backend.
-- We rely on it heavily, for ploting time series graphs to show changes in shapes and endpoint fields over time.
----------------------------------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS apis.request_dumps
(
    id                        uuid      NOT  NULL DEFAULT    gen_random_uuid(),
    created_at                TIMESTAMP WITH TIME ZONE       NOT               NULL DEFAULT current_timestamp,
    updated_at                TIMESTAMP WITH TIME ZONE       NOT               NULL DEFAULT current_timestamp,
    project_id                uuid      NOT  NULL REFERENCES projects.projects (id) ON      DELETE CASCADE,
    host                      text      NOT  NULL DEFAULT    '',
    url_path                  text      NOT  NULL DEFAULT    '',
    raw_url                   text      NOT  NULL DEFAULT    '',
    path_params               jsonb     NOT  NULL DEFAULT    '{}',
    method                    text      NOT  NULL DEFAULT    '',
    referer                   text      NOT  NULL DEFAULT    '',
    proto_major               int       NOT  NULL DEFAULT    0,
    proto_minor               int       NOT  NULL DEFAULT    0,
    duration                  interval,
    status_code               int       NOT  NULL DEFAULT    0,

    query_params              jsonb     NOT  NULL DEFAULT    '{}'::jsonb,
    request_headers           jsonb     NOT  NULL DEFAULT    '{}'::jsonb,
    response_headers          jsonb     NOT  NULL DEFAULT    '{}'::jsonb,
    request_body              jsonb     NOT  NULL DEFAULT    '{}'::jsonb,
    response_body             jsonb     NOT  NULL DEFAULT    '{}'::jsonb,
    
    -- Instead of holding ids which are difficult to compute, let's rather store their hashes only. 
    endpoint_hash             text      NOT  NULL DEFAULT    ''::text,
    shape_hash                text      NOT  NULL DEFAULT    ''::text,
    format_hashes             text[]    NOT  NULL DEFAULT    '{}'::text[],
    field_hashes              text[]    NOT  NULL DEFAULT    '{}'::text[],

    PRIMARY KEY(project_id,created_at,id)
);
SELECT manage_updated_at('apis.request_dumps');
SELECT create_hypertable('apis.request_dumps', 'created_at');
SELECT add_retention_policy('apis.request_dumps',INTERVAL '3 months',true);
CREATE INDEX IF NOT EXISTS idx_apis_request_dumps_project_id ON apis.request_dumps(project_id, created_at);


CREATE TABLE IF NOT EXISTS apis.reports 
(
    id                        uuid      NOT  NULL DEFAULT    gen_random_uuid(),
    created_at                TIMESTAMP WITH TIME ZONE       NOT               NULL DEFAULT current_timestamp,
    updated_at                TIMESTAMP WITH TIME ZONE       NOT               NULL DEFAULT current_timestamp,
    project_id                UUID      NOT  NULL REFERENCES projects.projects (id) ON      DELETE CASCADE,
    user_id                   UUID      NOT  NULL REFERENCES users.users       (id) ON      DELETE CASCADE ON UPDATE CASCADE,
    report_type               text      NOT  NULL DEFAULT    ''
    report_json               jsonb     NOT  NULL DEFAULT    '{}'::jsonb,
)

-- Create a view that tracks endpoint related statistic points from the request dump table.
DROP MATERIALIZED VIEW IF EXISTS apis.endpoint_request_stats;
CREATE MATERIALIZED VIEW apis.endpoint_request_stats AS 
 WITH request_dump_stats as (
        SELECT
            project_id, url_path, method,
	 		endpoint_hash,
            percentile_agg(EXTRACT(epoch FROM duration)) as agg,
            sum(EXTRACT(epoch FROM duration))  as total_time,
            count(1)  as total_requests,
            sum(sum(EXTRACT(epoch FROM duration))) OVER (partition by project_id) as total_time_proj,
            sum(count(*)) OVER (partition by project_id) as total_requests_proj
        FROM apis.request_dumps
        where created_at > NOW() - interval '14' day
        GROUP BY project_id, url_path, method, endpoint_hash
    )
    SELECT 	
        enp.id endpoint_id,
        enp.hash endpoint_hash,
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
	FROM apis.endpoints enp 
	JOIN request_dump_stats rds on (rds.project_id=enp.project_id AND rds.endpoint_hash=enp.hash);
  
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
    (select count(*) from apis.anomalies ann where rds.project_id=ann.project_id and ann.anomaly_type!='field') total_anomalies,
    (select count(*) from apis.anomalies ann where rds.project_id=ann.project_id and created_at<=NOW()::DATE-7) total_anomalies_last_week,
    (select count(*) from apis.anomalies ann where rds.project_id=ann.project_id) total_fields,
    (select count(*) from apis.anomalies ann where rds.project_id=ann.project_id and created_at<=NOW()::DATE-7) total_fields_last_week
  FROM request_dump_stats rds;
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_project_request_stats_project_id ON apis.project_request_stats(project_id);

-- TODO: Create triggers to create new anomalies when new fields, 
-- TODO: endpoints and shapes are created.

-- FIXME: reevaluate how anomaly_vm will work and be rendered
DROP MATERIALIZED VIEW IF EXISTS apis.anomalies_vm;
CREATE MATERIALIZED VIEW IF NOT EXISTS apis.anomalies_vm AS
SELECT
    an.id,
    an.created_at,
    an.updated_at,
    an.project_id,
    an.acknowleged_at,
    an.acknowleged_by,
    an.anomaly_type,
    an.action,
    an.target_hash,
    shapes.id shape_id,
    coalesce(shapes.new_unique_fields, '{}'::TEXT[]) new_unique_fields, 
    coalesce(shapes.deleted_fields, '{}'::TEXT[]) deleted_fields,
    coalesce(shapes.updated_field_formats, '{}'::TEXT[]) updated_field_formats,
    fields.id field_id,
    fields.key field_key,
    fields.key_path field_key_path,
    fields.field_category field_category,
    fields.format field_format,
    formats.id format_id,
    formats.field_type format_type,
    formats.examples format_examples,
    endpoints.id endpoint_id,
    endpoints.method endpoint_method,
    endpoints.url_path endpoint_url_path,
    an.archived_at
from
    apis.anomalies an
    LEFT JOIN apis.formats on (
        target_hash = formats.hash
        AND an.project_id = formats.project_id
    )
    LEFT JOIN apis.fields on (
        (
            (fields.hash = formats.field_hash )
            AND an.project_id = fields.project_id
        )
        OR fields.hash = formats.field_hash
    )
    LEFT JOIN apis.shapes on (
        target_hash = shapes.hash
        AND an.project_id = shapes.project_id
    )
    LEFT JOIN apis.endpoints ON (
        starts_with(an.target_hash, endpoints.hash)
        AND an.project_id = endpoints.project_id
    )
where
    (anomaly_type = 'endpoint')
    OR (
        anomaly_type = 'shape'
        AND endpoints.project_id = an.project_id
        AND endpoints.created_at != an.created_at
    )
    OR (
        anomaly_type = 'format'
        AND fields.project_id = an.project_id
        AND fields.created_at != an.created_at
    )
    OR NOT ( anomaly_type = ANY('{"endpoint","shape","field","format"}'::apis.anomaly_type[]));

CREATE UNIQUE INDEX idx_apis_anomaly_vm_id ON apis.anomalies_vm (id);
CREATE INDEX idx_apis_anomaly_vm_project_id ON apis.anomalies_vm (project_id);
CREATE INDEX idx_apis_anomaly_vm_anomaly_type ON apis.anomalies_vm (anomaly_type);
CREATE INDEX idx_apis_anomaly_vm_project_id_target_hash ON apis.anomalies_vm (project_id, target_hash);
CREATE INDEX idx_apis_anomaly_vm_project_id_endpoint_id ON apis.anomalies_vm (project_id, endpoint_id);

CREATE OR REPLACE PROCEDURE apis.refresh_request_dump_views_every_5mins(job_id int, config jsonb) LANGUAGE PLPGSQL AS
$$
BEGIN
  RAISE NOTICE 'Executing action % with config %', job_id, config;
  REFRESH MATERIALIZED VIEW CONCURRENTLY apis.endpoint_request_stats;
  REFRESH MATERIALIZED VIEW CONCURRENTLY apis.project_request_stats;
END
$$;
-- Refresh view every 5mins
SELECT add_job('apis.refresh_request_dump_views_every_5mins','5min');

------------------------
 CREATE OR REPLACE PROCEDURE apis.refresh_request_dump_views_every_2mins(job_id int, config jsonb) LANGUAGE PLPGSQL AS
 $$
 BEGIN
   RAISE NOTICE 'Executing action % with config %', job_id, config;
   REFRESH MATERIALIZED VIEW CONCURRENTLY apis.anomalies_vm;
 END
 $$;
 -- Refresh view every 5mins
 SELECT add_job('apis.refresh_request_dump_views_every_2mins','2min');
--------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS background_jobs 
  ( id         serial    primary key
  , created_at timestamp with    time zone    default now() not null
  , updated_at timestamp with    time zone    default now() not null
  , run_at     timestamp with    time zone    default now() not null
  , status     text      not     null
  , payload    jsonb     not     null
  , last_error jsonb     null
  , attempts   int       not     null default 0
  , locked_at  timestamp with    time zone    null
  , locked_by  text      null
  , constraint incorrect_locking_info CHECK ((status <> 'locked' and locked_at is null and locked_by is null) or (status = 'locked' and locked_at is not null and locked_by is not null)) 
  ); 

create index if not exists idx_background_jobs_created_at on background_jobs(created_at); 
create index if not exists idx_background_jobs_updated_at on background_jobs(updated_at); 
create index if not exists idx_background_jobs_locked_at on background_jobs(locked_at); 
create index if not exists idx_background_jobs_locked_by on background_jobs(locked_by); 
create index if not exists idx_background_jobs_status on background_jobs(status); 
create index if not exists idx_background_jobs_run_at on background_jobs(run_at);




create or replace function notify_job_monitor_for_background_jobs() returns trigger as $$ 
begin  
  perform pg_notify('background_jobs',  
    json_build_object('id', new.id, 'run_at', new.run_at, 'locked_at', new.locked_at)::text);  
  return new;  
end;  
$$ language plpgsql; 
drop trigger if exists trg_notify_job_monitor_for_background_jobs on background_jobs; 
create trigger trg_notify_job_monitor_for_background_jobs after insert on background_jobs for each row execute procedure notify_job_monitor_for_background_jobs();

CREATE TABLE IF NOT EXISTS projects.redacted_fields
  (                id        UUID       NOT            NULL       DEFAULT           gen_random_uuid() PRIMARY KEY,
    project_id     UUID      NOT        NULL           REFERENCES projects.projects (id)              ON      DELETE CASCADE,
    created_at     TIMESTAMP WITH       TIME           ZONE       NOT               NULL              DEFAULT current_timestamp,
    updated_at     TIMESTAMP WITH       TIME           ZONE       NOT               NULL              DEFAULT current_timestamp,
    deleted_at     TIMESTAMP WITH       TIME           ZONE,
    -- path is a key path for field path to the field that should be redacted. eg .data.message (Note the trailing dot)
    path           TEXT      NOT        NULL           DEFAULT    '',
    -- configured_via represents where the source of the redacted_fields listing. Eg via the dashboard
    -- or in the future if we allow setting redacted fields via our sdks.
    configured_via TEXT      NOT        NULL           DEFAULT    '',
    description    TEXT      NOT        NULL           DEFAULT    '',
    endpoint_hash  TEXT,
    field_category apis.field_category 
);
SELECT manage_updated_at('projects.redacted_fields');
CREATE INDEX IF NOT EXISTS idx_projects_redacted_fields_project_id ON projects.redacted_fields(project_id);

-- NEW project requests view that rounds up stats by 1minute
-- NOTE: Is this actually used?
DROP MATERIALIZED VIEW  IF EXISTS apis.project_requests_by_endpoint_per_min;
CREATE MATERIALIZED VIEW apis.project_requests_by_endpoint_per_min WITH (timescaledb.continuous)
    AS SELECT
    time_bucket('1 minute', created_at) as timeB,
    project_id, endpoint_hash,
    method || ' ' || url_path endpoint_title,
    status_code, host, count(id) total_count,
    percentile_agg(EXTRACT(epoch FROM duration)) as agg,
    sum(EXTRACT(epoch FROM duration)) as total_time
  FROM
    apis.request_dumps
  GROUP BY project_id, timeB, endpoint_hash, method, url_path, status_code,host
  WITH NO DATA;
-- continuous aggregate policy to refresh every hour.
SELECT add_continuous_aggregate_policy('apis.project_requests_by_endpoint_per_min',
     start_offset => INTERVAL '2 months',
     end_offset => INTERVAL '6 hours',
     schedule_interval => INTERVAL '1 hour');



-- cron doesn't work in the timescaledb database because its not the default database. 
-- so instead, we installed it into the default database and use a different function:
-- SELECT cron.schedule_in_database('DailyOrttoSync', '0 8 * * *', $$INSERT INTO background_jobs (run_at, status, payload) VALUES (now(), 'queued',  jsonb_build_object('tag', 'DailyOrttoSync')$$, 'apitoolkit-prod-eu');

SELECT cron.schedule_in_database('DailyReports', '* * * * *', 'DailyReports', 'apitoolkit-prod-eu');

-- This is for regular databases locally or if we migrate to a new database setup.
-- SELECT cron.schedule('DailyOrttoSync', '0 8 * * *', $$INSERT INTO background_jobs (run_at, status, payload) VALUES (now(), 'queued',  jsonb_build_object('tag', 'DailyOrttoSync')$$);
-- useful query to view job details
-- select * from cron.job_run_details order by start_time desc limit 5;

COMMIT;
