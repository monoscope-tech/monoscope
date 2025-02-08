BEGIN;

CREATE EXTENSION IF NOT EXISTS citext;
CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE;
CREATE EXTENSION IF NOT EXISTS timescaledb_toolkit;
CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
CREATE EXTENSION IF NOT EXISTS hstore;
CREATE EXTENSION IF NOT EXISTS tablefunc;


-- create schemas
CREATE SCHEMA IF NOT EXISTS users;
CREATE SCHEMA IF NOT EXISTS projects;
CREATE SCHEMA IF NOT EXISTS apis;
CREATE SCHEMA IF NOT EXISTS monitors;
CREATE SCHEMA IF NOT EXISTS tests;

-----------------------------------------------------------------------
-- HELPER FUNCTIONS AND DOMAIN.
-----------------------------------------------------------------------
-----------------------------------------------------------------------
--make manage_updated_at reusable
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION manage_updated_at(_tbl regclass) RETURNS VOID AS $$
BEGIN
  EXECUTE format('CREATE TRIGGER set_updated_at BEFORE UPDATE ON %s FOR EACH ROW EXECUTE PROCEDURE set_updated_at()', _tbl);
  EXCEPTION WHEN others THEN null;
END;
$$ LANGUAGE plpgsql;

--------------------------------------------------------------------

CREATE OR REPLACE FUNCTION set_updated_at() RETURNS trigger AS $$
BEGIN
  IF (
    NEW IS DISTINCT FROM OLD AND
    NEW.updated_at IS NOT DISTINCT FROM OLD.updated_at
  ) THEN NEW.updated_at := current_timestamp;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- end -------------------------------------------------------------

DO $$ BEGIN
  CREATE DOMAIN email AS citext
    CHECK ( value ~ '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$' );
  EXCEPTION WHEN duplicate_object THEN null;
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
  is_sudo           BOOL         NOT  NULL DEFAULT 'f',
  phone_number Text DEFAULT NULL
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

CREATE TYPE notification_channel_enum AS ENUM ('email', 'slack');
ALTER TYPE notification_channel_enum ADD VALUE 'discord';
ALTER TYPE notification_channel_enum ADD VALUE 'phone';


CREATE TABLE IF NOT EXISTS projects.projects
(
  id          UUID      NOT  NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  created_at  TIMESTAMP WITH TIME ZONE    NOT               NULL    DEFAULT current_timestamp,
  updated_at  TIMESTAMP WITH TIME ZONE    NOT               NULL    DEFAULT current_timestamp,
  deleted_at  TIMESTAMP WITH TIME ZONE,
  active      BOOL      NOT  NULL DEFAULT 't',
  title       TEXT      NOT  NULL DEFAULT '',
  description TEXT      NOT  NULL DEFAULT '',
  payment_plan          TEXT NOT NULL DEFAULT 'Free',
  questions             JSONB DEFAULT NULL,
  daily_notif           BOOL DEFAULT FALSE,
  weekly_notif          BOOL DEFAULT TRUE,
  time_zone             TEXT DEFAULT 'UTC',
  notifications_channel notification_channel_enum[] DEFAULT ARRAY['email']::notification_channel_enum[],
  sub_id                TEXT,
  first_sub_item_id     TEXT,
  order_id              TEXT,
  usage_last_reported   TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp
);
SELECT manage_updated_at('projects.projects');
ALTER TABLE projects.projects ADD COLUMN IF NOT EXISTS discord_url TEXT DEFAULT NULL;
ALTER TABLE projects.projects ADD COLUMN IF NOT EXISTS billing_day TIMESTAMP WITH TIME ZONE DEFAULT current_timestamp;
ALTER TABLE projects.projects ADD COLUMN IF NOT EXISTS onboarding_steps_completed TEXT[] DEFAULT ARRAY[]::TEXT[];
ALTER TABLE projects.projects ADD COLUMN IF NOT EXISTS notify_phone_number TEXT DEFAULT NULL;
ALTER TABLE projects.projects ADD COLUMN IF NOT EXISTS notify_emails TEXT[] DEFAULT ARRAY[]::TEXT[];


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
ALTER TABLE apis.swagger_jsons ADD COLUMN host TEXT NOT NULL DEFAULT ''::TEXT;

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

    -- the hash will represent an xxhash of the project_id and method and the url_path
    -- it will be used as the main identifier for endpoints since it can be generated deterministically
    -- without having to check the value from the database.
    hash text NOT NULL DEFAULT ''::text,
    outgoing BOOLEAN DEFAULT FALSE,
    host text NOT  NULL DEFAULT    ''::text,
    description TEXT NOT NULL DEFAULT ''::TEXT
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
    new_unique_fields         text[] NOT NULL DEFAULT '{}'::TEXT[],
    -- All fields which were usually sent for all other requests on this endpoint, but are no longer being sent.
    deleted_fields            text[] NOT NULL DEFAULT '{}'::TEXT[],
    -- All fields associated with this shape which are updates
    updated_field_formats     text[] NOT NULL DEFAULT '{}'::TEXT[],
    status_code               int DEFAULT 0
);
SELECT manage_updated_at('apis.shapes');
CREATE INDEX IF NOT EXISTS idx_apis_shapes_project_id ON apis.shapes(project_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_shapes_hash ON apis.shapes(hash);
ALTER TABLE apis.shapes ADD COLUMN response_description TEXT NOT NULL DEFAULT ''::TEXT;
ALTER TABLE apis.shapes ADD COLUMN request_description TEXT NOT NULL DEFAULT ''::TEXT;

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
    key_path            text            NOT  NULL DEFAULT    '',
    field_category apis.field_category NOT NULL DEFAULT 'request_body'::apis.field_category,

    -- the hash of a field is the <hash of the endpoint> + <the hash of <field_category>,<key_path_str>,<field_type>>
    hash text NOT NULL DEFAULT ''::TEXT
);
SELECT manage_updated_at('apis.fields');
CREATE INDEX IF NOT EXISTS idx_apis_fields_project_id ON apis.fields(project_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_fields_hash ON apis.fields(hash);
ALTER TABLE apis.fields ADD COLUMN is_enum BOOL NOT NULL DEFAULT 'f';
ALTER TABLE apis.fields ADD COLUMN is_required BOOL NOT NULL DEFAULT 'f';

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

CREATE TYPE apis.anomaly_type AS ENUM ('unknown', 'field', 'endpoint','shape', 'format', 'runtime_exception');
CREATE TYPE apis.anomaly_action AS ENUM ('unknown', 'created');
CREATE TABLE IF NOT EXISTS apis.anomalies
(
  id             uuid                NOT        NULL        DEFAULT    gen_random_uuid() PRIMARY KEY,
  created_at     TIMESTAMP           WITH       TIME        ZONE       NOT               NULL    DEFAULT current_timestamp,
  updated_at     TIMESTAMP           WITH       TIME        ZONE       NOT               NULL    DEFAULT current_timestamp,
  project_id     uuid                NOT        NULL        REFERENCES projects.projects (id)    ON      DELETE CASCADE,
  acknowleged_at TIMESTAMP           WITH       TIME        ZONE,
  acknowleged_by UUID                REFERENCES users.users (id),
  anomaly_type   apis.anomaly_type   NOT        NULL        DEFAULT    'unknown'::apis.anomaly_type,
  action         apis.anomaly_action NOT        NULL        DEFAULT    'unknown'::apis.anomaly_action,
  target_hash    text,
  archived_at    TIMESTAMP           WITH       TIME        ZONE
);
SELECT manage_updated_at('apis.anomalies');
CREATE INDEX IF NOT EXISTS idx_apis_anomalies_project_id ON apis.anomalies(project_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_anomalies_project_id_target_hash ON apis.anomalies(project_id, target_hash);
ALTER TABLE apis.anomalies DROP CONSTRAINT IF EXISTS anomalies_acknowleged_by_fkey;
ALTER TABLE apis.anomalies ADD CONSTRAINT anomalies_acknowleged_by_fkey FOREIGN KEY (acknowleged_by) REFERENCES users.users (id) ON DELETE CASCADE;

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
  INSERT INTO background_jobs (run_at, status, payload) VALUES (now() + INTERVAL '5 minutes', 'queued',  jsonb_build_object('tag', 'NewAnomaly', 'contents', json_build_array(NEW.project_id, NEW.created_at, anomaly_type::text, anomaly_action::text, NEW.hash)));
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER fields_created_anomaly AFTER INSERT ON apis.fields FOR EACH ROW EXECUTE PROCEDURE apis.new_anomaly_proc('field', 'created');
CREATE OR REPLACE TRIGGER format_created_anomaly AFTER INSERT ON apis.formats FOR EACH ROW EXECUTE PROCEDURE apis.new_anomaly_proc('format', 'created');
CREATE OR REPLACE TRIGGER endpoint_created_anomaly AFTER INSERT ON apis.endpoints FOR EACH ROW EXECUTE PROCEDURE apis.new_anomaly_proc('endpoint', 'created');
CREATE OR REPLACE TRIGGER shapes_created_anomaly AFTER INSERT ON apis.shapes FOR EACH ROW EXECUTE PROCEDURE apis.new_anomaly_proc('shape', 'created');

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
    duration_ns               BIGINT    NOT  NULL default 0,
    sdk_type                  TEXT NOT NULL default '',
    parent_id                 uuid,
    service_version           text,
    errors                    jsonb NOT NULL DEFAULT '{}'::jsonb,
    tags                      text[] NOT NULL DEFAULT '{}'::text[],
    request_type              TEXT NOT NULL DEFAULT 'Incoming',

    PRIMARY KEY(project_id,created_at,id)
);
SELECT manage_updated_at('apis.request_dumps');
SELECT create_hypertable('apis.request_dumps', by_range('created_at', INTERVAL '4 hours'), migrate_data => true);
SELECT add_retention_policy('apis.request_dumps',INTERVAL '14 days',true);
CREATE INDEX IF NOT EXISTS idx_apis_request_dumps_project_id ON apis.request_dumps(project_id, created_at DESC);
ALTER TABLE apis.request_dumps SET (timescaledb.compress, timescaledb.compress_orderby = 'created_at DESC', timescaledb.compress_segmentby = 'project_id');
SELECT add_compression_policy('apis.request_dumps', INTERVAL '8 hours');
CREATE INDEX IF NOT EXISTS idx_apis_request_dumps_project_id_parent_id ON apis.request_dumps(project_id, parent_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_apis_request_dumps_project_id_endpoint_hash ON apis.request_dumps(project_id, endpoint_hash, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_apis_request_dumps_project_id_shape_hash ON apis.request_dumps(project_id, shape_hash, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_apis_request_dumps_project_id_shape_hash ON apis.request_dumps(project_id, shape_hash, created_at DESC);
CREATE INDEX IF NOT EXISTS idxgin_apis_request_dumps_errors ON apis.request_dumps USING GIN (errors);
CREATE INDEX IF NOT EXISTS idxgin_apis_request_dumps_format_hashes ON apis.request_dumps USING GIN (format_hashes);

-- ==========================================================================================================================
--                    END OF REQUEST DUMP AND ITS CONTINUOUS AGGREGATES
-- ==========================================================================================================================

CREATE TABLE IF NOT EXISTS apis.reports
(
    id                        uuid      NOT  NULL DEFAULT    gen_random_uuid(),
    created_at                TIMESTAMP WITH TIME ZONE       NOT               NULL DEFAULT current_timestamp,
    updated_at                TIMESTAMP WITH TIME ZONE       NOT               NULL DEFAULT current_timestamp,
    project_id                UUID      NOT  NULL REFERENCES projects.projects (id) ON      DELETE CASCADE,
    report_type               text      NOT  NULL DEFAULT    '',
    report_json               jsonb     NOT  NULL DEFAULT    '{}'::jsonb,
    PRIMARY KEY(id)
);
SELECT manage_updated_at('apis.reports');
CREATE INDEX IF NOT EXISTS idx_reports_project_id ON apis.reports(project_id);


-- TODO: rewrite this. This query is killing the database.
-- Create a view that tracks endpoint related statistic points from the request dump table.
DROP MATERIALIZED VIEW IF EXISTS apis.endpoint_request_stats;
CREATE MATERIALIZED VIEW IF NOT EXISTS apis.endpoint_request_stats AS
 WITH request_dump_stats as (
      SELECT
          project_id, url_path, method,
          endpoint_hash, host,
          percentile_agg(EXTRACT(epoch FROM duration)) as agg,
          sum(EXTRACT(epoch FROM duration))  as total_time,
          count(1)  as total_requests,
          sum(sum(EXTRACT(epoch FROM duration))) OVER (partition by project_id) as total_time_proj,
          sum(count(*)) OVER (partition by project_id) as total_requests_proj
      FROM apis.request_dumps
      where created_at > NOW() - interval '14' day
      GROUP BY project_id, url_path, method, endpoint_hash, host
    )
    SELECT
        enp.id endpoint_id,
        enp.hash endpoint_hash,
        rds.project_id,
        rds.url_path, rds.method, rds.host,
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

CREATE TABLE IF NOT EXISTS apis.issues
(
  id              UUID NOT NULL DEFAULT gen_random_uuid(),
  created_at      TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at      TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  project_id      UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
  acknowleged_at  TIMESTAMP WITH TIME ZONE,
  anomaly_type    apis.anomaly_type NOT NULL,
  target_hash     TEXT,
  issue_data      JSONB NOT NULL DEFAULT '{}',
  endpoint_id     UUID,
  acknowleged_by  UUID,
  archived_at    TIMESTAMP           WITH       TIME        ZONE
);
SELECT manage_updated_at('apis.issues');
CREATE INDEX IF NOT EXISTS idx_apis_issues_project_id ON apis.issues(project_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_issues_project_id_target_hash ON apis.issues(project_id, target_hash);


-- Create a view that tracks project request related statistic points from the request dump table.
CREATE MATERIALIZED VIEW IF NOT EXISTS apis.project_request_stats AS
  WITH request_dump_stats AS (
      SELECT
          project_id,
          percentile_agg(EXTRACT(epoch FROM duration)) as agg,
          sum(EXTRACT(epoch FROM duration)) as total_time,
          count(1) as total_requests
      FROM apis.request_dumps
      WHERE created_at > NOW() - interval '14' day
      GROUP BY project_id
  ),
  current_week_requests AS (
      SELECT
          project_id,
          COUNT(*) AS request_count
      FROM apis.request_dumps
      WHERE created_at >= NOW() - interval '14 days'
      GROUP BY project_id
  ),
  previous_week_requests AS (
      SELECT
          project_id,
          COUNT(*) AS request_count
      FROM apis.request_dumps
      WHERE created_at >= NOW() - interval '14 days' AND created_at < NOW() - interval '7 days'
      GROUP BY project_id
  ),
  endpoints_stats AS (
      SELECT
          project_id,
          count(*) as total_endpoints,
          count(*) FILTER (WHERE created_at <= NOW()::DATE - 7) as total_endpoints_last_week
      FROM apis.endpoints
      GROUP BY project_id
  ),
  shapes_stats AS (
      SELECT
          project_id,
          count(*) as total_shapes,
          count(*) FILTER (WHERE created_at <= NOW()::DATE - 7) as total_shapes_last_week
      FROM apis.shapes
      GROUP BY project_id
  ),
  anomalies_stats AS (
      SELECT
          project_id,
          count(*) FILTER (WHERE anomaly_type != 'field' AND acknowleged_at IS NULL) as total_anomalies,
          count(*) FILTER (WHERE created_at < NOW()::DATE - 7 AND anomaly_type != 'field' AND acknowleged_at IS NULL) as total_anomalies_last_week
      FROM apis.issues
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
      CAST (total_time/1000000 AS FLOAT8) total_time,
      CAST (total_requests AS INT) total_requests,
      coalesce(es.total_endpoints, 0) as total_endpoints,
      coalesce(es.total_endpoints_last_week, 0) as total_endpoints_last_week,
      coalesce(ss.total_shapes, 0) as total_shapes,
      coalesce(ss.total_shapes_last_week, 0) as total_shapes_last_week,
      coalesce(ass.total_anomalies, 0) as total_anomalies,
      coalesce(ass.total_anomalies_last_week, 0) as total_anomalies_last_week,
      CAST (coalesce(cw.request_count / (7 * 24 * 60), 0) AS INT) requests_per_min,
      CAST (coalesce(pw.request_count / (7 * 24 * 60), 0) AS INT) requests_per_min_last_week
  FROM request_dump_stats rds
  LEFT JOIN current_week_requests cw ON rds.project_id = cw.project_id
  LEFT JOIN previous_week_requests pw ON rds.project_id = pw.project_id
  LEFT JOIN endpoints_stats es ON rds.project_id = es.project_id
  LEFT JOIN shapes_stats ss ON rds.project_id = ss.project_id
  LEFT JOIN anomalies_stats ass ON rds.project_id = ass.project_id;

CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_project_request_stats_project_id ON apis.project_request_stats(project_id);
-- TODO: Create triggers to create new anomalies when new fields,
-- TODO: endpoints and shapes are created.

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




create or replace function notify_job_monitor_for_background_jobs() returns trigger as
$$ BEGIN
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
    deactivated_at TIMESTAMP WITH       TIME           ZONE,
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


-- apitoolkit_daily_job should be executed every day at midnight
CREATE OR REPLACE PROCEDURE apitoolkit_daily_job(job_id int, config jsonb) LANGUAGE PLPGSQL AS
$$ BEGIN
  INSERT INTO background_jobs (run_at, status, payload) VALUES (now(), 'queued',  jsonb_build_object('tag', 'DailyJob'));
END;
$$;
SELECT add_job('apitoolkit_daily_job', schedule_interval => interval '1 DAY', initial_start => '2024-05-16 00:00'::timestamptz);

CREATE TABLE IF NOT EXISTS apis.share_requests
 (
    id                 UUID      NOT        NULL           DEFAULT           gen_random_uuid() PRIMARY KEY,
    project_id         UUID      NOT        NULL           REFERENCES projects.projects (id)              ON      DELETE CASCADE,
    created_at         TIMESTAMP WITH       TIME           ZONE       NOT               NULL              DEFAULT current_timestamp,
    updated_at         TIMESTAMP WITH       TIME           ZONE       NOT               NULL              DEFAULT current_timestamp,
    expired_at         TIMESTAMP WITH       TIME           ZONE       NOT               NULL              DEFAULT current_timestamp + INTERVAL '1 hour',
    request_dump_id    UUID      NOT        NULL,
    request_created_at TIMESTAMP WITH TIME ZONE NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_apis_share_requests_id ON apis.share_requests(id);
CREATE INDEX IF NOT EXISTS idx_share_requests ON apis.share_requests(request_created_at);

CREATE TABLE IF NOT EXISTS apis.share_events
 (
    id                 UUID      NOT        NULL           DEFAULT           gen_random_uuid() PRIMARY KEY,
    project_id         UUID      NOT        NULL           REFERENCES projects.projects (id)              ON      DELETE CASCADE,
    created_at         TIMESTAMP WITH       TIME           ZONE       NOT               NULL              DEFAULT current_timestamp,
    event_id           UUID      NOT        NULL,
    event_type         TEXT      NOT        NULL,
);
CREATE INDEX IF NOT EXISTS idx_apis_share_events_id ON apis.share_events(id);


CREATE TABLE IF NOT EXISTS apis.slack
(
  id             UUID        NOT     NULL   DEFAULT        gen_random_uuid() PRIMARY KEY,
  project_id     UUID        NOT     NULL   REFERENCES projects.projects (id)              ON      DELETE CASCADE,
  created_at     TIMESTAMP   WITH    TIME   ZONE       NOT               NULL              DEFAULT current_timestamp,
  updated_at     TIMESTAMP   WITH    TIME   ZONE       NOT               NULL              DEFAULT current_timestamp,
  webhook_url   TEXT        NOT     NULL   DEFAULT        '',
  UNIQUE (project_id)
);

CREATE TABLE IF NOT EXISTS tests.collections
(
  id               UUID        NOT     NULL   DEFAULT        gen_random_uuid() PRIMARY KEY,
  created_at       TIMESTAMP   WITH    TIME   ZONE       NOT               NULL              DEFAULT current_timestamp,
  updated_at       TIMESTAMP   WITH    TIME   ZONE       NOT               NULL              DEFAULT current_timestamp,
  deleted_at       TIMESTAMP WITH TIME ZONE DEFAULT NULL,
  project_id       UUID        NOT     NULL   REFERENCES projects.projects (id)              ON      DELETE CASCADE,
  last_run         TIMESTAMP   WITH    TIME   ZONE       DEFAULT NULL,
  title            TEXT        NOT     NULL   DEFAULT        '',
  description      TEXT        NOT     NULL   DEFAULT        '',
  config           jsonb       NOT     NULL   DEFAULT     '{}'::jsonb,
  is_scheduled     BOOL        NOT  NULL   DEFAULT 'f',
	collection_steps JSONB       NOT NULL DEFAULT '{}'::jsonb,
  schedule         INTERVAL NOT NULL DEFAULT '1 day'
);
SELECT manage_updated_at('tests.collections');
create index if not exists idx_apis_testing_project_Id on tests.collections(project_id);
ALTER TABLE tests.collections ADD COLUMN last_run_response jsonb DEFAULT NULL;
ALTER TABLE tests.collections ADD COLUMN last_run_passed INT DEFAULT 0;
ALTER TABLE tests.collections ADD COLUMN last_run_failed INT DEFAULT 0;
ALTER TABLE tests.collections ADD COLUMN tags TEXT[] NOT NULL DEFAULT '{}'::text[];
ALTER TABLE tests.collections ADD COLUMN collection_variables JSONB NOT NULL DEFAULT '[]'::jsonb;
ALTER TABLE tests.collections ADD COLUMN alert_severity TEXT NOT NULL DEFAULT '';
ALTER TABLE tests.collections ADD COLUMN alert_message TEXT NOT NULL DEFAULT '';
ALTER TABLE tests.collections ADD COLUMN alert_subject TEXT NOT NULL DEFAULT '';
ALTER TABLE tests.collections ADD COLUMN notify_after Text NOT NULL DEFAULT '6hours';
ALTER TABLE tests.collections ADD COLUMN notify_after_check BOOL NOT NULL DEFAULT 'f';
ALTER TABLE tests.collections ADD COLUMN stop_after TEXT NOT NULL DEFAULT "0";
ALTER TABLE tests.collections ADD COLUMN stop_after_check BOOL NOT NULL DEFAULT 'f';



CREATE TABLE IF NOT EXISTS monitors.query_monitors
(
  id                           UUID NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  project_id                   UUID NOT NULL REFERENCES projects.projects(id)        ON DELETE CASCADE,
  created_at                   TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at                   TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  check_interval_mins          INT NOT NULL DEFAULT 1,
  alert_threshold              INT NOT NULL,
  warning_threshold            INT,
  log_query                    TEXT NOT NULL DEFAULT '',
  log_query_as_sql             TEXT NOT NULL DEFAULT '',
  last_evaluated               TIMESTAMP WITH TIME ZONE,
  warning_last_triggered       TIMESTAMP WITH TIME ZONE,
  alert_last_triggered         TIMESTAMP WITH TIME ZONE,
  trigger_less_than            BOOL,
  threshold_sustained_for_mins INT NOT NULL DEFAULT 0,
  alert_config                 JSONB NOT NULL DEFAULT '{}',
  deactivated_at               TIMESTAMP WITH TIME ZONE,
  deleted_at                   TIMESTAMP WITH TIME ZONE
);
SELECT manage_updated_at('monitors.query_monitors');


CREATE TABLE IF NOT EXISTS apis.subscriptions (
  id UUID NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  project_id TEXT,
  subscription_id INT NOT NULL,
  order_id INT NOT NULL,
  first_sub_id INT NOT NULL,
  product_name TEXT NOT NULL,
  user_email  TEXT NOT NULL
);
SELECT manage_updated_at('apis.subscriptions');


CREATE TABLE IF NOT EXISTS apis.daily_usage (
  id             UUID      NOT  NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  created_at     TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at     TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  project_id     UUID      NOT  NULL REFERENCES projects.projects (id)   ON DELETE CASCADE,
  total_requests INT NOT  NULL
);
CREATE INDEX IF NOT EXISTS idx_apis_daily_usage_project_id ON apis.daily_usage(project_id);


-- used for the alerts, to execute queries stored in a table,
create or replace function eval(expression text) returns integer as $body$
declare result integer;
begin
  execute expression into result;
  return result;
end;
$body$
language plpgsql;


-- Checks for query monitors being triggered and creates a background job for any found
CREATE OR REPLACE PROCEDURE monitors.check_triggered_query_monitors(job_id int, config jsonb) LANGUAGE PLPGSQL AS $$
DECLARE
    -- Array to hold IDs from the query
    id_array UUID[];
BEGIN
    -- Execute the query and store the result in id_array
    SELECT ARRAY_AGG(id) INTO id_array
    FROM monitors.query_monitors
    WHERE alert_last_triggered IS NULL
      AND deactivated_at IS NULL
      AND (
          (NOT trigger_less_than AND (
               warning_threshold <= eval(log_query_as_sql)
               OR alert_threshold <= eval(log_query_as_sql)
           ))
          OR
          (trigger_less_than AND (
               warning_threshold >= eval(log_query_as_sql)
               OR alert_threshold >= eval(log_query_as_sql)
           ))
      );

    -- Check if id_array is not empty
    IF id_array IS NOT NULL AND array_length(id_array, 1) > 0 THEN
        -- Perform the insert operation using the array
        INSERT INTO background_jobs (run_at, status, payload)
        VALUES (NOW(), 'queued', jsonb_build_object('tag', 'QueryMonitorsTriggered', 'contents', id_array));
    END IF;
END;
$$;
SELECT add_job('monitors.check_triggered_query_monitors','1min');

-- Find all tests where last_run + schedule is < NOW()+10min
-- schedule them as 1 job per test.
CREATE OR REPLACE PROCEDURE tests.check_tests_to_trigger(job_id int, config JSONB) LANGUAGE PLPGSQL AS $$
DECLARE
    collection_record RECORD;
    curr_time TIMESTAMPTZ := NOW();
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

        -- Schedule the collection for each interval
        FOR i IN 0..num_schedules LOOP
			next_run_at := curr_time + (i * collection_record.schedule || ' minutes')::INTERVAL;
			IF next_run_at < collection_record.last_run THEN
       			 CONTINUE;
    		END IF;

			RAISE DEBUG 'DEBUG RUN SCHEDULE INDEX: %; NOW: %; RUN_AT: %', i,curr_time,  next_run_at ;
            INSERT INTO background_jobs (run_at, status, payload)
            VALUES (curr_time + (i * collection_record.schedule || ' minutes')::INTERVAL, 'queued',
					jsonb_build_object('tag', 'RunCollectionTests', 'contents', collection_record.id));
        END LOOP;

        -- Update the last_run timestamp for the collection to the start of the first new schedule
		IF next_run_at < collection_record.last_run THEN
        	UPDATE tests.collections
       		SET last_run = next_run_at
        	WHERE id = collection_record.id;
		END IF;
    END LOOP;
END;
$$;
SELECT add_job('tests.check_tests_to_trigger', '10min');

INSERT into projects.projects (id, title) VALUES ('00000000-0000-0000-0000-000000000000', 'Demo Project');


CREATE TABLE IF NOT EXISTS apis.errors
(
  id              UUID NOT NULL DEFAULT gen_random_uuid(),
  created_at      TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at      TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  project_id      UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
  hash            TEXT NOT NULL,
  error_type      TEXT NOT NULL,
  message         TEXT NOT NULL,
  error_data      JSONB NOT NULL DEFAULT '{}',

  PRIMARY KEY(id)
);
SELECT manage_updated_at('apis.errors');
CREATE INDEX IF NOT EXISTS idx_apis_errors_project_id ON apis.errors(project_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_apis_errors_project_id_hash ON apis.errors(project_id, hash);

CREATE OR REPLACE FUNCTION apis.new_anomaly_proc_job_only() RETURNS trigger AS $$
DECLARE
	anomaly_type apis.anomaly_type;
	anomaly_action apis.anomaly_action;
BEGIN
  IF TG_WHEN <> 'AFTER' THEN
      RAISE EXCEPTION 'apis.new_anomaly_proc() may only run as an AFTER trigger';
  END IF;
  anomaly_type := TG_ARGV[0];
  anomaly_action := TG_ARGV[1];
  INSERT INTO background_jobs (run_at, status, payload) VALUES (now(), 'queued',  jsonb_build_object('tag', 'NewAnomaly', 'contents', json_build_array(NEW.project_id, NEW.created_at, anomaly_type::text, anomaly_action::text, NEW.hash)));
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER error_created_anomaly AFTER INSERT ON apis.errors FOR EACH ROW EXECUTE PROCEDURE apis.new_anomaly_proc_job_only('runtime_exception', 'created');


COMMIT;
