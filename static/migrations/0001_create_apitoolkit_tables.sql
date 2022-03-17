BEGIN;

CREATE EXTENSION IF NOT EXISTS citext;
CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE;
CREATE EXTENSION IF NOT EXISTS timescaledb_toolkit;
CREATE EXTENSION IF NOT EXISTS pg_stat_statements;


-- create schemas
CREATE SCHEMA IF NOT EXISTS users;
CREATE SCHEMA IF NOT EXISTS projects;
CREATE SCHEMA IF NOT EXISTS apis;



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

-- create user table
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
  hosts TEXT[] NOT NULL DEFAULT '{}'
);
SELECT manage_updated_at('projects.projects');


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

---
---
---

CREATE TABLE IF NOT EXISTS apis.endpoints
(
    id uuid NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    project_id uuid NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
    url_path text NOT NULL DEFAULT ''::text,
    url_params jsonb NOT NULL DEFAULT '{}'::jsonb,
    method text NOT NULL DEFAULT 'GET'::text,
    hosts text[] NOT NULL DEFAULT '{}'::text[],
    UNIQUE(project_id, url_path, method)
);
SELECT manage_updated_at('apis.endpoints');
CREATE INDEX IF NOT EXISTS idx_apis_endpoints ON apis.endpoints(project_id);

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

-- TODO: Create triggers to create new anomalies when new fields, endpoints and shapes are created.


CREATE OR REPLACE FUNCTION apis.create_field_and_formats(
  i_project_id UUID, i_endpoint_id UUID, i_key TEXT, i_field_type apis.field_type, i_field_type_override TEXT, 
  i_format TEXT, i_format_override TEXT, i_description TEXT, i_key_path TEXT[], i_key_path_str TEXT, 
  i_field_category apis.field_category, i_examples TEXT[], i_examples_max_count INT 
)
RETURNS setof apis.formats AS $$
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
	RETURNING *;

END;
$$ LANGUAGE plpgsql;


-- apis.request_dumps table holds a timeseries dump of all requests that come into the backend.
-- We rely on it heavily, for ploting time series graphs to show changes in shapes and endpoint fields over time.
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

    PRIMARY KEY(project_id,created_at,id)
);
SELECT manage_updated_at('apis.request_dumps');
-- SELECT create_hypertable('apis.request_dumps', 'created_at');

COMMIT;
