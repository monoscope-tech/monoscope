BEGIN;

CREATE EXTENSION IF NOT EXISTS citext;
CREATE EXTENSION IF NOT EXISTS pgcrypto;


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
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  deleted_at TIMESTAMP WITH TIME ZONE,
  active BOOL NOT NULL DEFAULT 't',
  id UUID NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  first_name VARCHAR(100) NOT NULL DEFAULT '',
  last_name VARCHAR(100) NOT NULL DEFAULT '',
  display_image_url TEXT NOT NULL DEFAULT '',
  email email NOT NULL UNIQUE
);
SELECT manage_updated_at('users.users');

-- create user auth options table
CREATE TABLE IF NOT EXISTS users.user_auth_options
(
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  deleted_at TIMESTAMP WITH TIME ZONE,
  active BOOL NOT NULL DEFAULT 't',
  id UUID NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  user_id UUID NOT NULL REFERENCES users.users ON DELETE CASCADE,
  auth_provider VARCHAR(100) NOT NULL DEFAULT '',
  auth_id TEXT NOT NULL DEFAULT '', 
  auth_password TEXT NOT NULL DEFAULT '', -- password for the auth provider. Only used for email login
  auth_token TEXT NOT NULL DEFAULT '',
  auth_secret TEXT NOT NULL DEFAULT '',
  auth_refresh_token TEXT NOT NULL DEFAULT '',
  auth_expires_at TIMESTAMP WITH TIME ZONE,
  auth_data JSONB
);
SELECT manage_updated_at('users.user_auth_options');

CREATE TABLE IF NOT EXISTS projects.projects
(
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  deleted_at TIMESTAMP WITH TIME ZONE,
  active BOOL NOT NULL DEFAULT 't',
  id UUID NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  title TEXT NOT NULL DEFAULT '',
  description TEXT NOT NULL DEFAULT '',
  hosts TEXT[] NOT NULL DEFAULT '{}'
);
SELECT manage_updated_at('projects.projects');

CREATE TYPE projects.project_permissions AS ENUM ('admin', 'view', 'edit');
CREATE TABLE IF NOT EXISTS projects.project_members
(
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  deleted_at TIMESTAMP WITH TIME ZONE,
  active BOOL NOT NULL DEFAULT 't',
  id UUID NOT NULL DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE ON UPDATE CASCADE,
  user_id UUID NOT NULL REFERENCES users.users (id) ON DELETE CASCADE ON UPDATE CASCADE,
  permissions projects.project_permissions NOT NULL DEFAULT 'view',
  PRIMARY KEY (project_id, user_id)
);
SELECT manage_updated_at('projects.project_members');


CREATE TABLE IF NOT EXISTS projects.project_api_keys
(
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  deleted_at TIMESTAMP WITH TIME ZONE,
  active BOOL NOT NULL DEFAULT 't',
  id UUID NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE ON UPDATE CASCADE,
  title TEXT NOT NULL DEFAULT '',
  key_prefix TEXT NOT NULL DEFAULT ''
);
SELECT manage_updated_at('projects.project_api_keys');

-- insert an initial user. User should be deleted from db when app is ready for prod
INSERT INTO users.users(id, first_name, last_name, email)
  VALUES ('00000000-0000-0000-0000-000000000000', 'test', 'user', 'test@user.com');
INSERT INTO users.user_auth_options ( user_id, auth_id, auth_password)
  VALUES ('00000000-0000-0000-0000-000000000000', 'test@user.com', crypt('password', gen_salt('bf')));
-- insert an initial project.
INSERT INTO projects.projects (id, title, description)
  VALUES ('00000000-0000-0000-0000-000000000000','test title', 'test desc');
INSERT INTO projects.project_members (project_id, user_id, permissions)
  VALUES ('00000000-0000-0000-0000-000000000000','00000000-0000-0000-0000-000000000000','view');

---
---
---
CREATE TABLE IF NOT EXISTS apis.request_dumps
(
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    id uuid NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
    project_id uuid NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE ON UPDATE CASCADE,
    host text NOT NULL DEFAULT '',
    url_path text NOT NULL DEFAULT '',
    method text NOT NULL DEFAULT '',
    referer text NOT NULL DEFAULT '',
    proto_major int NOT NULL DEFAULT 0,
    proto_minor int NOT NULL DEFAULT 0,
    duration interval,
    status_code int NOT NULL DEFAULT 0,
    request_body jsonb NOT NULL DEFAULT '{}'::jsonb,
    response_body jsonb NOT NULL DEFAULT '{}'::jsonb,
    request_headers jsonb NOT NULL DEFAULT '{}'::jsonb,
    response_headers jsonb NOT NULL DEFAULT '{}'::jsonb
);
SELECT manage_updated_at('apis.request_dumps');

CREATE TABLE IF NOT EXISTS apis.endpoints
(
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    id uuid NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
    project_id uuid NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE ON UPDATE CASCADE,
    url_path text NOT NULL DEFAULT ''::text,
    url_params jsonb NOT NULL DEFAULT '{}'::jsonb,
    method text NOT NULL DEFAULT 'GET'::text,
    hosts text[] NOT NULL DEFAULT '{}'::text[],
    request_hashes text[] NOT NULL DEFAULT '{}'::text[],
    response_hashes text[] NOT NULL DEFAULT '{}'::text[],
    queryparam_hashes text[] NOT NULL DEFAULT '{}'::text[],
    UNIQUE(project_id, url_path, method)
);
SELECT manage_updated_at('apis.endpoints');

CREATE TYPE apis.field_type AS ENUM ('unknown','string','number','bool','object', 'list');
CREATE TYPE apis.field_category AS ENUM ('queryparam', 'request_header','response_headers', 'request_body', 'response_body');
CREATE TABLE IF NOT EXISTS apis.fields
(
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    id uuid NOT NULL DEFAULT gen_random_uuid()  PRIMARY KEY,
    project_id uuid NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE ON UPDATE CASCADE,
    endpoint uuid NOT NULL,
    key text  NOT NULL DEFAULT ''::text,
    -- field_type field_type NOT NULL DEFAULT 'unknown'::field_type,
    field_type text NOT NULL DEFAULT 'unknown',
    field_type_override text,
    format text NOT NULL DEFAULT 'none'::text,
    format_override text NOT NULL DEFAULT '',
    description text NOT NULL DEFAULT ''::text,
    key_path text[] NOT NULL DEFAULT '{}'::text[],
    key_path_str text NOT NULL DEFAULT '',
    field_category text NOT NULL DEFAULT '',
    UNIQUE (project_id, endpoint, key_path_str, format)
);
SELECT manage_updated_at('apis.fields');

CREATE TABLE IF NOT EXISTS apis.formats
(
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  deleted_at TIMESTAMP WITH TIME ZONE,
  project_id uuid NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE ON UPDATE CASCADE,
  id uuid NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  field_id uuid NOT NULL REFERENCES apis.fields (id) ON DELETE CASCADE ON UPDATE CASCADE,
  field_type apis.field_type NOT NULL DEFAULT 'unknown'::apis.field_type,
  field_format text NOT NULL DEFAULT '',
  examples text[] NOT NULL DEFAULT '{}'::text[],
  UNIQUE (project_id, field_id, field_format)
);
SELECT manage_updated_at('apis.formats');

CREATE OR REPLACE FUNCTION apis.create_field_and_formats(
  i_project_id UUID, i_endpoint UUID, i_key TEXT, i_field_type TEXT, i_field_type_override TEXT, 
  i_format TEXT, i_format_override TEXT, i_description TEXT, i_key_path TEXT[], i_key_path_str TEXT, 
  i_field_category TEXT, i_examples TEXT[], i_examples_max_count INT 
)
RETURNS setof apis.formats AS $$
BEGIN
  return query
  with returned_fields AS (
    INSERT INTO apis.fields (project_id, endpoint, key, field_type, field_type_override, format, format_override, description, key_path, key_path_str, field_category)
      VALUES(i_project_id, i_endpoint, i_key, i_field_type, i_field_type_override, i_format, i_format_override, i_description, i_key_path, i_key_path_str, i_field_category)
      ON CONFLICT (project_id, endpoint, key_path_str,format) DO NOTHING
    RETURNING project_id, id, field_type, format,i_examples
  ), current_fields AS (
    SELECT * FROM returned_fields
      UNION ALL
	SELECT project_id, id, field_type, format,i_examples
     FROM apis.fields
     WHERE  project_id=i_project_id AND endpoint=i_endpoint AND key_path_str=i_key_path_str AND format=i_format -- only executed if no INSERT
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
COMMIT;
