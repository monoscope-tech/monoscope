-- Instance-wide configuration editable from the admin Settings page.
-- key/value store: each row is one logical config (e.g. 'smtp', 'telegram').
-- Values are JSONB so we can evolve the shape without migrations.
CREATE SCHEMA IF NOT EXISTS system;

CREATE TABLE IF NOT EXISTS system.app_config
  ( key        TEXT        NOT NULL PRIMARY KEY
  , value      JSONB       NOT NULL DEFAULT '{}'::jsonb
  , updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
  );
