-- Align DB column types with what hasql-interpolate's strict OID checker expects,
-- so generic 'DecodeRow' resolves columns through derived 'DecodeValue' instances
-- (no per-query @::text@ casts needed).
--
-- 1. The @email@ domain wraps @citext@. libpq reports the domain OID, which the
--    @CI Text@ decoder (now built on 'D.citext') doesn't accept. Drop the domain
--    and store the column as plain @citext@; preserve the format validation as a
--    column-level CHECK.
ALTER TABLE users.users ALTER COLUMN email TYPE citext;
ALTER TABLE users.users ADD CONSTRAINT users_email_format CHECK (
  email ~ '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$'
);
DROP DOMAIN IF EXISTS email;

-- 2. Haskell @Int@ is 64-bit on every machine we run on; the generic decoder
--    grabs @int8@. These two columns were declared as @INT@ (int4) and now
--    blow up at decode time. BIGINT is a strict superset of the existing data.
ALTER TABLE apis.log_patterns      ALTER COLUMN baseline_samples  TYPE BIGINT;
ALTER TABLE projects.replay_sessions ALTER COLUMN event_file_count TYPE BIGINT;
