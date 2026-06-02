-- VARCHAR(n) was used for four legacy columns. Postgres treats VARCHAR and
-- TEXT identically except for length enforcement (which we never relied on)
-- and the catalog OID — and that OID mismatch crashes hasql-interpolate's
-- strict text decoder. Normalise to TEXT so generic decoders Just Work.
ALTER TABLE users.users  ALTER COLUMN first_name TYPE TEXT;
ALTER TABLE users.users  ALTER COLUMN last_name  TYPE TEXT;
ALTER TABLE projects.teams ALTER COLUMN name   TYPE TEXT;
ALTER TABLE projects.teams ALTER COLUMN handle TYPE TEXT;
