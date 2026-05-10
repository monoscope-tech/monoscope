-- The in-app management UI for per-project redaction rules was removed; no
-- handler writes or reads this table anymore. The 0090 cascade also dropped
-- the field_category column it relied on, leaving the schema in a broken
-- state. Hard-coded ingestion-time redaction (.set-cookie, .password) lives
-- in the Haskell path and does not depend on this table.
DROP TABLE IF EXISTS projects.redacted_fields CASCADE;
