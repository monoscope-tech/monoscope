-- Drop the legacy projects.redacted_fields table. Per-project redact rules
-- and the management UI were removed; ingestion now applies a hard-coded
-- redact list (.set-cookie, .password) in ProcessMessage.extractObservation.

DROP TABLE IF EXISTS projects.redacted_fields CASCADE;
