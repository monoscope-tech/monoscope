-- Alert payloads can carry end-user identity from OpenTelemetry. This belongs
-- to the project: one incident can fan out to several teams/channels, so a
-- per-destination setting could disclose PII inconsistently. It is enabled by
-- default and can be disabled in notification settings.
ALTER TABLE projects.projects
  ADD COLUMN IF NOT EXISTS include_user_identity_in_alerts BOOLEAN NOT NULL DEFAULT TRUE;
