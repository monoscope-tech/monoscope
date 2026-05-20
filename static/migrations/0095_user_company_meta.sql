-- Move marketing/telemetry fields (originally collected in project onboarding step 1)
-- onto the user record, so they're asked once at registration rather than every
-- time someone creates a project.
ALTER TABLE users.users
  ADD COLUMN IF NOT EXISTS company_name   TEXT DEFAULT NULL,
  ADD COLUMN IF NOT EXISTS company_size   TEXT DEFAULT NULL,
  ADD COLUMN IF NOT EXISTS found_us_from  TEXT DEFAULT NULL;
