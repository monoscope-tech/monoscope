-- Local password-based auth: add password hash to users.users.
-- Nullable so existing Auth0 / basic-auth users (created via OAuth callback)
-- remain valid; only locally-registered users get a password_hash.
ALTER TABLE users.users
  ADD COLUMN IF NOT EXISTS password_hash TEXT DEFAULT NULL;
