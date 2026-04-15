-- Stamp originating service and environment onto discovered endpoints so
-- new-endpoint alerts can surface which runtime introduced the route.

ALTER TABLE apis.endpoints
  ADD COLUMN IF NOT EXISTS service_name TEXT,
  ADD COLUMN IF NOT EXISTS environment  TEXT;
