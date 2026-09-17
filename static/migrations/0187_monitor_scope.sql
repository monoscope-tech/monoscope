-- Monitor evaluation runs without a browser session. Persist this scope beside
-- the readable KQL so background evaluation cannot cross a chosen boundary.
ALTER TABLE monitors.query_monitors
  ADD COLUMN IF NOT EXISTS environment TEXT,
  ADD COLUMN IF NOT EXISTS service TEXT;
