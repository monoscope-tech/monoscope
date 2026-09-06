-- Shared L2 for RUM panel results. The in-memory rumCache is per replica, so every
-- replica paid each panel's cold scan (the vitals-detail scan alone costs ~25s) once per
-- TTL, and the first visitor after expiry always ate one. Keyed by the same hashed cache
-- key the memory layer uses; payload is the panel's serialized RumQueryResult.
CREATE TABLE IF NOT EXISTS rum_panel_cache (
  cache_key  TEXT PRIMARY KEY,
  payload    JSONB       NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_rum_panel_cache_expires ON rum_panel_cache (expires_at);
