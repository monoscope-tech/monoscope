-- Track per-host traffic recency so the daily retention sweep can archive hosts
-- with no events for 30 days (archived_by stays NULL = system action) and
-- unarchive them when traffic returns. Backfilled to NOW() so nothing is
-- archived before a full observation window has passed.
ALTER TABLE apis.hosts ADD COLUMN IF NOT EXISTS last_seen_at TIMESTAMPTZ NOT NULL DEFAULT NOW();
