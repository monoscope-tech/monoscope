BEGIN;

-- UNLOGGED table for fast writes without WAL overhead
-- Data is not crash-safe but acceptable for cache
CREATE UNLOGGED TABLE IF NOT EXISTS query_cache (
  id UUID UNIQUE NOT NULL DEFAULT gen_random_uuid(),
  created_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,

  -- Cache key components (composite primary key for efficient lookups)
  project_id UUID NOT NULL,
  source TEXT NOT NULL DEFAULT 'spans',
  query_hash TEXT NOT NULL,
  bin_interval TEXT NOT NULL,

  -- Cached time range
  cached_from TIMESTAMPTZ NOT NULL,
  cached_to TIMESTAMPTZ NOT NULL,

  -- Cached data: JSON not JSONB (faster writes, we always read whole blob)
  cached_data JSON NOT NULL,

  -- Cache metadata
  hit_count INT NOT NULL DEFAULT 1,
  last_accessed_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,

  -- Original query for debugging
  original_query TEXT NOT NULL DEFAULT '',

  PRIMARY KEY (project_id, source, query_hash, bin_interval)
)
WITH (
  fillfactor = 70,                        -- enables HOT updates for hit_count/last_accessed_at
  autovacuum_vacuum_scale_factor = 0.1,   -- vacuum when 10% of rows are dead (vs default 20%)
  autovacuum_analyze_scale_factor = 0.05  -- analyze when 5% of rows change (vs default 10%)
);

COMMIT;
