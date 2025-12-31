BEGIN;

-- UNLOGGED table for fast writes without WAL overhead
-- Data is not crash-safe but acceptable for cache
CREATE UNLOGGED TABLE IF NOT EXISTS query_cache (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  created_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,

  -- Cache key components
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  source TEXT NOT NULL DEFAULT 'spans',
  query_hash TEXT NOT NULL,
  bin_interval TEXT NOT NULL,

  -- Original query for debugging
  original_query TEXT NOT NULL DEFAULT '',

  -- Cached time range
  cached_from TIMESTAMPTZ NOT NULL,
  cached_to TIMESTAMPTZ NOT NULL,

  -- Cached data (JSONB for flexibility)
  cached_data JSONB NOT NULL,

  -- Cache metadata
  hit_count INT NOT NULL DEFAULT 0,
  last_accessed_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,

  -- Unique constraint on cache key (also serves as lookup index)
  UNIQUE (project_id, source, query_hash, bin_interval)
);

CREATE INDEX IF NOT EXISTS idx_query_cache_eviction
  ON query_cache(cached_to);

CREATE INDEX IF NOT EXISTS idx_query_cache_lru
  ON query_cache(last_accessed_at);

COMMIT;
