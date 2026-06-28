-- The dispatcher claims due targets with: WHERE enabled ORDER BY last_scraped_at ASC NULLS FIRST LIMIT n.
-- The partial index from 0101 covers the filter but not the sort, so Postgres scans+sorts all enabled
-- rows before LIMIT. This composite index serves both the filter (partial WHERE enabled) and the ordering,
-- making it redundant with idx_prometheus_scrape_configs_enabled, which we drop.
CREATE INDEX IF NOT EXISTS idx_prometheus_scrape_configs_claim
  ON apis.prometheus_scrape_configs (last_scraped_at ASC NULLS FIRST)
  WHERE enabled;

DROP INDEX IF EXISTS apis.idx_prometheus_scrape_configs_enabled;
