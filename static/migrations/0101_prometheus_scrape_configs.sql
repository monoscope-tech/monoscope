-- Prometheus scrape targets: user-configured /metrics endpoints the platform
-- polls on a per-target interval, parses (text exposition format) and ingests
-- as metrics. Multiple targets per project are allowed.
CREATE TABLE IF NOT EXISTS apis.prometheus_scrape_configs (
  id                      UUID        NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  project_id              UUID        NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
  created_at              TIMESTAMP   WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  updated_at              TIMESTAMP   WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
  name                    TEXT        NOT NULL DEFAULT '',
  url                     TEXT        NOT NULL,
  scrape_interval_seconds BIGINT      NOT NULL DEFAULT 60,  -- BIGINT: Haskell Int decodes as int8 (see 0098)
  auth_header             TEXT,                                  -- optional, e.g. "Bearer <token>"
  extra_labels            JSONB       NOT NULL DEFAULT '{}',     -- static labels merged onto every sample
  enabled                 BOOLEAN     NOT NULL DEFAULT TRUE,
  last_scraped_at         TIMESTAMP   WITH TIME ZONE,
  last_status             TEXT                                   -- 'ok' or the last error message
);
SELECT manage_updated_at('apis.prometheus_scrape_configs');
CREATE INDEX IF NOT EXISTS idx_prometheus_scrape_configs_project_id ON apis.prometheus_scrape_configs(project_id);
-- Partial index for the scrape ticker: only enabled targets are ever polled.
CREATE INDEX IF NOT EXISTS idx_prometheus_scrape_configs_enabled ON apis.prometheus_scrape_configs(enabled) WHERE enabled;
