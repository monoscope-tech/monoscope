-- Uptime checks choose their request (method, headers, body), a text the response must
-- contain, and the teams their alerts go to (none chosen means @everyone).
ALTER TABLE apis.prometheus_scrape_configs
  ADD COLUMN IF NOT EXISTS http_method text NOT NULL DEFAULT 'get',
  ADD COLUMN IF NOT EXISTS request_headers jsonb NOT NULL DEFAULT '[]',
  ADD COLUMN IF NOT EXISTS request_body text,
  ADD COLUMN IF NOT EXISTS response_contains text,
  ADD COLUMN IF NOT EXISTS teams uuid[] NOT NULL DEFAULT '{}';
