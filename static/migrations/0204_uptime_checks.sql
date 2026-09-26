-- Uptime checks reuse the scrape-target table and its lease/dispatch: a check is a
-- target of kind 'uptime' whose response status is compared to expected_status.
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'uptime';
ALTER TABLE apis.prometheus_scrape_configs
  ADD COLUMN IF NOT EXISTS kind TEXT NOT NULL DEFAULT 'prometheus',
  ADD COLUMN IF NOT EXISTS expected_status INT,
  ADD COLUMN IF NOT EXISTS consecutive_failures INT NOT NULL DEFAULT 0;
