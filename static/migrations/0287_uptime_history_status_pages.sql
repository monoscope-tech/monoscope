BEGIN;

-- Every project with a member has an @everyone team: backfill the stragglers, then create it
-- with the first member however the project was made (onboarding was the only code path).
CREATE OR REPLACE FUNCTION projects.ensure_everyone_team() RETURNS trigger AS $$
BEGIN
  INSERT INTO projects.teams (project_id, created_by, name, description, handle, is_everyone, members, notify_emails, slack_channels, discord_channels, phone_numbers, pagerduty_services)
  VALUES (NEW.project_id, NEW.user_id, 'Everyone', 'All project members and configured integrations', 'everyone', TRUE,
          '{}'::uuid[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[])
  ON CONFLICT DO NOTHING;
  RETURN NEW;
END $$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS project_members_everyone_team ON projects.project_members;
CREATE TRIGGER project_members_everyone_team AFTER INSERT ON projects.project_members
  FOR EACH ROW EXECUTE FUNCTION projects.ensure_everyone_team();

INSERT INTO projects.teams (project_id, created_by, name, description, handle, is_everyone, members, notify_emails, slack_channels, discord_channels, phone_numbers, pagerduty_services)
SELECT p.id, pm.user_id, 'Everyone', 'All project members and configured integrations', 'everyone', TRUE,
       '{}'::uuid[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[]
FROM projects.projects p
CROSS JOIN LATERAL (SELECT user_id FROM projects.project_members WHERE project_id = p.id ORDER BY created_at LIMIT 1) pm
WHERE NOT EXISTS (SELECT 1 FROM projects.teams t WHERE t.project_id = p.id AND t.is_everyone AND t.deleted_at IS NULL)
ON CONFLICT DO NOTHING;

-- A monitor with no team notifies @everyone; store that explicitly so every reader sees it.
CREATE OR REPLACE FUNCTION monitors.default_everyone_team() RETURNS trigger AS $$
BEGIN
  IF NEW.teams = '{}' THEN
    NEW.teams := COALESCE((SELECT ARRAY[id] FROM projects.teams WHERE project_id = NEW.project_id AND is_everyone AND deleted_at IS NULL LIMIT 1), '{}');
  END IF;
  RETURN NEW;
END $$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS query_monitors_everyone_team ON monitors.query_monitors;
CREATE TRIGGER query_monitors_everyone_team BEFORE INSERT OR UPDATE OF teams ON monitors.query_monitors
  FOR EACH ROW EXECUTE FUNCTION monitors.default_everyone_team();

UPDATE monitors.query_monitors SET teams = '{}' WHERE teams = '{}';

-- One row per uptime probe or cron outcome: the history behind day bars, dashboards and status pages.
-- region names the probe location; cron outcomes are evaluated centrally and use 'global'.
CREATE TABLE IF NOT EXISTS apis.check_results (
  project_id UUID NOT NULL,
  check_id UUID NOT NULL,
  kind TEXT NOT NULL CHECK (kind IN ('uptime', 'cron')),
  region TEXT NOT NULL,
  checked_at TIMESTAMPTZ NOT NULL,
  ok BOOLEAN NOT NULL,
  status_code INT,
  duration_ms INT,
  dns_ms INT,
  message TEXT
);
SELECT create_hypertable('apis.check_results', by_range('checked_at', INTERVAL '7 days'), if_not_exists => true);
CREATE INDEX IF NOT EXISTS idx_check_results_check ON apis.check_results (check_id, checked_at DESC);
CREATE INDEX IF NOT EXISTS idx_check_results_project ON apis.check_results (project_id, checked_at DESC);
SELECT add_retention_policy('apis.check_results', INTERVAL '400 days', if_not_exists => true);

-- Probe locations a check runs from; empty means the deployment's default region.
ALTER TABLE apis.prometheus_scrape_configs ADD COLUMN IF NOT EXISTS regions TEXT[] NOT NULL DEFAULT '{}';

CREATE TABLE IF NOT EXISTS apis.status_pages (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  slug TEXT NOT NULL UNIQUE,
  title TEXT NOT NULL,
  description TEXT NOT NULL DEFAULT '',
  visibility TEXT NOT NULL DEFAULT 'public' CHECK (visibility IN ('public', 'internal')),
  check_ids UUID[] NOT NULL DEFAULT '{}'
);
CREATE INDEX IF NOT EXISTS idx_status_pages_project ON apis.status_pages (project_id);

COMMIT;
