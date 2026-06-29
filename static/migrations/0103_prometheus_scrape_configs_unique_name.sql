-- Each target's name becomes the service.name its metrics are grouped under, and
-- metrics_meta is unique on (project_id, metric_name, service_name) — so two targets sharing
-- a name in one project would silently clobber each other's metadata. Enforce uniqueness.
ALTER TABLE apis.prometheus_scrape_configs
  ADD CONSTRAINT uq_prometheus_scrape_configs_project_name UNIQUE (project_id, name);
