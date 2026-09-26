-- Per-error tag distribution rollup (browser, os, release, country, ...), written at
-- ingest so the issue page reads counts instead of scanning telemetry per view.
CREATE TABLE IF NOT EXISTS apis.error_tag_counts (
  project_id UUID NOT NULL,
  error_id UUID NOT NULL REFERENCES apis.error_patterns(id) ON DELETE CASCADE,
  tag_key TEXT NOT NULL,
  tag_value TEXT NOT NULL,
  count BIGINT NOT NULL DEFAULT 0,
  PRIMARY KEY (project_id, error_id, tag_key, tag_value)
);
