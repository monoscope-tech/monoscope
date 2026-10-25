-- Free-tier sampling counters per capped project, UTC hour and pool (see Pkg.Sampling). `seen` is
-- counted before sampling, so yesterday's hours forecast today's traffic; `kept` is what landed.
CREATE TABLE IF NOT EXISTS projects.ingest_sampling (
  project_id uuid        NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  hour       timestamptz NOT NULL,
  is_error   boolean     NOT NULL,
  seen       bigint      NOT NULL DEFAULT 0,
  kept       bigint      NOT NULL DEFAULT 0,
  PRIMARY KEY (project_id, hour, is_error)
);
CREATE INDEX IF NOT EXISTS ingest_sampling_hour_idx ON projects.ingest_sampling (hour);
