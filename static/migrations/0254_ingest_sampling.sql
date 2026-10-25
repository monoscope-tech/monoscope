-- Free-tier sampling counters per capped project and UTC hour (see Pkg.Sampling). `seen` is
-- counted before sampling, so yesterday's hours forecast today's traffic; `kept` is what landed.
CREATE TABLE IF NOT EXISTS projects.ingest_sampling (
  project_id  uuid        NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  hour        timestamptz NOT NULL,
  seen        bigint      NOT NULL DEFAULT 0,
  kept        bigint      NOT NULL DEFAULT 0,
  seen_errors bigint      NOT NULL DEFAULT 0,
  kept_errors bigint      NOT NULL DEFAULT 0,
  PRIMARY KEY (project_id, hour)
);
