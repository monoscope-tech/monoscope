-- Deployment environment on the service-map rollup, so the map can answer "prod only" the
-- way Datadog's flow map does with its Env facet.
--
-- The empty string, not NULL, for spans that carry no environment: it keeps the column in
-- the primary key (NULL is not comparable there, so a NULLable env would let the same hop
-- upsert twice and double every count) and it gives the UI one honest bucket to label
-- "unset" rather than a hole.
--
-- Existing rows keep '' and are indistinguishable from genuinely unlabelled traffic, which
-- is the truthful outcome: we did not record their environment, and back-filling one would
-- invent it. The rollup rewrites the last buckets continuously, so real values appear within
-- a bucket or two of deploy.
BEGIN;

ALTER TABLE apis.service_dependency_edges
    ADD COLUMN IF NOT EXISTS env TEXT NOT NULL DEFAULT '';

-- The hop identity now includes the environment: prod and staging calling the same
-- dependency are different edges, and collapsing them would report one system's traffic as
-- the other's.
ALTER TABLE apis.service_dependency_edges
    DROP CONSTRAINT IF EXISTS service_dependency_edges_pkey;

ALTER TABLE apis.service_dependency_edges
    ADD PRIMARY KEY (project_id, bucket, source_key, source_kind, target_key, target_kind, env);

COMMIT;
