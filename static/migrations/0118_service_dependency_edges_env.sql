-- Environment-dimensioned service-map rollup, so the map can answer "prod only" the way
-- Datadog's flow map does with its Env facet.
--
-- A NEW TABLE rather than a column on service_dependency_edges, and that is the whole point
-- of this file. The environment has to be part of the hop's identity — prod and staging
-- calling the same dependency are different hops, and a shared key reports one system's
-- traffic as the other's. But Postgres infers ON CONFLICT from a *complete* unique index, so
-- widening the existing primary key stops the currently-deployed six-column upsert matching
-- anything at all, and the rollup silently stops writing until the new code ships. Adding the
-- column outside the key is worse, not safer: two environments in one bucket produce two rows
-- with identical keys in a single INSERT ... SELECT unnest, which errors outright with
-- "ON CONFLICT DO UPDATE command cannot affect row a second time".
--
-- CREATE TABLE has neither problem. The running code never references this table, so this
-- migration is inert until the code that reads it is deployed, in either order.
BEGIN;

CREATE TABLE IF NOT EXISTS apis.service_dependency_edges_env (
    project_id       UUID        NOT NULL,
    bucket           TIMESTAMPTZ NOT NULL,
    env              TEXT        NOT NULL,
    source_key       TEXT        NOT NULL,
    source_kind      TEXT        NOT NULL,
    target_key       TEXT        NOT NULL,
    target_kind      TEXT        NOT NULL,
    req_count        BIGINT      NOT NULL,
    error_count      BIGINT      NOT NULL,
    sum_duration_ns  BIGINT      NOT NULL,
    lat_hist         JSONB       NOT NULL,
    updated_at       TIMESTAMPTZ NOT NULL DEFAULT now(),
    PRIMARY KEY (project_id, bucket, env, source_key, source_kind, target_key, target_kind)
);

SELECT create_hypertable('apis.service_dependency_edges_env', by_range('bucket', INTERVAL '1 day'),
                         migrate_data => true, if_not_exists => true);

SELECT add_retention_policy('apis.service_dependency_edges_env', INTERVAL '30 days', if_not_exists => true);

CREATE INDEX IF NOT EXISTS service_dependency_edges_env_project_bucket_idx
    ON apis.service_dependency_edges_env (project_id, bucket DESC);

COMMIT;
