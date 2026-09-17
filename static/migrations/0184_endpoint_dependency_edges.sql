-- Endpoint-scoped, five-minute dependency rollups.  The endpoint hash is part
-- of the primary key: the same downstream service can be healthy for one API
-- route and failing for another, so a service-level edge cannot answer an
-- endpoint investigation without rejoining raw spans at dashboard time.
--
-- `trace_count` is retained separately from calls: retry-heavy traces are a
-- materially different incident shape from many independent requests.
BEGIN;

CREATE TABLE IF NOT EXISTS apis.endpoint_dependency_edges (
    project_id       UUID        NOT NULL,
    bucket           TIMESTAMPTZ NOT NULL,
    env              TEXT        NOT NULL,
    endpoint_hash    TEXT        NOT NULL,
    target_key       TEXT        NOT NULL,
    target_kind      TEXT        NOT NULL,
    req_count        BIGINT      NOT NULL,
    trace_count      BIGINT      NOT NULL,
    error_count      BIGINT      NOT NULL,
    sum_duration_ns  BIGINT      NOT NULL,
    lat_hist         JSONB       NOT NULL,
    updated_at       TIMESTAMPTZ NOT NULL DEFAULT now(),
    PRIMARY KEY (project_id, bucket, env, endpoint_hash, target_key, target_kind)
);

SELECT create_hypertable('apis.endpoint_dependency_edges', by_range('bucket', INTERVAL '1 day'),
                         migrate_data => true, if_not_exists => true);
SELECT add_retention_policy('apis.endpoint_dependency_edges', INTERVAL '30 days', if_not_exists => true);
CREATE INDEX IF NOT EXISTS endpoint_dependency_edges_lookup_idx
    ON apis.endpoint_dependency_edges (project_id, endpoint_hash, bucket DESC);

COMMIT;
