-- Service-map rollup: one row per (project, 5-minute bucket, caller, callee).
--
-- The global map cannot be derived at read time. Deriving edges needs a self-join of the
-- span table on (trace_id, parent_id = span_id); on TimeFusion that join shape instantiates
-- an unbounded dedup buffer per scan and is exactly what OOM-crash-looped production. Rolling
-- the same join over one closed 5-minute slice touches 1/288th of the data, runs once per
-- project instead of once per page view, and leaves a table small enough that any range
-- answers from Postgres in milliseconds.
--
-- Latency is a sparse log-scale histogram (4 buckets/octave over microseconds) kept as a
-- bucket->count JSONB object rather than a sketch: it is exactly mergeable, so percentiles
-- for an arbitrary range are read off the merged histogram instead of averaging per-bucket
-- percentiles, which is wrong. The read path selects it as-is and merges in Haskell — a few
-- thousand narrow rows per range — so there is no unnest join on read.
BEGIN;

CREATE TABLE IF NOT EXISTS apis.service_dependency_edges (
    project_id       UUID        NOT NULL,
    bucket           TIMESTAMPTZ NOT NULL,
    source_key       TEXT        NOT NULL,
    source_kind      TEXT        NOT NULL,
    target_key       TEXT        NOT NULL,
    target_kind      TEXT        NOT NULL,
    req_count        BIGINT      NOT NULL,
    error_count      BIGINT      NOT NULL,
    sum_duration_ns  BIGINT      NOT NULL,
    lat_hist         JSONB       NOT NULL,
    updated_at       TIMESTAMPTZ NOT NULL DEFAULT now(),
    -- Both kinds are in the key: the same (caller, callee) pair observed once as a service
    -- hop and once as an inferred dependency are different edges, and leaving the kinds out
    -- would let one silently overwrite the other.
    PRIMARY KEY (project_id, bucket, source_key, source_kind, target_key, target_kind)
);

SELECT create_hypertable('apis.service_dependency_edges', by_range('bucket', INTERVAL '1 day'),
                         migrate_data => true, if_not_exists => true);

SELECT add_retention_policy('apis.service_dependency_edges', INTERVAL '30 days', if_not_exists => true);

CREATE INDEX IF NOT EXISTS service_dependency_edges_project_bucket_idx
    ON apis.service_dependency_edges (project_id, bucket DESC);

COMMIT;
