-- Promote `deployment.environment.name` to a column on otel_logs_and_spans, so prod/staging
-- becomes a dimension you can filter, facet and scope the whole app by rather than a key
-- buried in the `resource` blob.
--
-- otel_metrics has carried `resource___deployment___environment___name` since 0108; spans and
-- logs never got it, which is why the service map had to grow its own env-dimensioned rollup
-- table (0118) instead of just grouping by a column, and why commit 20506f4 had to revert an
-- env facet that "queried a column Postgres does not have". This is that column.
--
-- Additive and nullable, so it is O(1) on a hypertable this size and inert until the code
-- that writes it ships. Existing rows keep their environment in the `resource` Variant and
-- simply read as unset — no backfill, because rewriting history in TimeFusion is the
-- operation that has repeatedly OOM-killed it, and "unset" is treated as *unknown*, never as
-- *excluded*, by the selector that reads this.
--
-- Deliberately NOT indexed. Environment is low cardinality, so an index on it alone prunes
-- badly; every query that filters on it is already bounded by (project_id, timestamp), which
-- the hypertable's chunk exclusion handles.
BEGIN;

ALTER TABLE otel_logs_and_spans
    ADD COLUMN IF NOT EXISTS resource___deployment___environment___name TEXT;

COMMIT;
