-- Make the endpoint-merge cleanup's predicate indexable.
--
-- `apis.anomalies.target_hash` and `apis.issues.target_hash` are an 8-char
-- endpoint hash optionally followed by a 16-char field/shape suffix, so folding
-- a merged-out endpoint's rows onto its canonical one selects with
-- `LEFT(target_hash, 8) = <hash>`. That expression is not sargable against the
-- plain `(project_id, target_hash)` index, and `apis.anomalies` is a
-- multi-million-row table — EXPLAIN showed a Seq Scan per statement. The
-- cleanup job never had merged pairs to process before, so nothing surfaced it.
--
-- Built without CONCURRENTLY on purpose: postgresql-simple-migration sends the
-- whole file as one simple query, which PostgreSQL wraps in an implicit
-- transaction, and CREATE INDEX CONCURRENTLY cannot run inside one. On a large
-- existing database build these by hand with CONCURRENTLY first; the
-- IF NOT EXISTS then makes this migration a no-op there.
CREATE INDEX IF NOT EXISTS idx_anomalies_project_endpoint_prefix
    ON apis.anomalies (project_id, LEFT(target_hash, 8));

CREATE INDEX IF NOT EXISTS idx_issues_project_endpoint_prefix
    ON apis.issues (project_id, LEFT(target_hash, 8));
