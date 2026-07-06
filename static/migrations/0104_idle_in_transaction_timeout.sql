-- Backstop for the 2026-07-06 outage: a session left `idle in transaction`
-- (last statement done, COMMIT never sent — client/pooler orphaned it) holds
-- its locks forever and wedges the table. `statement_timeout` does not cover
-- this state; only `idle_in_transaction_session_timeout` does.
--
-- Data.Effectful.Hasql.guardWriteTx sets this per-write-transaction, but that
-- only covers the Hasql pool. This database-level default catches everything
-- else too — the postgresql-simple pool, any path that bypasses guardWriteTx,
-- and PgDog server sessions orphaned mid-transaction after a client disconnect.
--
-- Deliberately ONLY the idle-in-transaction timeout: it aborts idle open
-- transactions but never interrupts a running query, so it is safe as a blanket
-- default (unlike a global statement_timeout, which would kill legitimate long
-- dashboard aggregations). current_database() keeps this portable across
-- prod/dev/test where the database name differs.
DO $$
BEGIN
  EXECUTE format(
    'ALTER DATABASE %I SET idle_in_transaction_session_timeout = %L',
    current_database(), '60s');
END $$;
