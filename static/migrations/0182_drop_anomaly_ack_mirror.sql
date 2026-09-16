-- Retire the acknowledged/archived mirror on apis.anomalies.
--
-- Issue lifecycle state lives on apis.issues alone. These columns were a second
-- copy of it, kept in sync by a prefix sweep over 6.3M rows that no reader ever
-- consulted — every read path (getAnomaliesVM, Endpoints' hash remap, the
-- recently-active-projects probe) uses only target_hash and created_at.
--
-- ORDERING: the writes stopped in 23dafe4f6. This must not ship in the same
-- deploy as that commit — while it rolls, replicas still running the previous
-- image would be writing columns this removes. It is safe once every replica is
-- on 23dafe4f6 or later, which is why it is a separate migration landing after
-- that deploy rather than alongside it.
--
-- Dropping is safe to repeat and safe to run against a database where an earlier
-- attempt half-applied; IF EXISTS covers both.
--
-- acknowledged_by carries anomalies_acknowledged_by_fkey (migration 0001).
-- DROP COLUMN takes a column's own constraints with it, so that needs no
-- separate statement — but it does mean this is not reversible by re-adding the
-- columns alone.

ALTER TABLE apis.anomalies DROP COLUMN IF EXISTS acknowledged_at;
ALTER TABLE apis.anomalies DROP COLUMN IF EXISTS acknowledged_by;
ALTER TABLE apis.anomalies DROP COLUMN IF EXISTS archived_at;
