-- Live Tail subscriptions grow a second surface: the Events tab's live toggle.
--
-- Events streams the same pre-write matches Live Tail does, but it is not a log view and it
-- has no service to gate on. Rather than bolt a nullable "is this the other kind" flag onto
-- the row, the table now carries an explicit discriminator, and the code reads the pair
-- (scope, service) back into a sum type where the gate is structural — `logs_only` cannot be
-- constructed without a service, `all_signals` has no place to put one.
--
-- That is why `service` becomes nullable here. It is not a weakening: 0125 could only express
-- "always gated", and the gate is still enforced for `logs_only` — now by the type that reads
-- the row rather than by a NOT NULL that the second surface would have had to lie to.
--
-- `columns` records what the browser is rendering. The server cannot derive it: a query's
-- final column list may contain SQL expressions only the database can evaluate, so the client
-- states its own column order and ingest resolves each name against the in-memory record.
-- Empty for `logs_only`, which renders a fixed projection and needs no such list.
BEGIN;

ALTER TABLE projects.live_tail_subscriptions
    ADD COLUMN IF NOT EXISTS scope   TEXT   NOT NULL DEFAULT 'logs_only',
    ADD COLUMN IF NOT EXISTS columns TEXT[] NOT NULL DEFAULT '{}';

ALTER TABLE projects.live_tail_subscriptions
    ALTER COLUMN service DROP NOT NULL;

-- Belt and braces for the invariant the reader already enforces by dropping bad rows: a
-- service-gated subscription must actually name one. Without this a future writer could
-- persist `logs_only` with a NULL service, and every ingest pod would silently discard it —
-- a tail that registers, leases, renews, and never matches anything.
ALTER TABLE projects.live_tail_subscriptions
    DROP CONSTRAINT IF EXISTS live_tail_scope_service_ck;
ALTER TABLE projects.live_tail_subscriptions
    ADD CONSTRAINT live_tail_scope_service_ck
    CHECK (scope <> 'logs_only' OR service IS NOT NULL);

COMMIT;
