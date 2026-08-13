-- Live Tail stops being a logs-only, one-service view.
--
-- Two changes, both widening what a subscription may say:
--
-- `kind` records which signals the tail shows — `logs` (the default and what every existing
-- row meant), `spans`, or `any`. It used to be implicit: ingest matched `kind = 'log'` for
-- every `logs_only` subscription, so a user who wanted to watch spans arriving had no way to
-- ask. The three modes are a closed set on the Haskell side (`Pkg.LiveTail.SignalKind`), whose
-- derived spelling is the value stored here.
--
-- `service` becomes genuinely optional. 0126 made the column nullable but kept a CHECK forcing
-- `logs_only` to name one, because at the time the service *was* the bound on Live Tail's
-- volume. It is not: the bound is the per-connection queue, which drops the oldest rows and
-- reports the count — the same bound `all_signals` has always relied on. Dropping the CHECK is
-- what lets the page open on a project-wide tail instead of an empty screen and a dropdown.
BEGIN;

ALTER TABLE projects.live_tail_subscriptions
    ADD COLUMN IF NOT EXISTS kind TEXT NOT NULL DEFAULT 'logs';

ALTER TABLE projects.live_tail_subscriptions
    DROP CONSTRAINT IF EXISTS live_tail_scope_service_ck;

COMMIT;
