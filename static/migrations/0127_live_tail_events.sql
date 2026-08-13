-- The transport of last resort for Live Tail: a relay table.
--
-- Matched rows have to cross from the ingest pod to whichever web pod holds the browser's SSE
-- connection. Kafka does that well and is the queue this system is built around — but it is
-- optional infrastructure: dev boxes, docker-compose, and self-hosted installs routinely run
-- without it, and a feature that quietly does nothing there is a feature that does not work.
--
-- Postgres is the floor because it is the one dependency every deployment already has. No new
-- infrastructure, and Live Tail works everywhere; Kafka becomes an optimisation for fleets
-- that already run it rather than a prerequisite.
--
-- The earlier code chose an in-process hub whenever no brokers were configured. That is right
-- for a genuinely single process and silently wrong the moment ingest and web are separate
-- pods without Kafka — and no process can tell which it is, since a web pod cannot see whether
-- CONSUMER_ONLY pods exist. Guessing was the bug; this table removes the guess.
--
-- UNLOGGED on purpose. Nothing here is worth crash-recovering: a row a browser was not
-- connected to receive is a row it has already missed, reconnects deliberately never rewind,
-- and the reaper deletes everything older than a few seconds anyway. Skipping WAL keeps a
-- table on the ingest hot path from becoming a write amplifier.
BEGIN;

CREATE UNLOGGED TABLE IF NOT EXISTS projects.live_tail_events (
    id              BIGSERIAL   PRIMARY KEY,
    -- Deliberately no FK to live_tail_subscriptions: a row arriving microseconds after its
    -- subscription was deleted is normal, and it must not fail the ingest-side insert. An
    -- orphan is delivered to nobody and reaped seconds later.
    subscription_id UUID        NOT NULL,
    payload         JSONB       NOT NULL,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- The consumer's only query: everything after the last id it saw. The primary key already
-- serves it; this index exists for the reaper, which sweeps by age.
CREATE INDEX IF NOT EXISTS live_tail_events_created_idx
    ON projects.live_tail_events (created_at);

COMMIT;
