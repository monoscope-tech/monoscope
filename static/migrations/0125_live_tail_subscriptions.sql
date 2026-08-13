-- Live Tail subscriptions: what each open browser stream is asking ingest to watch for.
--
-- Live Tail matches telemetry on the ingest pod, before the TimeFusion/Postgres write, so the
-- ingest pod has to learn what a browser on some *other* pod is watching. This table is that
-- channel: web pods write a row on registration, ingest pods poll unexpired rows into an
-- in-memory cache. It is coordination state, not user data — nothing here outlives the tab
-- that created it.
--
-- The row is a *lease*, not a registration. `expires_at` is authoritative and every read
-- filters on it, because the failure modes that matter (browser crash, pod kill, laptop lid
-- closed mid-incident) all lose the DELETE. An open SSE connection renews the lease; anything
-- that stops renewing stops matching within one lease period, with no cleanup job in the path.
-- A periodic delete only reclaims space — it must never be what makes a subscription inactive.
--
-- The filter is stored as its original KQL text rather than a serialized AST: the parser is
-- the single source of truth for what a query means, and a stored AST would be a second
-- encoding of that meaning able to drift from it across a deploy. Ingest pods re-parse on
-- cache refresh (a few rows every couple of seconds, so the cost is irrelevant) and drop any
-- row that no longer parses instead of guessing.
BEGIN;

CREATE TABLE IF NOT EXISTS projects.live_tail_subscriptions (
    id           UUID        PRIMARY KEY DEFAULT gen_random_uuid(),
    project_id   UUID        NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
    -- Owner. Every route re-checks project membership; this records who to count against the
    -- per-user limit and is never taken from the request body.
    user_id      UUID        NOT NULL REFERENCES users.users (id) ON DELETE CASCADE,
    -- Required. The service gate is what bounds Live Tail's volume, so it is NOT NULL here
    -- rather than left to the handler to remember.
    service      TEXT        NOT NULL,
    environment  TEXT,
    -- Filter-only KQL as the user typed it. '' means "no extra filter".
    query        TEXT        NOT NULL DEFAULT '',
    created_at   TIMESTAMPTZ NOT NULL DEFAULT now(),
    expires_at   TIMESTAMPTZ NOT NULL
);

-- The ingest cache refresh: every unexpired row, ordered by project. This is the only query
-- on the hot path's side of the table, and it runs on every ingest pod every couple of
-- seconds, so it gets a covering-ish index rather than a sequential scan that grows with
-- abandoned leases.
CREATE INDEX IF NOT EXISTS live_tail_subscriptions_active_idx
    ON projects.live_tail_subscriptions (expires_at, project_id);

-- Per-user and per-project limit counting, and the reaper.
CREATE INDEX IF NOT EXISTS live_tail_subscriptions_owner_idx
    ON projects.live_tail_subscriptions (project_id, user_id, expires_at);

COMMIT;
