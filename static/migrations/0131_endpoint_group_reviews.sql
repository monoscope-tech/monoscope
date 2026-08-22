-- Verdicts from the LLM review of endpoint groups the deterministic classifier
-- left alone.
--
-- Three jobs in one table. It is the dedup key, so the interval job does not
-- re-ask the same question about the same unchanged group forever — the review
-- is keyed on a hash of the group's membership, so it is re-asked exactly when
-- the membership changes. It is the audit log for a decision made by a model.
-- And `shape` is the report worth reading: recurring shapes ("stripe customer
-- id", "prefixed order id") are the deterministic rules we have not written yet.
--
-- `verdict` is 'param' (one route, values are a parameter), 'routes' (distinct
-- routes sharing a prefix, do not merge) or 'mixed'.
CREATE TABLE IF NOT EXISTS apis.endpoint_group_reviews (
    id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    created_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    project_id    UUID NOT NULL,
    group_key     TEXT NOT NULL,
    members_hash  TEXT NOT NULL,
    member_count  INT NOT NULL,
    verdict       TEXT NOT NULL,
    shape         TEXT NOT NULL DEFAULT '',
    applied       BOOLEAN NOT NULL DEFAULT FALSE
);

-- One live verdict per group; a membership change replaces it.
CREATE UNIQUE INDEX IF NOT EXISTS idx_endpoint_group_reviews_key
    ON apis.endpoint_group_reviews (project_id, group_key);

CREATE INDEX IF NOT EXISTS idx_endpoint_group_reviews_shape
    ON apis.endpoint_group_reviews (verdict, shape);
