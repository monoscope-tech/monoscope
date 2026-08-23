-- Id formats a project has been observed to use, promoted to deterministic rules.
--
-- The point is to stop paying a model to answer the same question. Once a
-- family has been confirmed and merged, the literal prefix its values share
-- ("cus_", "SB-", "a-df0u-") is often enough to recognise the next one on a
-- path nobody has seen yet — no population to accumulate, no call to make.
--
-- Scoped to a project, and optionally a host, on purpose. "cus_" means Stripe
-- customer in one customer's API and could mean anything in another's; a rule
-- learned from one project's traffic has no business classifying another's.
--
-- `collisions_checked` records how many known-static segments the prefix was
-- tested against before being accepted. A rule is only admitted when that test
-- found none: the merge decision itself cannot be verified mechanically, but
-- "does this pattern match something we already know is a route word" can be,
-- and that is the whole basis for trusting a promoted rule more than the
-- verdict it came from.
CREATE TABLE IF NOT EXISTS apis.learned_id_rules (
    id                 UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    created_at         TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    project_id         UUID NOT NULL,
    host               TEXT NOT NULL DEFAULT '',
    prefix             TEXT NOT NULL,
    min_length         INT NOT NULL,
    shape              TEXT NOT NULL DEFAULT '',
    source_group_key   TEXT NOT NULL DEFAULT '',
    collisions_checked INT NOT NULL DEFAULT 0,
    disabled_at        TIMESTAMPTZ
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_learned_id_rules_key
    ON apis.learned_id_rules (project_id, host, prefix);

CREATE INDEX IF NOT EXISTS idx_learned_id_rules_live
    ON apis.learned_id_rules (project_id) WHERE disabled_at IS NULL;
