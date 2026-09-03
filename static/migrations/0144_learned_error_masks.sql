-- Masks distilled from error groups the review loop merged, so the next variant is
-- grouped at ingest instead of after a notification has already gone out.
--
-- Mirrors apis.learned_id_rules (0133), which does the same job for endpoint path
-- segments: the model's verdict is expensive and arrives late, the rule it implies is
-- free and arrives first.
--
-- A mask is a literal prefix and suffix around a part that varies. `SB-{hex} not
-- processing` becomes prefix "SB-", suffix " not processing" — anchored at both ends
-- on purpose, because an unanchored mask is a wildcard and a wildcard merges
-- everything eventually.
--
-- Scoped to (project, error_type). A mask learned from one project's order ids has no
-- authority over another's, and the same literal in a different error type is a
-- different claim.
CREATE TABLE IF NOT EXISTS apis.learned_error_masks (
    id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    created_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    project_id    UUID NOT NULL,
    error_type    TEXT NOT NULL,
    prefix        TEXT NOT NULL,
    suffix        TEXT NOT NULL,
    -- Smallest varying part this mask will swallow. Derived from the group it came
    -- from, so a mask learned over 12-character ids does not also eat a 2-character
    -- one that happens to sit between the same literals.
    min_var_len   INT NOT NULL,
    -- The group whose verdict authorised it, for the audit trail and so a revert can
    -- find the mask it justified.
    group_key     TEXT NOT NULL,
    -- Set when a mask is withdrawn. Kept rather than deleted: a mask that had to be
    -- withdrawn is a fact worth not re-deriving.
    retired_at    TIMESTAMPTZ
);

-- One live mask per literal pair per error type.
CREATE UNIQUE INDEX IF NOT EXISTS idx_learned_error_masks_key
    ON apis.learned_error_masks (project_id, error_type, prefix, suffix);

CREATE INDEX IF NOT EXISTS idx_learned_error_masks_live
    ON apis.learned_error_masks (project_id)
    WHERE retired_at IS NULL;
