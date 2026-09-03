-- Verdicts from the LLM review of error groups, and the audit trail that lets a
-- merge be taken back.
--
-- Mirrors apis.endpoint_group_reviews (0131) deliberately: same three jobs — dedup
-- key so an unchanged group is not re-asked forever, audit log for a decision a
-- model made, and a report of recurring shapes that are the deterministic masks we
-- have not written yet.
--
-- Two differences from the endpoint table, both because errors are cheaper to get
-- wrong:
--
--   * `group_key` is a SHAPE hash (error type + normalised message), not a pattern
--     hash. Fingerprints get re-keyed — the 2026-09-03 hex fix re-keyed every open
--     pattern — and a review keyed on `hash` would be orphaned by that. A shape is
--     computed from content a re-hash does not change.
--   * `applied_canonical_ids` holds row UUIDs, not hashes, for the same reason.
--
-- `verdict` reuses the endpoint vocabulary: 'param' (one bug, merge them), 'routes'
-- (distinct bugs that merely read alike, do not merge) or 'mixed'.
CREATE TABLE IF NOT EXISTS apis.error_group_reviews (
    id                    UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    created_at            TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    project_id            UUID NOT NULL,
    group_key             TEXT NOT NULL,
    members_hash          TEXT NOT NULL,
    member_count          INT NOT NULL,
    first_member_count    INT NOT NULL DEFAULT 0,
    verdict               TEXT NOT NULL,
    shape                 TEXT NOT NULL DEFAULT '',
    confirmations         INT NOT NULL DEFAULT 1,
    -- Set when the refute pass has been run and did NOT overturn the proposal.
    -- Separate from confirmations because agreeing twice with the same question is
    -- weaker evidence than surviving the opposite one.
    refuted_at            TIMESTAMPTZ,
    survived_refute       BOOLEAN NOT NULL DEFAULT FALSE,
    applied_at            TIMESTAMPTZ,
    applied_canonical_ids UUID[] NOT NULL DEFAULT '{}',
    reverted_at           TIMESTAMPTZ
);

-- One live verdict per group; a membership change replaces it.
CREATE UNIQUE INDEX IF NOT EXISTS idx_error_group_reviews_key
    ON apis.error_group_reviews (project_id, group_key);

-- The quarantine scan: applied, not reverted, still inside the window.
CREATE INDEX IF NOT EXISTS idx_error_group_reviews_quarantine
    ON apis.error_group_reviews (project_id, applied_at)
    WHERE applied_at IS NOT NULL AND reverted_at IS NULL;

-- Recurring shapes are the report worth reading — the masks Phase 4 distils.
CREATE INDEX IF NOT EXISTS idx_error_group_reviews_shape
    ON apis.error_group_reviews (verdict, shape);

-- Where the corpus sweep got to, so a run that dies resumes instead of restarting
-- or — worse — declaring the whole corpus done after its first page, which is
-- exactly how the replay merge stranded 183 sessions.
CREATE TABLE IF NOT EXISTS apis.error_review_cursor (
    project_id     UUID PRIMARY KEY,
    last_shape_key TEXT NOT NULL DEFAULT '',
    updated_at     TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    swept_at       TIMESTAMPTZ
);
