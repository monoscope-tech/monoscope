-- Evidence needed before an LLM verdict is allowed to merge endpoints.
--
-- A "param" verdict says N endpoints are one route. Acting on it deletes their
-- issues and cannot be undone, and no mechanical check can confirm it — a
-- template built from eight route words passes every structural test one built
-- from eight ids does. So the confidence has to come from asking more than once
-- and from watching the population behave.
--
-- `confirmations` counts independent passes that agreed on "param". Each pass
-- sees the group in a different batch, so agreement is not one sample repeated.
--
-- `first_member_count` is the size at the first verdict. Compared against the
-- current size it answers the question no single snapshot can: is this an open
-- set? Id families keep gaining members forever; a family of verbs
-- (verify_phone, deactivate_user, update_email) is finished the day it ships.
-- Growth between confirmations is the strongest evidence available here and it
-- costs nothing to collect.
--
-- `applied_at` / `applied_canonical_hashes` mark the merges this table caused,
-- so cleanup can leave them alone during the quarantine and a later pass can
-- undo them. It is an array because one verdict about one path position merges
-- every suffix underneath it, which is several templates, not one.
ALTER TABLE apis.endpoint_group_reviews
    ADD COLUMN IF NOT EXISTS confirmations          INT NOT NULL DEFAULT 1,
    ADD COLUMN IF NOT EXISTS first_member_count     INT NOT NULL DEFAULT 0,
    ADD COLUMN IF NOT EXISTS applied_at             TIMESTAMPTZ,
    ADD COLUMN IF NOT EXISTS applied_canonical_hashes TEXT[] NOT NULL DEFAULT '{}',
    ADD COLUMN IF NOT EXISTS reverted_at            TIMESTAMPTZ;

-- Cleanup consults this on every batch to honour the quarantine.
CREATE INDEX IF NOT EXISTS idx_endpoint_group_reviews_applied
    ON apis.endpoint_group_reviews (project_id, applied_at)
    WHERE applied_at IS NOT NULL;
