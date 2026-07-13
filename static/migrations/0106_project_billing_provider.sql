-- Store the payment provider explicitly instead of guessing it at read time from
-- the shape of sub_id (the old billingProvider :: Maybe Text -> BillingProvider).
-- Webhooks now write billing_provider directly; this backfills historical rows with
-- the same inference so the column is authoritative going forward. Values match the
-- WrappedEnumSC encoding of BillingProvider:
--   'stripe_provider' | 'lemon_squeezy_provider' | 'no_billing_provider'.
--
-- MUST remain the LAST column on projects.projects: Project/ProjectListItem decode
-- positionally via `p.*`/`pp.*`, and the billingProvider field is appended last in both.
ALTER TABLE projects.projects
  ADD COLUMN IF NOT EXISTS billing_provider TEXT NOT NULL DEFAULT 'no_billing_provider';

-- LIKE 'sub\_%' escapes the underscore so it matches the literal "sub_" prefix
-- (bare 'sub_%' would treat _ as a wildcard) — exact parity with billingProviderFromSubId.
-- WHERE excludes rows that would only be rewritten to the existing default (no-op writes).
UPDATE projects.projects
SET billing_provider = CASE
      WHEN sub_id LIKE 'sub\_%' THEN 'stripe_provider'
      ELSE 'lemon_squeezy_provider'
    END
WHERE sub_id LIKE 'sub\_%' OR sub_id ~ '^[0-9]+$';
