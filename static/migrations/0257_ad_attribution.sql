BEGIN;

-- Ad click IDs, UTMs and Meta browser IDs captured before signup (Pkg.AdConversions).
ALTER TABLE users.users ADD COLUMN IF NOT EXISTS attribution JSONB;

-- Rows for a Google Ads click-conversion upload (Goals > Conversions > Uploads). A project's
-- activation is credited to its first member, the account that created it. order_id matches the
-- in-app gtag transaction_id, so a conversion the tag already counted is not counted twice.
CREATE OR REPLACE VIEW users.google_ads_offline_conversions AS
WITH clicks AS (
  SELECT id, created_at, attribution->>'gclid' AS gclid, attribution->>'gbraid' AS gbraid, attribution->>'wbraid' AS wbraid,
         to_timestamp((attribution->>'clicked_at')::bigint) AS clicked_at
  FROM users.users
  WHERE attribution ?| ARRAY['gclid', 'gbraid', 'wbraid']
), conversions AS (
  SELECT c.*, 'signup' AS conversion_name, c.created_at AS conversion_at, 20 AS conversion_value, 'signup-' || c.id AS order_id
  FROM clicks c
  UNION ALL
  SELECT c.*, 'activated', am.occurred_at, 100, 'activated-' || am.project_id
  FROM projects.activation_milestones am
  JOIN LATERAL (SELECT user_id FROM projects.project_members WHERE project_id = am.project_id ORDER BY created_at, id LIMIT 1) owner ON TRUE
  JOIN clicks c ON c.id = owner.user_id
  WHERE am.milestone = 'ingest_verified'
)
SELECT gclid, gbraid, wbraid, conversion_name, conversion_at,
       to_char(conversion_at AT TIME ZONE 'UTC', 'YYYY-MM-DD HH24:MI:SS"+0000"') AS conversion_time,
       conversion_value, 'USD' AS conversion_currency, order_id, clicked_at
FROM conversions
WHERE conversion_at < COALESCE(clicked_at, conversion_at) + INTERVAL '90 days';

COMMIT;
