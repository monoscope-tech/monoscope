-- Legacy plan names nothing bills for (no subscription): fold them into Free so the
-- free-tier cap applies. "Open Source" stays: self-hosted basic-auth installs use it.
UPDATE projects.projects
   SET payment_plan = 'Free'
 WHERE payment_plan IN ('Hobby', 'Startup', 'Growth', 'GraduatedPricing')
   AND NULLIF(sub_id, '') IS NULL
   AND NULLIF(first_sub_item_id, '') IS NULL
   AND billing_provider = 'no_billing_provider';
