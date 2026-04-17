-- Split customer_id out of order_id. LemonSqueezy owns order_id; Stripe gets a
-- dedicated customer_id column instead of overloading order_id with the Stripe
-- customer id.
ALTER TABLE projects.projects ADD COLUMN IF NOT EXISTS customer_id TEXT;

-- Backfill rows whose sub_id looks like a Stripe subscription — those rows
-- previously stored the Stripe customer id in order_id.
UPDATE projects.projects
   SET customer_id = order_id
 WHERE sub_id LIKE 'sub_%'
   AND customer_id IS NULL
   AND order_id IS NOT NULL;
