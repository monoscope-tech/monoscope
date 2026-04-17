-- Unique index on apis.subscriptions.subscription_id enables idempotent inserts
-- via ON CONFLICT, so replays/manual retries don't leave us without a row.
CREATE UNIQUE INDEX IF NOT EXISTS subscriptions_subscription_id_key
  ON apis.subscriptions (subscription_id);
