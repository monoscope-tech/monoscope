-- Restore apis.slack.webhook_url. Dropped in 0077 on a misread of the usage
-- (the audit flagged it as "never read" because the only reader was the row
-- round-trip, not the Slack client). We actually need it: for the OAuth-time
-- default channel — especially private channels the user picks in Slack's
-- OAuth consent screen — the webhook URL is the ONLY way to post without
-- forcing the user to /invite the bot separately. The bot user is not
-- auto-added to the selected channel, only a channel-bound webhook is issued.
--
-- Alerts routed to the default channel go via the webhook (no bot-membership
-- required, no thread_ts support). Alerts to any other channel the user adds
-- continue to use chat.postMessage with the bot token, which does require
-- membership.

BEGIN;

ALTER TABLE apis.slack ADD COLUMN IF NOT EXISTS webhook_url TEXT;

COMMIT;
