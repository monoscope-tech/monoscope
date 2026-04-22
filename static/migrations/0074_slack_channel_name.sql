-- Store the default channel name captured from the Slack OAuth incoming webhook
-- response, so the integrations settings page can display a readable chip even
-- when the bot doesn't have access to the channel via conversations.info.
ALTER TABLE apis.slack ADD COLUMN IF NOT EXISTS channel_name TEXT;
