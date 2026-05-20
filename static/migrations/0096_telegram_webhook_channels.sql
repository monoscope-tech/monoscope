-- Add two new notification channels: Telegram and generic Webhooks.
-- Following the existing pattern in projects.teams: an array column per channel
-- holding the per-target identifiers, plus 'telegram'/'webhook' tokens become
-- valid entries in the inverse-whitelist `disabled_channels` column.

ALTER TABLE projects.teams
  ADD COLUMN IF NOT EXISTS telegram_chats TEXT[] NOT NULL DEFAULT '{}',
  ADD COLUMN IF NOT EXISTS webhook_urls   TEXT[] NOT NULL DEFAULT '{}';
