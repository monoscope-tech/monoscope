ALTER TABLE apis.slack_reply_publications ADD COLUMN history_cursor TEXT CHECK (history_cursor IS NULL OR history_cursor <> '');
