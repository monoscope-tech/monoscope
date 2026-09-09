ALTER TABLE apis.slack_investigation_progress ADD COLUMN history_cursor TEXT CHECK (history_cursor IS NULL OR history_cursor <> '');
