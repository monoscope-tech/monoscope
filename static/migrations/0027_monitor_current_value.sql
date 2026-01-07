-- Cache the evaluated query result to avoid running eval() on every dashboard load
ALTER TABLE monitors.query_monitors ADD COLUMN current_value integer DEFAULT 0;
