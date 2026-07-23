-- Errors extracted from trace-less logs persisted trace ids as '' instead of NULL,
-- making the issue-detail page fetch "trace ''" (a scan of every trace-less row).
UPDATE apis.error_patterns SET recent_trace_id = NULL WHERE recent_trace_id = '';
UPDATE apis.error_patterns SET first_trace_id = NULL WHERE first_trace_id = '';
