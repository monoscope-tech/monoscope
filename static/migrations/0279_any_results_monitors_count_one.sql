-- "The query has any results" monitors were saved with alert_threshold 0, and a reading
-- breaches when it is >= the threshold, so they alerted on an empty window. They now mean
-- a count of at least 1; a threshold-0 "above" monitor without a warning can mean nothing else.
UPDATE monitors.query_monitors SET alert_threshold = 1
WHERE alert_threshold = 0 AND warning_threshold IS NULL AND NOT trigger_less_than;
