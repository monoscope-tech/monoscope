-- Replace path-inferred API-change service labels with the endpoint's recorded service.
-- Host labels are read from issue_data; service remains a valid telemetry scope.
UPDATE apis.issues i
SET service = (
  SELECT NULLIF(btrim(e.service_name), '')
  FROM apis.endpoints e
  WHERE e.project_id = i.project_id AND e.hash = i.target_hash
)
WHERE i.issue_type = 'api_change';
