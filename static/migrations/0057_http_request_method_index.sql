CREATE INDEX IF NOT EXISTS idx_logs_spans_http_method
  ON otel_logs_and_spans (project_id, attributes___http___request___method)
  WHERE attributes___http___request___method IS NOT NULL;
