-- Drop error notification triggers; issue creation + notification now handled in Haskell
DROP TRIGGER IF EXISTS error_created_anomaly ON apis.error_patterns;
DROP TRIGGER IF EXISTS error_regressed_trigger ON apis.error_patterns;
DROP FUNCTION IF EXISTS apis.new_error_proc();
