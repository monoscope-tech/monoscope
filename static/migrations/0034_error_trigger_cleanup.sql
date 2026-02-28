-- Remove unused trigger arguments from error_created_anomaly trigger.
-- new_error_proc() never reads TG_ARGV; the three string literals were leftover noise.
DROP TRIGGER IF EXISTS error_created_anomaly ON apis.error_patterns;
CREATE TRIGGER error_created_anomaly AFTER INSERT ON apis.error_patterns FOR EACH ROW EXECUTE PROCEDURE apis.new_error_proc();
