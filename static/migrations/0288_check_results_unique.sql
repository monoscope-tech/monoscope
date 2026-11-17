-- One result per check, region and instant, so concurrent evaluators cannot record it twice.
CREATE UNIQUE INDEX IF NOT EXISTS idx_check_results_unique ON apis.check_results (check_id, region, checked_at);
