ALTER TABLE otel_metrics_meta ADD COLUMN IF NOT EXISTS metric_labels TEXT[] NOT NULL DEFAULT '{}';
