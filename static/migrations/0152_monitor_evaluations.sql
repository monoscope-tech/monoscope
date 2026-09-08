-- Notification charts show the values that the monitor actually evaluated.
-- Source queries can be scalar expressions and cannot always be rebinned safely.
CREATE TABLE monitors.evaluations (
  monitor_id UUID NOT NULL REFERENCES monitors.query_monitors(id) ON DELETE CASCADE,
  evaluated_at TIMESTAMPTZ NOT NULL,
  value DOUBLE PRECISION NOT NULL CHECK (value NOT IN ('NaN'::float8, 'Infinity'::float8, '-Infinity'::float8)),
  status TEXT NOT NULL CHECK (status IN ('normal', 'warning', 'alerting')),
  PRIMARY KEY (monitor_id, evaluated_at)
);

CREATE INDEX evaluations_retention_idx ON monitors.evaluations (evaluated_at);
