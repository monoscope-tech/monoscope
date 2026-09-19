BEGIN;

CREATE TABLE projects.ai_usage (
  id BIGSERIAL PRIMARY KEY,
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  model TEXT NOT NULL,
  source TEXT NOT NULL,
  input_tokens BIGINT NOT NULL CHECK (input_tokens >= 0),
  output_tokens BIGINT NOT NULL CHECK (output_tokens >= 0),
  cost_microusd BIGINT NOT NULL CHECK (cost_microusd >= 0),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX ai_usage_project_created
  ON projects.ai_usage (project_id, created_at DESC);

ALTER TABLE apis.daily_usage
  ADD COLUMN total_ai_input_tokens BIGINT NOT NULL DEFAULT 0,
  ADD COLUMN total_ai_output_tokens BIGINT NOT NULL DEFAULT 0,
  ADD COLUMN total_ai_cost_microusd BIGINT NOT NULL DEFAULT 0;

ALTER TABLE projects.usage_report_submissions
  DROP CONSTRAINT urs_meter_kind_enum;
ALTER TABLE projects.usage_report_submissions
  ADD CONSTRAINT urs_meter_kind_enum
  CHECK (meter_kind IN ('events', 'metric_datapoints', 'session_replays', 'ai_usage'));

ALTER TABLE projects.billing_meter_items
  DROP CONSTRAINT bmi_meter_kind_enum;
ALTER TABLE projects.billing_meter_items
  ADD CONSTRAINT bmi_meter_kind_enum
  CHECK (meter_kind IN ('events', 'metric_datapoints', 'session_replays', 'ai_usage'));

COMMIT;
