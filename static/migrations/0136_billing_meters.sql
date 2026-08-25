-- Pricing v2: three separately-metered dimensions.
--   events            $1 / 1,000,000    (existing `events_usage` meter)
--   metric_datapoints $1 / 10,000,000   (counted since 0088, never submitted)
--   session_replays   $1 / 1,000        (new; counted from projects.replay_sessions)
--
-- Additive only. Every existing submission row was an events submission, so the
-- column default backfills them correctly and no data migration is needed.

-- 1) Meter dimension on submissions. Values are `MeterKind`'s WrappedEnumSC
--    encoding (snake_case of the constructor) — keep this CHECK in sync with
--    the constructors of Models.Projects.Projects.MeterKind.
ALTER TABLE projects.usage_report_submissions
  ADD COLUMN IF NOT EXISTS meter_kind text NOT NULL DEFAULT 'events';

ALTER TABLE projects.usage_report_submissions
  DROP CONSTRAINT IF EXISTS urs_meter_kind_enum;
ALTER TABLE projects.usage_report_submissions
  ADD CONSTRAINT urs_meter_kind_enum
  CHECK (meter_kind IN ('events', 'metric_datapoints', 'session_replays'));

-- 2) Replay counts snapshotted per window. projects.replay_sessions is subject
--    to a 30-day retention DELETE, so a historical window can never be
--    recomputed from it — this column is the durable record.
--    Deliberately NOT added into total_requests: replays are priced 1000x
--    differently and would corrupt the "$1 per 1M requests" estimate that
--    reads total_requests.
ALTER TABLE apis.daily_usage
  ADD COLUMN IF NOT EXISTS total_replays bigint NOT NULL DEFAULT 0;

-- 3) Per-meter Lemon Squeezy subscription items.
--    Stripe addresses a meter by customer + meter event name and needs no row
--    here. Lemon Squeezy addresses a usage record ONLY by subscription item, so
--    each metered dimension needs its own item on the subscription. The Events
--    meter keeps using projects.first_sub_item_id, so existing subscriptions are
--    untouched; a missing row for the other meters is what keeps them dormant.
CREATE TABLE IF NOT EXISTS projects.billing_meter_items (
  project_id  uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  meter_kind  text NOT NULL,
  sub_item_id text NOT NULL,
  created_at  timestamptz NOT NULL DEFAULT now(),
  updated_at  timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (project_id, meter_kind),
  CONSTRAINT bmi_meter_kind_enum CHECK (meter_kind IN ('events', 'metric_datapoints', 'session_replays')),
  CONSTRAINT bmi_sub_item_id_nonempty CHECK (sub_item_id <> '')
);

-- 4) Billing counts replay sessions started in a window. The table only had
--    (project_id) and (last_event_at) WHERE NOT merged; neither serves a
--    per-project time-range count.
CREATE INDEX IF NOT EXISTS idx_replay_sessions_project_created
  ON projects.replay_sessions (project_id, created_at);
