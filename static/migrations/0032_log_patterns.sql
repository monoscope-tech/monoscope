-- ALTER TYPE ADD VALUE must run outside a transaction (PG 12+ allows IF NOT EXISTS in txn).
-- Assumption: the migration runner does NOT wrap the whole file in an implicit transaction.
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'log_pattern';
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'log_pattern_rate_change';

BEGIN;

CREATE TABLE IF NOT EXISTS apis.log_patterns (
    id                      BIGSERIAL PRIMARY KEY,
    project_id              UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
    created_at              TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at              TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    log_pattern             TEXT NOT NULL,
    pattern_hash            TEXT NOT NULL,
    source_field            TEXT NOT NULL DEFAULT 'summary',

    service_name            TEXT,
    log_level               TEXT,
    sample_message          TEXT,
    trace_id                TEXT,
    state                   TEXT NOT NULL DEFAULT 'new',  -- 'new', 'acknowledged', 'ignored'
    first_seen_at           TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_seen_at            TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    occurrence_count        BIGINT NOT NULL DEFAULT 1,

    acknowledged_by         UUID REFERENCES users.users(id),
    acknowledged_at         TIMESTAMPTZ,

    -- Baseline for volume spike detection
    baseline_state                TEXT NOT NULL DEFAULT 'learning',  -- 'learning', 'established'
    baseline_volume_hourly_mean   FLOAT,
    baseline_volume_hourly_mad    FLOAT,
    baseline_samples              INT NOT NULL DEFAULT 0,
    baseline_updated_at           TIMESTAMPTZ,

    UNIQUE(project_id, source_field, pattern_hash)
);

SELECT manage_updated_at('apis.log_patterns');

CREATE INDEX IF NOT EXISTS idx_log_patterns_project_state ON apis.log_patterns(project_id, state);
CREATE INDEX IF NOT EXISTS idx_log_patterns_last_seen ON apis.log_patterns(project_id, last_seen_at DESC);
CREATE INDEX IF NOT EXISTS idx_log_patterns_service ON apis.log_patterns(project_id, service_name);

CREATE INDEX IF NOT EXISTS idx_log_patterns_project_hash
  ON apis.log_patterns(project_id, pattern_hash);

ALTER TABLE apis.issues ADD COLUMN IF NOT EXISTS target_hash TEXT NOT NULL DEFAULT '';
ALTER TABLE apis.issues ADD COLUMN IF NOT EXISTS environment TEXT;
ALTER TABLE apis.issues ALTER COLUMN service DROP NOT NULL;

-- Backfill target_hash so the unique index below doesn't fail on existing rows
UPDATE apis.issues SET target_hash = endpoint_hash WHERE target_hash = '';

ALTER TABLE otel_logs_and_spans DROP COLUMN IF EXISTS log_pattern;
ALTER TABLE otel_logs_and_spans DROP COLUMN IF EXISTS summary_pattern;
DROP INDEX IF EXISTS idx_logs_spans_log_pattern;
DROP INDEX IF EXISTS idx_logs_spans_summary_pattern;

CREATE TABLE IF NOT EXISTS apis.log_pattern_hourly_stats (
    project_id    UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
    source_field  TEXT NOT NULL DEFAULT 'summary',
    pattern_hash  TEXT NOT NULL,
    hour_bucket   TIMESTAMPTZ NOT NULL,
    event_count   BIGINT NOT NULL DEFAULT 0,
    PRIMARY KEY (project_id, source_field, pattern_hash, hour_bucket)
);
CREATE INDEX IF NOT EXISTS idx_lp_hourly_stats_project_hour
  ON apis.log_pattern_hourly_stats(project_id, source_field, hour_bucket DESC);

CREATE INDEX IF NOT EXISTS idx_log_patterns_baseline_established
  ON apis.log_patterns(project_id) WHERE baseline_state = 'established';

-- Drop old unique indexes that conflict with the new ON CONFLICT clause
DROP INDEX IF EXISTS apis.idx_apis_issues_project_id_target_hash;
DROP INDEX IF EXISTS apis.unique_open_api_change_issue_per_endpoint;

CREATE UNIQUE INDEX IF NOT EXISTS idx_issues_project_target_type_open
  ON apis.issues (project_id, target_hash, issue_type)
  WHERE acknowledged_at IS NULL AND archived_at IS NULL;

COMMIT;
