BEGIN;


ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'log_pattern';
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'error_escalating';
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'error_regressed';
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'log_pattern_rate_change';
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'endpoint_latency_degradation';
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'endpoint_error_rate_spike';
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'endpoint_volume_rate_change';

DROP TABLE IF EXISTS apis.issues CASCADE;

CREATE TABLE apis.issues (
    id                  UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    project_id          UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
    created_at          TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at          TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    issue_type          apis.issue_type NOT NULL,
    source_type         TEXT NOT NULL,              -- 'error', 'log_pattern', 'endpoint', 'shape'
    target_hash         TEXT NOT NULL,              -- links to error.hash, log_pattern.pattern_hash, endpoint.hash

    title               TEXT NOT NULL DEFAULT '',
    service             TEXT,
    environment         TEXT,
    severity            TEXT NOT NULL DEFAULT 'warning',  -- 'critical', 'warning', 'info'
    critical            BOOLEAN NOT NULL DEFAULT FALSE,
    -- Lifecycle
    first_seen_at       TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_seen_at        TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    acknowledged_at     TIMESTAMPTZ,
    acknowledged_by     UUID REFERENCES users.users(id) ON DELETE SET NULL,
    resolved_at         TIMESTAMPTZ,
    recommended_action  TEXT NOT NULL DEFAULT '',
    migration_complexity TEXT NOT NULL DEFAULT 'n/a', -- 'low', 'medium', 'high', 'n/a'
    request_payloads    JSONB NOT NULL DEFAULT '[]'::jsonb,
    response_payloads   JSONB NOT NULL DEFAULT '[]'::jsonb,
    archived_at         TIMESTAMPTZ,
    llm_enhanced_at     TIMESTAMPTZ,
    llm_enhancement_version INT,
    -- Flexible category-specific data
    issue_data          JSONB NOT NULL DEFAULT '{}'
);


  , -- Actions
    recommendedAction :: Text
  , migrationComplexity :: Text -- "low", "medium", "high", "n/a"
  -- Data payload (polymorphic based on issueType)
  , issueData :: Aeson AE.Value
  , -- Payload changes tracking (for API changes)
    requestPayloads :: Aeson [PayloadChange]
  , responsePayloads :: Aeson [PayloadChange]
  , -- LLM enhancement tracking
    llmEnhancedAt :: Maybe UTCTime
  , llmEnhancementVersion :: Maybe Int
SELECT manage_updated_at('apis.issues');

-- Indexes
CREATE INDEX idx_issues_project_id ON apis.issues(project_id);
CREATE INDEX idx_issues_project_created ON apis.issues(project_id, created_at DESC);
CREATE INDEX idx_issues_issue_type ON apis.issues(project_id, issue_type);
CREATE INDEX idx_issues_source ON apis.issues(project_id, source_type, target_hash);
CREATE INDEX idx_issues_severity ON apis.issues(project_id, severity);
CREATE INDEX idx_issues_critical ON apis.issues(project_id, critical) WHERE critical = TRUE;
CREATE INDEX idx_issues_open ON apis.issues(project_id, acknowledged_at, archived_at)
    WHERE acknowledged_at IS NULL AND archived_at IS NULL;
CREATE INDEX idx_issues_unresolved ON apis.issues(project_id, resolved_at)
    WHERE resolved_at IS NULL AND archived_at IS NULL;

-- Dedupe: one open issue per source per issue type
CREATE UNIQUE INDEX idx_issues_unique_open
    ON apis.issues(project_id, issue_type, target_hash)
    WHERE resolved_at IS NULL AND archived_at IS NULL;

COMMIT;
