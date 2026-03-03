ALTER TABLE apis.error_patterns
    ADD COLUMN IF NOT EXISTS canonical_id    UUID REFERENCES apis.error_patterns(id) ON DELETE SET NULL,
    ADD COLUMN IF NOT EXISTS embedding       FLOAT4[],
    ADD COLUMN IF NOT EXISTS embedding_at    TIMESTAMPTZ,
    ADD COLUMN IF NOT EXISTS merge_override  BOOLEAN NOT NULL DEFAULT FALSE;

ALTER TABLE apis.log_patterns
    ADD COLUMN IF NOT EXISTS canonical_id    BIGINT REFERENCES apis.log_patterns(id) ON DELETE SET NULL,
    ADD COLUMN IF NOT EXISTS embedding       FLOAT4[],
    ADD COLUMN IF NOT EXISTS embedding_at    TIMESTAMPTZ,
    ADD COLUMN IF NOT EXISTS merge_override  BOOLEAN NOT NULL DEFAULT FALSE;

CREATE INDEX IF NOT EXISTS idx_ep_no_embedding ON apis.error_patterns(project_id) WHERE embedding IS NULL;
CREATE INDEX IF NOT EXISTS idx_lp_no_embedding ON apis.log_patterns(project_id) WHERE embedding IS NULL;
CREATE INDEX IF NOT EXISTS idx_ep_canonical ON apis.error_patterns(canonical_id) WHERE canonical_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_lp_canonical ON apis.log_patterns(canonical_id) WHERE canonical_id IS NOT NULL;
