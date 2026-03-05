ALTER TABLE apis.endpoints
  ADD COLUMN IF NOT EXISTS canonical_hash TEXT,
  ADD COLUMN IF NOT EXISTS canonical_path TEXT,
  ADD COLUMN IF NOT EXISTS embedding FLOAT4[],
  ADD COLUMN IF NOT EXISTS embedding_at TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS merge_override BOOLEAN NOT NULL DEFAULT FALSE;

CREATE INDEX IF NOT EXISTS idx_endpoints_canonical_hash
  ON apis.endpoints(canonical_hash) WHERE canonical_hash IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_endpoints_unmerged
  ON apis.endpoints(project_id) WHERE canonical_hash IS NULL AND merge_override = FALSE;

CREATE INDEX IF NOT EXISTS idx_endpoints_canonical_path
  ON apis.endpoints(project_id) WHERE canonical_path IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_endpoints_embedding
  ON apis.endpoints(project_id) WHERE embedding IS NULL AND merge_override = FALSE;
