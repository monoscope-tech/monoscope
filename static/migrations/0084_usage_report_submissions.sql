-- Tracks per-chunk usage reports sent to billing providers (Lemon Squeezy, Stripe).
-- Lemon Squeezy caps a single usage-records POST at 1,000,000 events, so large
-- windows are split into chunks. Bookkeeping (daily_usage + usage_last_reported)
-- is committed BEFORE any provider HTTP call; this table records the outcome of
-- each downstream submission so failures are retriable on the next daily tick
-- without re-submitting successful chunks or re-counting internal totals.
CREATE TABLE IF NOT EXISTS projects.usage_report_submissions (
  id           uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id   uuid NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  window_start timestamptz NOT NULL,
  window_end   timestamptz NOT NULL,
  quantity     bigint NOT NULL,
  status       text NOT NULL DEFAULT 'pending',
  last_error   text,
  submitted_at timestamptz,
  created_at   timestamptz NOT NULL DEFAULT now(),
  -- Invariants that are tribal knowledge in Haskell: LS caps a single POST at
  -- 1M events (we use 900k for safety margin); status ↔ companion-column
  -- coupling; monotonic window. Enforced in the DB so any writer (including
  -- ops queries) cannot produce an illegal row.
  CONSTRAINT urs_quantity_bounded CHECK (quantity > 0 AND quantity <= 900000),
  CONSTRAINT urs_status_enum CHECK (status IN ('pending', 'submitted', 'failed')),
  CONSTRAINT urs_submitted_iff_timestamp CHECK ((status = 'submitted') = (submitted_at IS NOT NULL)),
  CONSTRAINT urs_failed_iff_error CHECK ((status = 'failed') = (last_error IS NOT NULL)),
  CONSTRAINT urs_window_monotonic CHECK (window_start <= window_end)
);

-- Partial index: the daily drain path selects non-submitted rows by project;
-- 'submitted' rows accumulate unboundedly and would bloat a full index.
CREATE INDEX IF NOT EXISTS idx_urs_project_pending
  ON projects.usage_report_submissions (project_id, created_at)
  WHERE status <> 'submitted';
