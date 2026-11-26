-- Allow onboarding as a GitHub App installation return destination.
ALTER TABLE projects.github_installation_attempts
  DROP CONSTRAINT IF EXISTS github_installation_attempts_destination_check;

ALTER TABLE projects.github_installation_attempts
  ADD CONSTRAINT github_installation_attempts_destination_check
  CHECK (destination IN ('code', 'sync', 'onboarding'));

-- Durable state for autonomous onboarding instrumentation runs (pi-durable).
CREATE TABLE IF NOT EXISTS projects.onboarding_instrument_runs (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  repository_id UUID REFERENCES projects.repositories(id) ON DELETE SET NULL,
  repo_full_name TEXT NOT NULL,
  branch TEXT NOT NULL DEFAULT 'main',
  target_kind TEXT NOT NULL DEFAULT 'auto',
  status TEXT NOT NULL DEFAULT 'queued',
  detected_stack TEXT NOT NULL DEFAULT '',
  phases JSONB NOT NULL DEFAULT '[]'::jsonb,
  diff_preview TEXT NOT NULL DEFAULT '',
  suggested_urls JSONB NOT NULL DEFAULT '[]'::jsonb,
  pr_url TEXT,
  pr_number INTEGER,
  verified_event_at TIMESTAMPTZ,
  error_summary TEXT,
  error_log TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS onboarding_instrument_runs_project_idx
  ON projects.onboarding_instrument_runs (project_id, created_at DESC);
