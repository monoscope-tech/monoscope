-- Monorepo service folder a Setup agent run instruments ('' = repository root).
ALTER TABLE projects.onboarding_instrument_runs ADD COLUMN IF NOT EXISTS root_path TEXT NOT NULL DEFAULT '';
