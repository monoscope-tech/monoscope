-- The LLM pages' per-trace GenAI aggregates for each closed hour (Pages.LLM.windowUnits), so a page
-- reads TimeFusion only for its window's open edges instead of rescanning a day of attributes.
CREATE TABLE IF NOT EXISTS apis.llm_unit_hours (
  project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
  hour TIMESTAMPTZ NOT NULL,
  units JSONB NOT NULL,
  computed_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (project_id, hour)
);
