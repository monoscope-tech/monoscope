CREATE TABLE IF NOT EXISTS apis.facet_summaries (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    project_id TEXT NOT NULL,
    table_name TEXT NOT NULL,
    facet_json JSONB NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
    updated_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
    UNIQUE (project_id, table_name)
);

-- Add a compound index for faster lookups by project and table
CREATE INDEX IF NOT EXISTS facet_summaries_project_table_idx ON apis.facet_summaries (project_id, table_name);

-- Apply the updated_at trigger to our table
SELECT manage_updated_at('apis.facet_summaries');
