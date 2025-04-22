CREATE TABLE IF NOT EXISTS apis.facet_summaries (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    project_id TEXT NOT NULL,
    table_name TEXT NOT NULL,
    timestamp TIMESTAMPTZ NOT NULL,
    facet_json JSONB NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp
);

-- Create an index for efficient querying of facets
CREATE INDEX IF NOT EXISTS facet_summaries_project_table_timestamp_idx ON apis.facet_summaries (project_id, table_name, timestamp);

-- Add a compound index for faster lookups by project and table
CREATE INDEX IF NOT EXISTS facet_summaries_project_table_idx ON apis.facet_summaries (project_id, table_name);

-- Add truncation policy for facets that are over 90 days old
CREATE OR REPLACE FUNCTION remove_old_facet_summaries() RETURNS TRIGGER AS $$
BEGIN
    DELETE FROM apis.facet_summaries WHERE timestamp < NOW() - INTERVAL '30 days';
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trigger_remove_old_facet_summaries ON apis.facet_summaries;
CREATE TRIGGER trigger_remove_old_facet_summaries
AFTER INSERT ON apis.facet_summaries
EXECUTE PROCEDURE remove_old_facet_summaries();
