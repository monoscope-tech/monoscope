CREATE TABLE IF NOT EXISTS projects.redacted_fields
  ( id UUID NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
    project_id UUID NOT NULL REFERENCES projects.projects (id) ON DELETE CASCADE,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
    deleted_at TIMESTAMP WITH TIME ZONE,
    path TEXT NOT NULL DEFAULT '',
    configured_via TEXT NOT NULL DEFAULT '',
    description TEXT NOT NULL DEFAULT '',
    endpoint UUID REFERENCES apis.endpoints (id) ON DELETE CASCADE
);
SELECT manage_updated_at('projects.redacted_fields');
CREATE INDEX IF NOT EXISTS idx_projects_redacted_fields_project_id ON projects.redacted_fields(project_id); 
