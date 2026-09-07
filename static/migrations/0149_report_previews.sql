-- One expiring preview per project. Payload is the typed PreviewStatus JSON;
-- requested_at identifies the lease so an older worker cannot replace a retry.
CREATE TABLE apis.report_previews (
    project_id UUID PRIMARY KEY REFERENCES projects.projects(id) ON DELETE CASCADE,
    requested_at TIMESTAMPTZ NOT NULL,
    expires_at TIMESTAMPTZ NOT NULL,
    payload JSONB NOT NULL
);
