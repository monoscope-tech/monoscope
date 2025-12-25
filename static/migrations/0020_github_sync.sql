BEGIN;

CREATE TABLE IF NOT EXISTS apis.github_installations (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    installation_id BIGINT NOT NULL UNIQUE,
    project_id UUID NOT NULL UNIQUE
        REFERENCES projects.projects(id) ON DELETE CASCADE,
    access_token TEXT,
    token_expires_at TIMESTAMPTZ
);

SELECT manage_updated_at('apis.github_installations');
CREATE INDEX idx_github_installations_project_id ON apis.github_installations(project_id);

COMMIT;
