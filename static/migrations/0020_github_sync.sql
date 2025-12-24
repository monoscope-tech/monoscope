-- After user intsall GitHub app,  table for storing installation info, it should be linked to a project 
CREATE TABLE IF NOT EXISTS apis.github_installations (
    id SERIAL PRIMARY KEY,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    installation_id BIGINT UNIQUE NOT NULL,
    project_id INT REFERENCES projects.projects(id) ON DELETE CASCADE,
    access_token TEXT,
    token_expires_at TIMESTAMP,
);