ALTER TABLE projects.git_sync
    ADD COLUMN export_branch TEXT NOT NULL DEFAULT 'monoscope-dashboards',
    ADD COLUMN pull_request_number BIGINT,
    ADD COLUMN export_paused BOOLEAN NOT NULL DEFAULT false,
    ADD COLUMN last_export_hash TEXT;

CREATE TABLE projects.repository_resources (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
    sync_id UUID NOT NULL REFERENCES projects.git_sync(id) ON DELETE CASCADE,
    kind TEXT NOT NULL CHECK (kind IN ('dashboard', 'query', 'uptime', 'cron')),
    local_id UUID NOT NULL,
    file_path TEXT NOT NULL CHECK (file_path <> '' AND file_path !~ '(^/|(^|/)\.\.(/|$))'),
    merged_definition JSONB,
    remote_definition JSONB,
    conflicted BOOLEAN NOT NULL DEFAULT false,
    UNIQUE (project_id, kind, local_id),
    UNIQUE (sync_id, file_path)
);

CREATE TABLE projects.repository_analysis (
    repository_id UUID PRIMARY KEY REFERENCES projects.repositories(id) ON DELETE CASCADE,
    analyzed_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    suggestions JSONB NOT NULL DEFAULT '[]',
    error TEXT
);

-- Retain ownership of existing dashboards and paths. Their first read establishes the baseline.
INSERT INTO projects.repository_resources (project_id, sync_id, kind, local_id, file_path)
SELECT d.project_id, d.git_sync_id, 'dashboard', d.id,
       CASE WHEN s.path_prefix = '' THEN 'dashboards/' ELSE s.path_prefix || '/dashboards/' END || d.file_path
FROM projects.dashboards d JOIN projects.git_sync s ON s.id = d.git_sync_id
WHERE d.file_path IS NOT NULL;
