-- Promote hosts to a first-class entity. Until now, the API Catalog page
-- derived hosts via `SELECT DISTINCT host FROM apis.endpoints` on every render.
-- A dedicated table lets us:
--   * archive/unarchive a host (incoming or outgoing) without touching endpoints
--   * attach per-host metadata over time (description, owner, criticality, …)
--   * avoid the recurring DISTINCT over a multi-million-row endpoints table
CREATE TABLE IF NOT EXISTS apis.hosts (
    id           UUID         PRIMARY KEY DEFAULT gen_random_uuid(),
    project_id   UUID         NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
    host         TEXT         NOT NULL,
    outgoing     BOOLEAN      NOT NULL,
    description  TEXT         NOT NULL DEFAULT '',
    created_at   TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
    updated_at   TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
    archived_at  TIMESTAMPTZ  NULL,
    archived_by  UUID         NULL,
    UNIQUE (project_id, host, outgoing)
);

CREATE INDEX IF NOT EXISTS idx_apis_hosts_project_active
  ON apis.hosts (project_id, outgoing) WHERE archived_at IS NULL;

CREATE INDEX IF NOT EXISTS idx_apis_hosts_project_archived
  ON apis.hosts (project_id, outgoing) WHERE archived_at IS NOT NULL;

-- Backfill from existing endpoints so the catalog continues working immediately.
INSERT INTO apis.hosts (project_id, host, outgoing)
SELECT DISTINCT project_id, host, outgoing
  FROM apis.endpoints
 WHERE host <> ''
ON CONFLICT (project_id, host, outgoing) DO NOTHING;
