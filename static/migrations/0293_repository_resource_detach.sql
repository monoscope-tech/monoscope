-- A detached resource keeps its row as a tombstone, so the sync scan neither imports its
-- repository file nor registers that file again as a new resource.
ALTER TABLE projects.repository_resources ADD COLUMN IF NOT EXISTS detached_at TIMESTAMPTZ;
