-- Fix race condition: replace MAX(seq_num) trigger with atomic counter table
CREATE TABLE apis.issue_seq_counters (
  project_id UUID PRIMARY KEY REFERENCES projects.projects(id) ON DELETE CASCADE,
  last_seq INT NOT NULL DEFAULT 0
);

CREATE INDEX idx_issues_project_seq_num ON apis.issues (project_id, seq_num);

-- Seed counter table from existing data
INSERT INTO apis.issue_seq_counters (project_id, last_seq)
SELECT project_id, COALESCE(MAX(seq_num), 0) FROM apis.issues GROUP BY project_id;

-- Replace trigger with atomic counter version
CREATE OR REPLACE FUNCTION apis.issues_set_seq_num() RETURNS trigger AS $$
BEGIN
  INSERT INTO apis.issue_seq_counters (project_id, last_seq) VALUES (NEW.project_id, 1)
    ON CONFLICT (project_id) DO UPDATE SET last_seq = apis.issue_seq_counters.last_seq + 1
    RETURNING last_seq INTO NEW.seq_num;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
