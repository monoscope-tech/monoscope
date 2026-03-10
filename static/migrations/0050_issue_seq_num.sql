-- Project-scoped sequential issue numbers for compact display (#42 instead of UUID)
ALTER TABLE apis.issues ADD COLUMN seq_num INT NOT NULL DEFAULT 0;

-- Backfill existing issues: assign sequential numbers per project ordered by created_at
WITH numbered AS (
  SELECT id, ROW_NUMBER() OVER (PARTITION BY project_id ORDER BY created_at) AS rn
  FROM apis.issues
)
UPDATE apis.issues SET seq_num = numbered.rn FROM numbered WHERE apis.issues.id = numbered.id;

-- Trigger to auto-assign seq_num on INSERT
CREATE OR REPLACE FUNCTION apis.issues_set_seq_num() RETURNS trigger AS $$
BEGIN
  NEW.seq_num := COALESCE((SELECT MAX(seq_num) FROM apis.issues WHERE project_id = NEW.project_id), 0) + 1;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER issues_seq_num_trigger
  BEFORE INSERT ON apis.issues
  FOR EACH ROW
  EXECUTE FUNCTION apis.issues_set_seq_num();
