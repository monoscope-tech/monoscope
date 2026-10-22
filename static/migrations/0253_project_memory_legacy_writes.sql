BEGIN;
CREATE FUNCTION projects.ensure_memory_page_identity() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  INSERT INTO projects.memory_pages (project_id, page_id, path)
  VALUES (NEW.project_id, NEW.page_id, NEW.page_id::text)
  ON CONFLICT (project_id, page_id) DO NOTHING;
  RETURN NEW;
END;
$$;
CREATE TRIGGER memory_revisions_page_identity
  BEFORE INSERT ON projects.memory_revisions FOR EACH ROW
  EXECUTE FUNCTION projects.ensure_memory_page_identity();
COMMIT;
