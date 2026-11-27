-- A repository keeps its best proof setup: how far it got (0 failed, 1 ran, 2 the app answers,
-- 3 per-role logins), its time on the prepared build, and its last error for the next attempt to repair.
-- Existing recipes passed a head recording, so their setup ran.
ALTER TABLE projects.pr_review_proof_recipes
  ADD COLUMN stage smallint NOT NULL DEFAULT 1,
  ADD COLUMN setup_seconds integer,
  ADD COLUMN failure text;
