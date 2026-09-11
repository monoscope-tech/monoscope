-- A project switched off while we are still billing it.
--
-- "Engine/API Prod" (LemonSqueezy subscription 445873, paid without a gap, last
-- invoice 2026-09-08) sat at active = false with deleted_at IS NULL from
-- 2026-07-17. No code path produces that pair — the only statement that sets
-- projects.active = false (Projects.deleteProject) sets deleted_at in the same
-- UPDATE — so nothing in the app could show it, explain it, or undo it. It cost
-- nothing until 2026-08-25, when c71ec614f added `p.active = TRUE` to the
-- API-key lookup; from that day every ingest request for the project 401'd and
-- their telemetry stopped, silently, for 17 days.
--
-- deleted_at is the soft-delete marker. `active` on its own carries no meaning
-- the application acts on, and the 749 rows a manual cleanup switched off on
-- 2024-10-03 are deliberate and left alone. The invariant worth enforcing is the
-- narrow one the incident violated: we never switch off a project we are still
-- charging.
UPDATE projects.projects
   SET active = true
 WHERE active = false AND deleted_at IS NULL AND coalesce(sub_id, '') <> '';

ALTER TABLE projects.projects
  ADD CONSTRAINT projects_subscribed_not_deactivated
  CHECK (active OR deleted_at IS NOT NULL OR coalesce(sub_id, '') = '');
