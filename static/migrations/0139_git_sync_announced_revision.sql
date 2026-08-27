-- The revision a push webhook told us about, and when it told us.
--
-- gitSyncFromRepo decides "nothing changed" by comparing last_revision against the head the
-- git host reports at fetch time. When the host answers with a view older than the push it
-- just announced, that comparison says "unchanged" and the job completes having done nothing.
-- The push is then never applied — not late, never — until some later push happens to be
-- fetched freshly, which sweeps up the backlog. Measured on the demo project: 3 of 5 pushes
-- applied, and the two misses were still unapplied seven minutes later.
--
-- Recording what the webhook announced gives the job a second opinion. It can tell "the host
-- and I agree there is nothing new" apart from "the host is behind what it told me", and
-- retry the second case instead of silently accepting it.
--
-- announced_at bounds that retry without a counter column: a force-push can make an announced
-- sha permanently unfetchable, and retrying a dead sha forever is a worse bug than the one
-- being fixed.
ALTER TABLE projects.git_sync
  ADD COLUMN IF NOT EXISTS announced_revision text,
  ADD COLUMN IF NOT EXISTS announced_at timestamptz;
