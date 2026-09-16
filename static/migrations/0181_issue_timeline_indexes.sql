-- The issue detail page now reads its timeline from the episode ledger as well as
-- apis.issue_activity_log, and neither side of that join was indexed for it.
--
-- apis.incident_episodes is indexed only on (project_id, source_kind, source_id):
-- nothing leads with issue_id, which is what the timeline filters on. And
-- apis.incident_events has no index on episode_id at all — its UNIQUE (id,
-- episode_id) leads with id, and the (episode_id, project_id) foreign key does
-- not create one, because Postgres never indexes the referencing side.
--
-- Without these, every issue page load sequentially scans both tables. The
-- activity-log half of the same query has had idx_issue_activity_log_issue since
-- migration 0042; this gives the episode half its equivalent.

-- Partial: an episode with no issue is legacy (the write path now refuses to
-- open one), and the timeline only ever looks up issue_id IS NOT NULL.
CREATE INDEX IF NOT EXISTS incident_episodes_issue
  ON apis.incident_episodes (project_id, issue_id)
  WHERE issue_id IS NOT NULL;

-- observed_at trails the key so the timeline's ORDER BY is served by the index
-- rather than a sort of every event in the episode.
CREATE INDEX IF NOT EXISTS incident_events_episode
  ON apis.incident_events (episode_id, observed_at DESC);
