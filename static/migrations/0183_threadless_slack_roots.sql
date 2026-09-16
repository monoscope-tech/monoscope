-- A Slack destination reached only by incoming webhook cannot be threaded.
--
-- chat.postMessage returns the message timestamp that every reply and edit needs;
-- an incoming webhook returns "ok" and nothing else. Recovering the timestamp
-- afterwards takes either conversations.history or a Slack event carrying the
-- root — both of which need a bot token. An install without one therefore parks
-- its root in `waiting_root` forever, and the claim query's
--
--     AND (d.operation = 'post_root' OR r.message_ts IS NOT NULL)
--
-- means every later reply and edit for that root is never claimed at all. In
-- production that is 104 roots stuck since 2026-09-10 and 49 follow-ups that
-- were queued and silently never sent: an incident posted once and then went
-- quiet, with no recovery notice and no chart refresh.
--
-- `threadless` records that a root was accepted by a transport that can never
-- produce a timestamp, so its follow-ups post as standalone messages instead of
-- waiting on a thread that cannot exist.

ALTER TABLE apis.slack_incident_roots
  ADD COLUMN IF NOT EXISTS threadless BOOLEAN NOT NULL DEFAULT FALSE;

-- A root that has a timestamp is threaded by definition; the flag only ever
-- describes one that does not.
ALTER TABLE apis.slack_incident_roots
  DROP CONSTRAINT IF EXISTS slack_root_threadless_has_no_ts;
ALTER TABLE apis.slack_incident_roots
  ADD CONSTRAINT slack_root_threadless_has_no_ts CHECK (NOT (threadless AND message_ts IS NOT NULL));

-- Backfill: unconfirmed roots belonging to an install with no bot token were
-- always threadless — nothing could ever have confirmed them.
-- Scoped through the episode's project, not by team_id alone: apis.slack is
-- unique on project_id, so one workspace can be connected to two projects with
-- different bot-token status. The delivery worker resolves the install the same
-- way (getProjectSlackData on the delivery's project, then a teamId check), and a
-- one-shot backfill has no second chance to be right.
UPDATE apis.slack_incident_roots r
SET threadless = TRUE
FROM apis.incident_episodes e
WHERE e.id = r.episode_id
  AND r.message_ts IS NULL
  AND EXISTS (
    SELECT 1 FROM apis.slack s
    WHERE s.project_id = e.project_id AND s.team_id = r.team_id AND COALESCE(s.bot_token, '') = ''
  );

-- Settle the roots those deliveries were waiting on. They were sent — the
-- webhook accepted them — so they are delivered, not failed; only the timestamp
-- never arrived. Leaving them in `waiting_root` would keep blocking the
-- follow-ups this migration exists to release.
UPDATE apis.slack_incident_deliveries d
SET state = 'delivered', lease_token = NULL, lease_until = NULL
FROM apis.slack_incident_roots r
WHERE r.id = d.root_id AND r.threadless
  AND d.operation = 'post_root' AND d.state = 'waiting_root';

-- An edit to a message that cannot be edited is inapplicable rather than
-- pending. Recorded as failed with a reason so it is visible and, being settled,
-- stops blocking the replies queued behind it.
UPDATE apis.slack_incident_deliveries d
SET state = 'failed', last_error = 'threadless_root_cannot_be_edited', lease_token = NULL, lease_until = NULL
FROM apis.slack_incident_roots r
WHERE r.id = d.root_id AND r.threadless
  AND d.operation = 'update_root' AND d.state = 'pending';
