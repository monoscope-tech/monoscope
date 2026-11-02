BEGIN;

-- Classify stored Slack evidence once. Current identity links cannot establish its author.
CREATE TEMP TABLE monoscope_legacy_slack_audiences ON COMMIT DROP AS
WITH RECURSIVE receipts AS (
  SELECT run.project_id, run.conversation_id, event.team_id, event.payload #>> '{event,channel}' AS channel_id
  FROM apis.ai_routine_runs run
  LEFT JOIN apis.slack_events event ON event.id::text = run.source #>> '{contents,contents}'
  WHERE run.source->>'tag' = 'TriggeredRun' AND run.source #>> '{contents,tag}' = 'SlackMessage'
), roots AS (
  SELECT * FROM receipts
  UNION ALL
  SELECT conversation.project_id, conversation.conversation_id,
    conversation.context->>'team_id', conversation.context->>'channel_id'
  FROM apis.ai_conversations conversation
  WHERE conversation.conversation_type = 'slack_thread'
    AND NOT EXISTS (SELECT 1 FROM receipts WHERE receipts.project_id = conversation.project_id
      AND receipts.conversation_id = conversation.conversation_id)
), authors AS (
  SELECT project_id, conversation_id, user_id FROM apis.slack_turn_checkpoints WHERE message_ts IS NOT NULL
  UNION ALL SELECT project_id, conversation_id, user_id FROM apis.slack_turn_answers
  UNION ALL SELECT project_id, conversation_id, requested_by FROM apis.ai_routine_runs
    WHERE source->>'tag' = 'TriggeredRun' AND source #>> '{contents,tag}' IN ('SlackMessage', 'SlackCommand')
), base AS (
  SELECT roots.project_id, roots.conversation_id,
    CASE WHEN apis.valid_evidence_audience(channel.audience) THEN channel.audience
      WHEN roots.channel_id ~ '^D[A-Z0-9]+$' AND length(btrim(roots.team_id)) > 0
        AND owner.count = 1 AND NOT owner.unknown
      THEN jsonb_build_object('tag', 'PersonalAudience', 'contents', owner.id)
      ELSE '{"tag":"NoAudience"}'::jsonb END AS audience
  FROM roots
  CROSS JOIN LATERAL (SELECT jsonb_build_object('tag', 'SlackChannelAudience', 'contents',
    jsonb_build_object('teamId', roots.team_id, 'channelId', roots.channel_id)) AS audience) channel
  CROSS JOIN LATERAL (SELECT count(DISTINCT user_id) AS count, min(user_id::text) AS id,
    COALESCE(bool_or(user_id IS NULL), true) AS unknown FROM authors
    WHERE authors.project_id = roots.project_id AND authors.conversation_id = roots.conversation_id) owner
  WHERE apis.conversation_evidence_audience(roots.project_id, roots.conversation_id) = '{"tag":"ProjectAudience"}'::jsonb
  UNION ALL SELECT project_id, conversation_id, evidence_audience FROM apis.ai_conversations
    WHERE evidence_audience <> '{"tag":"ProjectAudience"}'::jsonb
  UNION ALL SELECT project_id, conversation_id, evidence_audience FROM apis.ai_routine_runs
    WHERE evidence_audience <> '{"tag":"ProjectAudience"}'::jsonb
), inherited(project_id, conversation_id, audience) AS (
  SELECT * FROM base
  UNION
  SELECT run.project_id, run.conversation_id, inherited.audience
  FROM inherited JOIN apis.ai_routines routine ON routine.project_id = inherited.project_id
    AND routine.conversation_id = inherited.conversation_id
  JOIN apis.ai_routine_runs run ON run.routine_id = routine.id AND run.project_id = routine.project_id
)
SELECT project_id, conversation_id, apis.meet_evidence_audiences(audience) AS audience
FROM inherited GROUP BY project_id, conversation_id;

-- Preserve the run-before-conversation lock order and the existing monotonic scope rules.
UPDATE apis.ai_routine_runs run SET evidence_audience = apis.meet_evidence_audience(run.evidence_audience, legacy.audience)
FROM monoscope_legacy_slack_audiences legacy
WHERE run.project_id = legacy.project_id AND run.conversation_id = legacy.conversation_id;

UPDATE apis.ai_conversations conversation SET evidence_audience = apis.meet_evidence_audience(conversation.evidence_audience, legacy.audience)
FROM monoscope_legacy_slack_audiences legacy
WHERE conversation.project_id = legacy.project_id AND conversation.conversation_id = legacy.conversation_id;

COMMIT;
