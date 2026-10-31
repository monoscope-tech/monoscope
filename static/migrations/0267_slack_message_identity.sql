BEGIN;

CREATE INDEX slack_events_message_identity ON apis.slack_events
  (team_id, (payload #>> '{event,channel}'), (payload #>> '{event,user}'),
   (payload #>> '{event,ts}'), received_at, id)
  WHERE payload #>> '{event,type}' IN ('message', 'app_mention')
    AND payload #>> '{event,subtype}' IS NULL
    AND payload #>> '{event,bot_id}' IS NULL;

COMMIT;
