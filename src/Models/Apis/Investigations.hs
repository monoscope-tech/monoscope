module Models.Apis.Investigations (
  Scope (..),
  Turn (..),
  Step (..),
  Checkpoint (..),
  loadCheckpoint,
  SavedCheckpoint (..),
  CheckpointCursor (..),
  CheckpointConflict (..),
  startCheckpoint,
  commitProgress,
  AgenticChatResult (..),
  ToolCallInfo (..),
  loadAnswer,
  saveAnswer,
  ReplyBatch (..),
  loadReplyBatch,
  saveReplyBatch,
  claimReplyPart,
  confirmReplyPublication,
  rejectReplyPublication,
  captureReplyPublication,
  Event (..),
  Followup (..),
  pendingFollowups,
  isAcceptedFollowup,
  ModelOutcome (..),
  Entry (..),
  History (..),
  ToolResult (..),
  recordEvent,
  recentEvents,
  ProgressTarget (..),
  ProgressMessage (..),
  loadProgress,
  claimProgress,
  confirmProgress,
  rejectProgress,
  captureProgress,
  recentProgressEvents,
) where

import Control.Exception (ErrorCall (..))
import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.LLM ()
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import Langchain.LLM.Core qualified as LLM
import Models.Projects.Projects qualified as Projects
import OpenAI.V1.Chat.Completions qualified as OpenAIV1
import Pkg.DeriveUtils (UUIDId)
import Relude
import System.Types (DB)
import UnliftIO.Exception (throwIO)


data Scope = Scope
  { runId :: UUIDId "investigation_run"
  , projectId :: Projects.ProjectId
  , userId :: Projects.UserId
  , teamId :: Text
  , channelId :: Text
  , threadTs :: Text
  , messageTs :: Text
  }


-- | A returned tool value may contain a domain failure. Returning does not mean
-- the hypothesis was confirmed or that an external operation succeeded.
data ToolResult = ToolResult {formatted :: Text, rawData :: Maybe AE.Value}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data ModelOutcome = ModelAnswered LLM.Message | ModelRequestFailed
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Signed human messages waiting in this investigation's thread.
data Followup = Followup
  { receiptId :: UUIDId "slack_event"
  , slackUserId :: Text
  , messageTs :: Text
  , text :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


pendingFollowups :: DB es => Scope -> Eff es [Followup]
pendingFollowups scope =
  Hasql.interp
    [HI.sql|WITH candidates AS (
    SELECT id, payload->'event'->>'user' AS slack_user_id, payload->'event'->>'ts' AS message_ts,
      payload->'event'->>'text' AS text,
      CASE WHEN payload->'event'->>'ts' ~ '^[0-9]+[.][0-9]+$' THEN (payload->'event'->>'ts')::numeric END AS message_order
    FROM apis.slack_events
    WHERE EXISTS (
      SELECT 1 FROM apis.slack_identities identity
      JOIN projects.project_members member ON member.project_id = #{scope.projectId} AND member.user_id = identity.user_id
      JOIN users.users account ON account.id = identity.user_id
      JOIN projects.projects project ON project.id = member.project_id
      JOIN apis.slack installation ON installation.project_id = member.project_id AND installation.team_id = identity.team_id
      WHERE identity.team_id = #{scope.teamId} AND identity.slack_user_id = payload->'event'->>'user'
        AND member.active AND member.deleted_at IS NULL AND account.active AND account.deleted_at IS NULL
        AND project.active AND project.deleted_at IS NULL
    ) AND processed_at IS NULL AND team_id = #{scope.teamId} AND payload->>'team_id' = #{scope.teamId}
      AND payload->'event'->>'channel' = #{scope.channelId}
      AND payload->'event'->>'thread_ts' = #{scope.threadTs}
      AND payload->'event'->>'type' IN ('message', 'app_mention')
      AND payload->'event'->>'subtype' IS NULL AND payload->'event'->>'bot_id' IS NULL
      AND NOT EXISTS (
        SELECT 1 FROM apis.investigation_journal journal
        WHERE journal.project_id = #{scope.projectId} AND journal.team_id = #{scope.teamId}
          AND journal.channel_id = #{scope.channelId} AND journal.thread_ts = #{scope.threadTs}
          AND journal.event->>'tag' = 'FollowupsAccepted'
          AND journal.event->'contents'->1 @> jsonb_build_array(jsonb_build_object('messageTs', payload->'event'->>'ts'))
      )
      AND CASE WHEN payload->'event'->>'ts' ~ '^[0-9]+[.][0-9]+$'
        THEN (payload->'event'->>'ts')::numeric > #{scope.messageTs}::numeric ELSE FALSE END
    ) SELECT DISTINCT ON (message_order) id, slack_user_id, message_ts, text
      FROM candidates ORDER BY message_order, id LIMIT 20|]


-- | A later Slack event for an already accepted message belongs to the original
-- investigation. Its journal entry commits with the checkpoint and receipt.
isAcceptedFollowup :: DB es => Projects.ProjectId -> Text -> Text -> Text -> Text -> Eff es Bool
isAcceptedFollowup pid team channel thread messageTs = do
  accepted <-
    Hasql.interpOne @(HI.OneColumn Bool)
      [HI.sql|SELECT TRUE FROM apis.investigation_journal
    WHERE project_id = #{pid} AND team_id = #{team} AND channel_id = #{channel} AND thread_ts = #{thread}
      AND event->>'tag' = 'FollowupsAccepted'
      AND event->'contents'->1 @> jsonb_build_array(jsonb_build_object('messageTs', #{messageTs}::text))
    LIMIT 1|]
  pure $ isJust accepted


data Event
  = InvestigationStarted Text Text
  | ModelStarted Int
  | ModelReturned Int ModelOutcome
  | ToolStarted Int LLM.ToolCall
  | ToolReturned Int LLM.ToolCall ToolResult
  | -- | Do not recursively embed the journal inside itself. The preceding
    -- ToolStarted event retains the call ID and arguments.
    InvestigationHistoryRead Int
  | FollowupsAccepted Int (NonEmpty Followup)
  | InvestigationFinished
  | InvestigationFailed
  | InvestigationInterrupted
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue) via Aeson Event


data Entry = Entry
  { runId :: UUIDId "investigation_run"
  , messageTs :: Text
  , observedAt :: UTCTime
  , event :: Event
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON, HI.DecodeRow)


recordEvent :: DB es => Scope -> Event -> Eff es ()
recordEvent scope event = Hasql.interpExecute_ $ recordEventSql scope event


recordEventSql :: Scope -> Event -> HI.Sql
recordEventSql scope event =
  [HI.sql|INSERT INTO apis.investigation_journal
    (run_id, project_id, user_id, team_id, channel_id, thread_ts, message_ts, event)
    VALUES (#{scope.runId}, #{scope.projectId}, #{scope.userId}, #{scope.teamId},
      #{scope.channelId}, #{scope.threadTs}, #{scope.messageTs}, #{Aeson event})|]


data History = History {entries :: [Entry], limitReached :: Bool}
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON)


-- | The most recent fifty events, in chronological order. A full page may be
-- incomplete; model/tool outputs stay evidence rather than conversation roles.
recentEvents :: DB es => Projects.ProjectId -> Text -> Text -> Text -> Eff es History
recentEvents pid team channel thread = readEvents pid team channel thread Nothing


readEvents :: DB es => Projects.ProjectId -> Text -> Text -> Text -> Maybe Text -> Eff es History
readEvents pid team channel thread messageTs = do
  rows <-
    Hasql.interp
      [HI.sql|SELECT run_id, message_ts, observed_at, event FROM apis.investigation_journal
      WHERE project_id = #{pid} AND team_id = #{team} AND channel_id = #{channel} AND thread_ts = #{thread}
        AND (#{messageTs}::text IS NULL OR message_ts = #{messageTs})
      ORDER BY id DESC LIMIT 50|]
  pure $ History (reverse rows) (length rows == 50)


-- | Information about a tool call made during agentic execution
data ToolCallInfo = ToolCallInfo
  { name :: Text
  , args :: Map.Map Text AE.Value
  , resultPreview :: Text
  , rawData :: Maybe AE.Value -- Structured query results for widget data reuse
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ToolCallInfo


-- | Result of an agentic chat with tool call history
data AgenticChatResult = AgenticChatResult
  { response :: Text
  , toolCalls :: [ToolCallInfo]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Turn = Turn
  { projectId :: Projects.ProjectId
  , conversationId :: UUIDId "conversation"
  , userId :: Projects.UserId
  , messageTs :: Text
  }
  deriving stock (Show)


loadAnswer :: DB es => Turn -> Eff es (Maybe AgenticChatResult)
loadAnswer turn =
  fmap (\(HI.OneColumn (Aeson answer)) -> answer)
    <$> Hasql.interpOne
      [HI.sql|SELECT answer FROM apis.slack_turn_answers
    WHERE project_id = #{turn.projectId} AND conversation_id = #{turn.conversationId}
      AND user_id = #{turn.userId} AND message_ts = #{turn.messageTs}|]


-- | Save the complete result and its conversation message together. A competing
-- writer receives the already committed answer rather than replacing it.
saveAnswer :: DB es => Turn -> AgenticChatResult -> Eff es AgenticChatResult
saveAnswer turn answer = do
  saved <-
    Hasql.interpOne
      [HI.sql|WITH saved AS (
      INSERT INTO apis.slack_turn_answers (project_id, conversation_id, user_id, message_ts, answer)
      VALUES (#{turn.projectId}, #{turn.conversationId}, #{turn.userId}, #{turn.messageTs}, #{Aeson answer})
      ON CONFLICT (project_id, conversation_id, user_id, message_ts)
      DO UPDATE SET answer = slack_turn_answers.answer RETURNING answer
    ), message AS (
      INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content, slack_message_ts, created_at)
      SELECT #{turn.projectId}, #{turn.conversationId}, 'assistant', answer->>'response', #{turn.messageTs}, clock_timestamp() FROM saved
      ON CONFLICT (project_id, conversation_id, slack_message_ts, role) WHERE slack_message_ts IS NOT NULL DO NOTHING
    ), cleared AS (
      DELETE FROM apis.slack_turn_checkpoints WHERE project_id = #{turn.projectId}
        AND conversation_id = #{turn.conversationId} AND user_id = #{turn.userId} AND message_ts = #{turn.messageTs}
    ) SELECT answer FROM saved|]
  maybe (throwIO $ ErrorCall "Saving a Slack answer returned no row") (pure . (\(HI.OneColumn (Aeson result)) -> result)) saved


-- | Freeze every rendered part before the first send. A confirmed part is never
-- replayed by a later delivery attempt for this turn.
data ReplyBatch = ReplyBatch {replies :: NonEmpty AE.Value, deliveredCount :: Int}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ReplyBatch


loadReplyBatch :: DB es => Turn -> Eff es (Maybe ReplyBatch)
loadReplyBatch turn =
  fmap (\(HI.OneColumn (Aeson batch)) -> batch)
    <$> Hasql.interpOne
      [HI.sql|SELECT to_jsonb(batch) FROM apis.slack_reply_batches batch
      WHERE project_id = #{turn.projectId} AND conversation_id = #{turn.conversationId}
        AND user_id = #{turn.userId} AND message_ts = #{turn.messageTs}|]


saveReplyBatch :: DB es => Turn -> NonEmpty AE.Value -> Eff es ReplyBatch
saveReplyBatch turn replies = do
  saved <-
    Hasql.interpOne
      [HI.sql|WITH saved AS (
      INSERT INTO apis.slack_reply_batches (project_id, conversation_id, user_id, message_ts, replies)
      VALUES (#{turn.projectId}, #{turn.conversationId}, #{turn.userId}, #{turn.messageTs}, #{Aeson replies})
      ON CONFLICT (project_id, conversation_id, user_id, message_ts)
      DO UPDATE SET replies = slack_reply_batches.replies RETURNING *
    ) SELECT to_jsonb(saved) FROM saved|]
  maybe (throwIO $ ErrorCall "Saving Slack reply batch returned no row") (pure . (\(HI.OneColumn (Aeson batch)) -> batch)) saved


-- | Reserve before HTTP. An existing unacknowledged reservation must not repost.
claimReplyPart :: DB es => Turn -> Text -> Text -> Text -> Int -> Eff es (Maybe (UUIDId "slack_reply"))
claimReplyPart turn team channel thread part =
  fmap HI.getOneColumn
    <$> Hasql.interpOne
      [HI.sql|INSERT INTO apis.slack_reply_publications
      (project_id, conversation_id, user_id, message_ts, part, team_id, channel_id, thread_ts)
    SELECT project_id, conversation_id, user_id, message_ts, #{part}, #{team}, #{channel}, #{thread}
    FROM apis.slack_reply_batches WHERE project_id = #{turn.projectId} AND conversation_id = #{turn.conversationId}
      AND user_id = #{turn.userId} AND message_ts = #{turn.messageTs}
      AND delivered_count = #{part} AND delivered_count < jsonb_array_length(replies)
    ON CONFLICT (project_id, conversation_id, user_id, message_ts, part) DO NOTHING
    RETURNING publication_id|]


-- | Acknowledgement and batch advancement are atomic. Repeated matching evidence
-- is harmless; a conflicting timestamp cannot change the delivery position.
confirmReplyPublication :: DB es => UUIDId "slack_reply" -> Text -> Eff es Bool
confirmReplyPublication publicationId timestamp = do
  confirmed <-
    Hasql.interpOne @(HI.OneColumn Bool)
      [HI.sql|WITH confirmed AS (
      UPDATE apis.slack_reply_publications SET slack_ts = #{timestamp}
      WHERE publication_id = #{publicationId} AND (slack_ts IS NULL OR slack_ts = #{timestamp}) RETURNING *
    ), advanced AS (
      UPDATE apis.slack_reply_batches b SET delivered_count = delivered_count + 1 FROM confirmed p
      WHERE b.project_id = p.project_id AND b.conversation_id = p.conversation_id
        AND b.user_id = p.user_id AND b.message_ts = p.message_ts AND b.delivered_count = p.part
    ) SELECT TRUE FROM confirmed|]
  pure $ isJust confirmed


rejectReplyPublication :: DB es => UUIDId "slack_reply" -> Eff es ()
rejectReplyPublication publicationId =
  Hasql.interpExecute_
    [HI.sql|DELETE FROM apis.slack_reply_publications WHERE publication_id = #{publicationId} AND slack_ts IS NULL|]


-- | Only a signed observation from the configured app in the stored destination
-- can confirm a send whose HTTP acknowledgement was lost.
captureReplyPublication :: DB es => Text -> UUIDId "slack_event" -> Eff es ()
captureReplyPublication appId receiptId = do
  observed <-
    Hasql.interp @[(UUIDId "slack_reply", Text)]
      [HI.sql|SELECT p.publication_id, se.payload #>> '{event,ts}'
    FROM apis.slack_reply_publications p, apis.slack_events se, apis.slack s
    WHERE se.id = #{receiptId} AND #{appId} <> ''
      AND s.project_id = p.project_id AND s.team_id = p.team_id
      AND p.publication_id::text = se.payload #>> '{event,metadata,event_payload,publication_id}'
      AND se.team_id = p.team_id AND se.payload->>'api_app_id' = #{appId}
      AND se.payload #>> '{event,channel}' = p.channel_id
      AND se.payload #>> '{event,thread_ts}' = p.thread_ts
      AND se.payload #>> '{event,type}' = 'message'
      AND COALESCE(se.payload #>> '{event,subtype}', 'bot_message') = 'bot_message'
      AND COALESCE(se.payload #>> '{event,app_id}', se.payload #>> '{event,bot_profile,app_id}') = #{appId}
      AND COALESCE(se.payload #>> '{event,bot_id}', '') <> ''
      AND se.payload #>> '{event,metadata,event_type}' = 'monoscope_investigation_reply'
      AND (se.payload #>> '{event,ts}') ~ '^[0-9]+\.[0-9]+$'|]
  for_ observed $ \(publication, timestamp) -> void $ confirmReplyPublication publication timestamp


-- | Exact model context and progress for one round. The API key is supplied by
-- the worker at execution time and is never part of the stored request.
data Step = Step
  { history :: LLM.ChatHistory
  , request :: OpenAIV1.CreateChatCompletion
  , iteration :: Int
  , timeRange :: (Maybe UTCTime, Maybe UTCTime)
  , toolCalls :: [ToolCallInfo]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Checkpoint
  = ModelPending Step
  | ToolsPending Step LLM.Message [(LLM.ToolCall, ToolResult)] [LLM.ToolCall]
  | AnswerReady AgenticChatResult
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue) via Aeson Checkpoint


data SavedCheckpoint = SavedCheckpoint {revision :: Int64, checkpoint :: Checkpoint}
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON, HI.DecodeRow)


data CheckpointCursor = CheckpointCursor {turn :: Turn, revision :: Int64}
  deriving stock (Show)


data CheckpointConflict = CheckpointConflict
  deriving stock (Generic, Show)
  deriving anyclass (Exception)


loadCheckpoint :: DB es => Turn -> Eff es (Maybe SavedCheckpoint)
loadCheckpoint turn =
  Hasql.interpOne
    [HI.sql|SELECT revision, checkpoint FROM apis.slack_turn_checkpoints
    WHERE project_id = #{turn.projectId} AND conversation_id = #{turn.conversationId}
      AND user_id = #{turn.userId} AND message_ts = #{turn.messageTs}|]


startCheckpoint :: DB es => Turn -> Checkpoint -> Eff es SavedCheckpoint
startCheckpoint turn checkpoint = do
  saved <-
    Hasql.interpOne
      [HI.sql|INSERT INTO apis.slack_turn_checkpoints (project_id, conversation_id, user_id, message_ts, checkpoint)
      VALUES (#{turn.projectId}, #{turn.conversationId}, #{turn.userId}, #{turn.messageTs}, #{Aeson checkpoint})
      ON CONFLICT (project_id, conversation_id, user_id, message_ts)
      DO UPDATE SET checkpoint = slack_turn_checkpoints.checkpoint RETURNING revision, checkpoint|]
  maybe (throwIO $ ErrorCall "Starting a Slack checkpoint returned no row") pure saved


-- | A stale worker cannot replace newer progress. Updating the checkpoint and
-- appending its activity event commit together. Accepted follow-ups also save
-- their conversation messages and receipt completion in this transaction.
commitProgress :: DB es => Maybe CheckpointCursor -> Maybe Scope -> Checkpoint -> Maybe Event -> Eff es (Maybe CheckpointCursor)
commitProgress cursor scope checkpoint event
  | isNothing cursor && (isNothing scope || isNothing event) = pure Nothing
  | otherwise = do
      result <- Hasql.transaction TxS.ReadCommitted TxS.Write do
        updated <- case cursor of
          Nothing -> pure $ Right Nothing
          Just key -> do
            versions <-
              Tx.statement ()
                $ HI.interp @[Int64]
                  True
                  [HI.sql|UPDATE apis.slack_turn_checkpoints SET checkpoint = #{Aeson checkpoint},
                revision = revision + 1, updated_at = clock_timestamp()
                WHERE project_id = #{key.turn.projectId} AND conversation_id = #{key.turn.conversationId}
                  AND user_id = #{key.turn.userId} AND message_ts = #{key.turn.messageTs}
                  AND revision = #{key.revision} RETURNING revision|]
            pure $ case versions of
              [version] -> Right $ Just (key{revision = version} :: CheckpointCursor)
              _ -> Left CheckpointConflict
        for_ (rightToMaybe updated) $ \_ -> for_ scope $ \key -> for_ event $ \activity -> do
          void $ Tx.statement () $ HI.interp @HI.RowsAffected True $ recordEventSql key activity
          case (cursor, activity) of
            (Just turnCursor, FollowupsAccepted _ followups) -> for_ followups $ \followup -> do
              void
                $ Tx.statement ()
                $ HI.interp @HI.RowsAffected
                  True
                  [HI.sql|WITH accepted AS (
                  UPDATE apis.slack_events SET processed_at = clock_timestamp()
                  WHERE team_id = #{key.teamId} AND processed_at IS NULL
                    AND payload->'event'->>'channel' = #{key.channelId}
                    AND payload->'event'->>'thread_ts' = #{key.threadTs}
                    AND payload->'event'->>'ts' = #{followup.messageTs}
                    AND payload->'event'->>'user' = #{followup.slackUserId}
                    AND payload->'event'->>'type' IN ('message', 'app_mention')
                    AND payload->'event'->>'subtype' IS NULL AND payload->'event'->>'bot_id' IS NULL
                  RETURNING id
                ) INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content, slack_message_ts, created_at)
                  SELECT #{turnCursor.turn.projectId}, #{turnCursor.turn.conversationId}, 'user', #{followup.text}, #{followup.messageTs}, clock_timestamp()
                  WHERE EXISTS (SELECT 1 FROM accepted)
                  ON CONFLICT (project_id, conversation_id, slack_message_ts, role) WHERE slack_message_ts IS NOT NULL DO NOTHING|]
            _ -> pure ()
        pure updated
      either throwIO pure result


-- | A separate identity for a Slack turn's progress publication. A row without
-- a timestamp is awaiting acknowledgement or a signed observation; do not repost.
data ProgressTarget = ProgressTarget
  { projectId :: Projects.ProjectId
  , teamId :: Text
  , channelId :: Text
  , threadTs :: Text
  , messageTs :: Text
  , userId :: Projects.UserId
  , slackUserId :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.Snake ProgressTarget


recentProgressEvents :: DB es => ProgressTarget -> Eff es History
recentProgressEvents target = readEvents target.projectId target.teamId target.channelId target.threadTs (Just target.messageTs)


data ProgressMessage = ProgressMessage {publicationId :: UUIDId "slack_progress", timestamp :: Maybe Text}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


loadProgress :: DB es => ProgressTarget -> Eff es (Maybe ProgressMessage)
loadProgress target =
  Hasql.interpOne
    [HI.sql|SELECT publication_id, progress_ts FROM apis.slack_investigation_progress
    WHERE project_id = #{target.projectId} AND team_id = #{target.teamId}
      AND channel_id = #{target.channelId} AND thread_ts = #{target.threadTs} AND message_ts = #{target.messageTs}|]


claimProgress :: DB es => ProgressTarget -> Eff es (Maybe ProgressMessage)
claimProgress target =
  Hasql.interpOne
    [HI.sql|INSERT INTO apis.slack_investigation_progress
    (project_id, team_id, channel_id, thread_ts, message_ts, user_id, slack_user_id)
    VALUES (#{target.projectId}, #{target.teamId}, #{target.channelId}, #{target.threadTs}, #{target.messageTs}, #{target.userId}, #{target.slackUserId})
    ON CONFLICT (project_id, team_id, channel_id, thread_ts, message_ts) DO NOTHING
    RETURNING publication_id, progress_ts|]


confirmProgress :: DB es => UUIDId "slack_progress" -> Text -> Eff es Bool
confirmProgress publicationId timestamp =
  isJust
    <$> Hasql.interpOne @(HI.OneColumn Bool)
      [HI.sql|UPDATE apis.slack_investigation_progress SET progress_ts = #{timestamp}
    WHERE publication_id = #{publicationId} AND (progress_ts IS NULL OR progress_ts = #{timestamp}) RETURNING TRUE|]


-- | Release only a definitely rejected publication. A concurrent signed
-- observation wins over the rejection and must not be deleted.
rejectProgress :: DB es => UUIDId "slack_progress" -> Eff es ()
rejectProgress publicationId =
  Hasql.interpExecute_
    [HI.sql|DELETE FROM apis.slack_investigation_progress
    WHERE publication_id = #{publicationId} AND progress_ts IS NULL|]


-- | Mirror incident-root reconciliation: require a signed stored receipt and
-- verify both the receiving app and message author before accepting its scope.
captureProgress :: DB es => Text -> UUIDId "slack_event" -> Eff es [(ProgressTarget, Text)]
captureProgress appId receiptId =
  map (\(Aeson target, timestamp) -> (target, timestamp))
    <$> Hasql.interp
      [HI.sql|UPDATE apis.slack_investigation_progress p SET progress_ts = se.payload #>> '{event,ts}'
    FROM apis.slack_events se, apis.slack s
    WHERE se.id = #{receiptId} AND #{appId} <> ''
      AND p.user_id IS NOT NULL AND p.slack_user_id IS NOT NULL
      AND s.project_id = p.project_id AND s.team_id = p.team_id
      AND p.publication_id::text = se.payload #>> '{event,metadata,event_payload,publication_id}'
      AND se.team_id = p.team_id AND se.payload->>'api_app_id' = #{appId}
      AND se.payload #>> '{event,channel}' = p.channel_id
      AND se.payload #>> '{event,thread_ts}' = p.thread_ts
      AND se.payload #>> '{event,type}' = 'message'
      AND COALESCE(se.payload #>> '{event,subtype}', 'bot_message') = 'bot_message'
      AND COALESCE(se.payload #>> '{event,app_id}', se.payload #>> '{event,bot_profile,app_id}') = #{appId}
      AND COALESCE(se.payload #>> '{event,bot_id}', '') <> ''
      AND se.payload #>> '{event,metadata,event_type}' = 'monoscope_investigation_progress'
      AND (se.payload #>> '{event,ts}') ~ '^[0-9]+\.[0-9]+$'
      AND (p.progress_ts IS NULL OR p.progress_ts = se.payload #>> '{event,ts}')
    RETURNING to_jsonb(p), p.progress_ts|]
