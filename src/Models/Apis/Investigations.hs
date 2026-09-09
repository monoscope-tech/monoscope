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
  Event (..),
  ModelOutcome (..),
  Entry (..),
  History (..),
  ToolResult (..),
  recordEvent,
  recentEvents,
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


data Event
  = InvestigationStarted Text Text
  | ModelStarted Int
  | ModelReturned Int ModelOutcome
  | ToolStarted Int LLM.ToolCall
  | ToolReturned Int LLM.ToolCall ToolResult
  | -- | Do not recursively embed the journal inside itself. The preceding
    -- ToolStarted event retains the call ID and arguments.
    InvestigationHistoryRead Int
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
recentEvents pid team channel thread = do
  rows <-
    Hasql.interp
      [HI.sql|SELECT run_id, message_ts, observed_at, event FROM apis.investigation_journal
      WHERE project_id = #{pid} AND team_id = #{team} AND channel_id = #{channel} AND thread_ts = #{thread}
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
-- appending its activity event either both commit or both roll back.
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
        for_ (rightToMaybe updated) $ \_ -> for_ scope $ \key -> for_ event $ \activity ->
          void $ Tx.statement () $ HI.interp @HI.RowsAffected True $ recordEventSql key activity
        pure updated
      either throwIO pure result
