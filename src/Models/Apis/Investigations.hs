module Models.Apis.Investigations (
  Scope (..),
  Turn (..),
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
import Langchain.LLM.Core qualified as LLM
import Models.Projects.Projects qualified as Projects
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
recordEvent scope event =
  Hasql.interpExecute_
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
    ) SELECT answer FROM saved|]
  maybe (throwIO $ ErrorCall "Saving a Slack answer returned no row") (pure . (\(HI.OneColumn (Aeson result)) -> result)) saved
