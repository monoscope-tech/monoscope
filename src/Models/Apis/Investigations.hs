module Models.Apis.Investigations (
  Scope (..),
  Event (..),
  ModelOutcome (..),
  Entry (..),
  History (..),
  ToolResult (..),
  recordEvent,
  recentEvents,
) where

import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.LLM ()
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Langchain.LLM.Core qualified as LLM
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId)
import Relude
import System.Types (DB)


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
