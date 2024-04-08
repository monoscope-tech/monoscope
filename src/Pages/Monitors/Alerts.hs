module Pages.Monitors.Alerts (alertListGetH, alertUpsertPostH, AlertUpsertForm (..)) where

import Data.CaseInsensitive qualified as CI
import Data.Time.Clock (UTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Lucid
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Relude
import System.Types
import Web.FormUrlEncoded (FromForm)


data AlertUpsertForm = AlertUpsertForm
  { alertId :: Maybe Text
  , alertThreshold :: Int
  , warningThreshold :: Maybe Text
  , recipientEmails :: [Text]
  , recipientSlacks :: [Text]
  , recipientEmailAll :: Maybe Bool
  , checkIntervalMins :: Int
  , direction :: Text
  , title :: Text
  , severity :: Text
  , subject :: Text
  , message :: Text
  , query :: Text
  , since :: Text
  , from :: Text
  , to :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


convertToQueryMonitor :: Projects.ProjectId -> UTCTime -> Monitors.QueryMonitorId -> AlertUpsertForm -> Monitors.QueryMonitor
convertToQueryMonitor projectId now queryMonitorId alertForm =
  let warningThresholdInt = readMaybe . toString =<< alertForm.warningThreshold
      alertConfig =
        Monitors.MonitorAlertConfig
          { severity = alertForm.severity
          , title = alertForm.title
          , subject = alertForm.subject
          , message = alertForm.message
          , emails = V.fromList $ map CI.mk $ alertForm.recipientEmails
          , emailAll = fromMaybe False alertForm.recipientEmailAll
          , slackChannels = V.fromList $ alertForm.recipientSlacks
          }
   in Monitors.QueryMonitor
        { id = queryMonitorId
        , createdAt = now
        , updatedAt = now
        , projectId = projectId
        , checkIntervalMins = alertForm.checkIntervalMins
        , alertThreshold = alertForm.alertThreshold
        , warningThreshold = warningThresholdInt
        , logQuery = alertForm.query
        , logQueryAsSql = "" -- Placeholder, replace with actual log query as SQL
        , lastEvaluated = now
        , warningLastTriggered = Nothing
        , alertLastTriggered = Nothing
        , triggerLessThan = alertForm.direction == "below"
        , thresholdSustainedForMins = 0 -- Placeholder, set according to your logic
        , alertConfig
        }


alertUpsertPostH :: Projects.ProjectId -> AlertUpsertForm -> ATAuthCtx (Html ())
alertUpsertPostH pid form = do
  traceShowM form
  queryMonitorId <- liftIO $ Monitors.QueryMonitorId <$> UUID.nextRandom
  now <- Time.currentTime
  let queryMonitor = convertToQueryMonitor pid now queryMonitorId form

  _ <- dbtToEff $ Monitors.queryMonitorUpsert queryMonitor
  -- TODO: add toast
  pure $ ""


alertListGetH :: Projects.ProjectId -> ATAuthCtx (Html ())
alertListGetH pid = do
  monitors <- dbtToEff $ Monitors.queryMonitorsAll pid
  pure $ queryMonitors_ monitors


queryMonitors_ :: V.Vector Monitors.QueryMonitor -> Html ()
queryMonitors_ monitors = do
  table_ [class_ "table"] do
    thead_ $ tr_ do
      th_ "Title"
      th_ ""
    tbody_ do
      V.forM_ monitors \monitor -> tr_ do
        th_ [] $ toHtml monitor.alertConfig.title
        th_ [] do
          a_ [class_ "btn btn-ghost btn-xs"] "preview"
          a_ [class_ "btn btn-ghost btn-xs"] "details"
