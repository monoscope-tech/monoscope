module Pages.Monitors.Alerts (
  alertSingleGetH,
  convertToQueryMonitor,
  alertSingleToggleActiveH,
  alertListGetH,
  alertUpsertPostH,
  alertOverviewGetH,
  AlertUpsertForm (..),
  Alert (..),
)
where

import Data.CaseInsensitive qualified as CI
import Data.Either.Extra (fromRight')
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Pkg.Parser (defSqlQueryCfg, finalAlertQuery, fixedUTCTime, parseQueryToComponents, presetRollup)
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
  , direction :: Text
  , title :: Text
  , severity :: Text
  , subject :: Text
  , message :: Text
  , query :: Text
  , since :: Text
  , from :: Text
  , to :: Text
  , frequency :: Maybe Text -- Added for monitor schedule
  , timeWindow :: Maybe Text -- Added for monitor schedule
  , conditionType :: Maybe Text -- Added for condition type
  , source :: Maybe Text -- Added for source support
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


convertToQueryMonitor :: Projects.ProjectId -> UTCTime -> Monitors.QueryMonitorId -> AlertUpsertForm -> Monitors.QueryMonitor
convertToQueryMonitor projectId now queryMonitorId alertForm =
  -- FIXME: handle errors correctly, not crashing
  let sqlQueryCfg = (defSqlQueryCfg projectId fixedUTCTime Nothing Nothing){presetRollup = Just "5m"}
      (_, qc) = fromRight' $ parseQueryToComponents sqlQueryCfg alertForm.query
      warningThresholdInt = readMaybe . toString =<< alertForm.warningThreshold

      -- Parse frequency from "Xm" format (e.g., "5m" -> 5)
      -- Ensure minimum interval is 1 minute
      checkInterval = case alertForm.frequency of
        Just freq -> max 1 $ fromMaybe 5 $ readMaybe $ toString $ T.dropEnd 1 freq -- Remove 'm' suffix, enforce min 1
        Nothing -> 5 -- Default to 5 minutes

      -- Determine if this is a threshold-based alert
      isThresholdAlert = alertForm.conditionType == Just "threshold_exceeded"

      alertConfig =
        Monitors.MonitorAlertConfig
          { severity = alertForm.severity
          , title = alertForm.title
          , subject = alertForm.subject
          , message = alertForm.message
          , emails = V.fromList $ map CI.mk alertForm.recipientEmails
          , emailAll = fromMaybe False alertForm.recipientEmailAll
          , slackChannels = V.fromList alertForm.recipientSlacks
          }
   in Monitors.QueryMonitor
        { id = queryMonitorId
        , createdAt = now
        , updatedAt = now
        , projectId = projectId
        , checkIntervalMins = checkInterval
        , alertThreshold = if isThresholdAlert then alertForm.alertThreshold else 0
        , warningThreshold = if isThresholdAlert then warningThresholdInt else Nothing
        , logQuery = alertForm.query
        , logQueryAsSql = fromMaybe "" qc.finalAlertQuery
        , lastEvaluated = now
        , warningLastTriggered = Nothing
        , alertLastTriggered = Nothing
        , triggerLessThan = alertForm.direction == "below"
        , thresholdSustainedForMins = 0 -- Placeholder, set according to your logic
        , alertConfig
        , deletedAt = Nothing
        , deactivatedAt = Nothing
        }


alertUpsertPostH :: Projects.ProjectId -> AlertUpsertForm -> ATAuthCtx (RespHeaders Alert)
alertUpsertPostH pid form = do
  let alertId = form.alertId >>= UUID.fromText
  queryMonitorId <- liftIO $ case alertId of
    Just alertId' -> pure (Monitors.QueryMonitorId alertId')
    Nothing -> Monitors.QueryMonitorId <$> UUID.nextRandom
  now <- Time.currentTime
  let queryMonitor = convertToQueryMonitor pid now queryMonitorId form

  _ <- dbtToEff $ Monitors.queryMonitorUpsert queryMonitor
  addSuccessToast "Monitor was updated successfully" Nothing
  addRespHeaders $ AlertNoContent ""


alertListGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders Alert)
alertListGetH pid = do
  monitors <- dbtToEff $ Monitors.queryMonitorsAll pid
  addRespHeaders $ AlertListGet monitors


alertSingleToggleActiveH :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders Alert)
alertSingleToggleActiveH pid monitorId = do
  _ <- dbtToEff $ Monitors.monitorToggleActiveById monitorId

  monitors <- dbtToEff $ Monitors.queryMonitorsAll pid
  addRespHeaders $ AlertListGet monitors


alertSingleGetH :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders Alert)
alertSingleGetH pid monitorId = do
  monitor <- dbtToEff $ Monitors.queryMonitorById monitorId
  addRespHeaders $ AlertSingle pid monitor


data Alert
  = AlertListGet (V.Vector Monitors.QueryMonitor)
  | AlertSingle Projects.ProjectId (Maybe Monitors.QueryMonitor)
  | AlertNoContent Text
  | AlertRedirect Text


instance ToHtml Alert where
  toHtml (AlertListGet monitors) = toHtml $ queryMonitors_ monitors
  toHtml (AlertSingle pid monitor) = toHtml $ alertSingleComp pid monitor
  toHtml (AlertNoContent msg) = toHtml msg
  toHtml (AlertRedirect url) = do
    script_ $ "window.location.href = '" <> url <> "';"
  toHtmlRaw = toHtml


alertSingleComp :: Projects.ProjectId -> Maybe Monitors.QueryMonitor -> Html ()
alertSingleComp pid monitor = do
  div_ [] do
    a_ [class_ "border-y p-3 block cursor-pointer", hxGet_ $ "/p/" <> pid.toText <> "/alerts", hxTarget_ "#alertsListContainer"] "‹ Back to alerts list"
    div_ [class_ "p-3"] $ span_ "Alert details view not implemented"


queryMonitors_ :: V.Vector Monitors.QueryMonitor -> Html ()
queryMonitors_ monitors = do
  table_ [class_ "table text-lg min-w-[65ch]"] do
    thead_ $ tr_ do
      th_ "Title"
      th_ ""
    tbody_ do
      V.forM_ monitors \monitor -> tr_ do
        let editAction =
              [__|
on click
if ('URLSearchParams' in window)
    make a URLSearchParams from window.location.search called :searchParams
    call :searchParams.set("foo", "bar")
    set :newRelativePathQuery to window.location.pathname + '?' + :searchParams.toString()
    call history.pushState(null, '', newRelativePathQuery)
end
            |]
        let editURI = "/p/" <> monitor.projectId.toText <> "/alerts/" <> monitor.id.toText
        td_
          [ class_ $ if isJust monitor.deactivatedAt then "line-through" else ""
          , hxTarget_ "#alertsListContainer"
          , hxGet_ editURI
          , editAction
          ]
          $ toHtml monitor.alertConfig.title
        td_ [] do
          a_
            [ class_ "btn btn-ghost btn-xs"
            , hxTarget_ "#alertsListContainer"
            , hxGet_ editURI
            , editAction
            ]
            "edit"
          a_
            [ class_ "btn btn-ghost btn-xs"
            , hxTarget_ "#alertsListContainer"
            , hxPost_ $ "/p/" <> monitor.projectId.toText <> "/alerts/" <> monitor.id.toText <> "/toggle_active"
            ]
            if isJust monitor.deactivatedAt then "reactivate" else "deactivate"


-- | Alert overview page handler - redirects to unified monitor overview
alertOverviewGetH :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders Alert)
alertOverviewGetH pid alertId = do
  -- Redirect to the unified monitor overview page
  addRespHeaders $ AlertRedirect $ "/p/" <> pid.toText <> "/monitors/" <> alertId.toText <> "/overview"

-- Removed old alert overview page components as they are now handled in the unified Testing module
