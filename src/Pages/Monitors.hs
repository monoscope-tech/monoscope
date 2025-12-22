module Pages.Monitors (
  alertSingleGetH,
  convertToQueryMonitor,
  alertSingleToggleActiveH,
  alertListGetH,
  alertUpsertPostH,
  alertOverviewGetH,
  monitorsPageGetH,
  alertTeamDeleteH,
  AlertUpsertForm (..),
  Alert (..),
)
where

import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Either.Extra (fromRight')
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as PG
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pkg.Parser (defSqlQueryCfg, finalAlertQuery, fixedUTCTime, parseQueryToComponents, presetRollup)
import Relude hiding (ask)
import System.Config (AuthContext (..))
import System.Types
import Utils (checkFreeTierExceeded, faSprite_)
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
  , frequency :: Maybe Text
  , timeWindow :: Maybe Text
  , conditionType :: Maybe Text
  , source :: Maybe Text
  , vizType :: Maybe Text
  , teams :: [UUID.UUID]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


convertToQueryMonitor :: Projects.ProjectId -> UTCTime -> Monitors.QueryMonitorId -> AlertUpsertForm -> Monitors.QueryMonitor
convertToQueryMonitor projectId now queryMonitorId alertForm =
  let sqlQueryCfg = (defSqlQueryCfg projectId fixedUTCTime Nothing Nothing){presetRollup = Just "5m"}
      (_, qc) = fromRight' $ parseQueryToComponents sqlQueryCfg alertForm.query
      warningThresholdInt = readMaybe . toString =<< alertForm.warningThreshold

      checkInterval = case alertForm.frequency of
        Just freq -> max 1 $ fromMaybe 5 $ readMaybe $ toString $ T.dropEnd 1 freq
        Nothing -> 5

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
        , thresholdSustainedForMins = 0
        , alertConfig
        , deletedAt = Nothing
        , deactivatedAt = Nothing
        , visualizationType = fromMaybe "timeseries" alertForm.vizType
        , teams = V.fromList alertForm.teams
        }


alertUpsertPostH :: Projects.ProjectId -> AlertUpsertForm -> ATAuthCtx (RespHeaders Alert)
alertUpsertPostH pid form = do
  let alertId = form.alertId >>= UUID.fromText
  queryMonitorId <- liftIO $ case alertId of
    Just alertId' -> pure (Monitors.QueryMonitorId alertId')
    Nothing -> Monitors.QueryMonitorId <$> UUID.nextRandom
  now <- Time.currentTime
  let queryMonitor = convertToQueryMonitor pid now queryMonitorId form

  _ <- Monitors.queryMonitorUpsert queryMonitor
  addSuccessToast "Monitor was updated successfully" Nothing
  addRespHeaders $ AlertNoContent ""


alertListGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders Alert)
alertListGetH pid = do
  monitors <- V.fromList <$> Monitors.queryMonitorsAll pid
  addRespHeaders $ AlertListGet monitors


alertSingleToggleActiveH :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders Alert)
alertSingleToggleActiveH pid monitorId = do
  _ <- Monitors.monitorToggleActiveById monitorId

  monitors <- V.fromList <$> Monitors.queryMonitorsAll pid
  addRespHeaders $ AlertListGet monitors


alertSingleGetH :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders Alert)
alertSingleGetH pid monitorId = do
  monitor <- Monitors.queryMonitorById monitorId
  addRespHeaders $ AlertSingle pid monitor


data Alert
  = AlertListGet (V.Vector Monitors.QueryMonitor)
  | AlertSingle Projects.ProjectId (Maybe Monitors.QueryMonitor)
  | AlertNoContent Text
  | AlertRedirect Text
  | AlertPage (PageCtx (Html ()))


instance ToHtml Alert where
  toHtml (AlertListGet monitors) = toHtml $ queryMonitors_ monitors
  toHtml (AlertSingle pid monitor) = toHtml $ alertSingleComp pid monitor
  toHtml (AlertNoContent msg) = toHtml msg
  toHtml (AlertRedirect url) = script_ $ "window.location.href = '" <> url <> "';"
  toHtml (AlertPage page) = toHtml page
  toHtmlRaw = toHtml


alertSingleComp :: Projects.ProjectId -> Maybe Monitors.QueryMonitor -> Html ()
alertSingleComp pid monitor = do
  div_ [] do
    a_ [class_ "border-y p-3 block cursor-pointer", hxGet_ $ "/p/" <> pid.toText <> "/monitors/alerts", hxTarget_ "#alertsListContainer"] "‹ Back to alerts list"
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
        let editURI = "/p/" <> monitor.projectId.toText <> "/monitors/alerts/" <> monitor.id.toText
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
            , hxPost_ $ "/p/" <> monitor.projectId.toText <> "/monitors/alerts/" <> monitor.id.toText <> "/toggle_active"
            ]
            if isJust monitor.deactivatedAt then "reactivate" else "deactivate"


-- | Alert overview page handler - redirects to unified monitor overview
alertOverviewGetH :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders Alert)
alertOverviewGetH pid alertId = do
  -- Redirect to the unified monitor overview page
  addRespHeaders $ AlertRedirect $ "/p/" <> pid.toText <> "/monitors/" <> alertId.toText <> "/overview"


-- | Full page handler for /p/<pid>/monitors
monitorsPageGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders Alert)
monitorsPageGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  monitors <- V.fromList <$> Monitors.queryMonitorsAll pid
  freeTierExceeded <- checkFreeTierExceeded pid project.paymentPlan
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Alerts"
          , menuItem = Just "Alerts"
          , docsLink = Just "https://monoscope.tech/docs/monitors/"
          , freeTierExceeded = freeTierExceeded
          , config = appCtx.env
          , pageActions = Just $ div_ [class_ "flex gap-2"] do
              a_ [class_ "btn btn-sm btn-primary gap-2", href_ $ "/p/" <> pid.toText <> "/log_explorer#create-alert-toggle"] do
                faSprite_ "bell" "regular" "h-4 w-4"
                "Create Alert"
          }
  addRespHeaders $ AlertPage $ PageCtx bwconf (monitorsPageContent_ pid monitors)


monitorsPageContent_ :: Projects.ProjectId -> V.Vector Monitors.QueryMonitor -> Html ()
monitorsPageContent_ pid monitors = do
  let activeMonitors = V.filter (isNothing . (.deactivatedAt)) monitors
      inactiveMonitors = V.filter (isJust . (.deactivatedAt)) monitors
  section_ [class_ "pt-2 mx-auto px-14 w-full flex flex-col gap-4"] do
    when (V.null monitors) do
      div_ [class_ "flex flex-col items-center justify-center py-16 text-center"] do
        faSprite_ "bell-slash" "regular" "h-12 w-12 text-iconNeutral mb-4"
        h3_ [class_ "text-lg font-medium text-textStrong mb-2"] "No alerts configured yet"
        p_ [class_ "text-textWeak mb-4"] "Create an alert to get notified when your queries match certain conditions."
        a_ [class_ "btn btn-primary", href_ $ "/p/" <> pid.toText <> "/log_explorer#create-alert-toggle"] "Create Alert"
    unless (V.null monitors) do
      div_ [class_ "flex gap-4 mb-4"] do
        div_ [class_ "badge badge-lg badge-ghost"] $ toHtml $ "Active: " <> show (V.length activeMonitors)
        div_ [class_ "badge badge-lg badge-ghost"] $ toHtml $ "Inactive: " <> show (V.length inactiveMonitors)
      div_ [id_ "alertsListContainer"] do
        queryMonitors_ monitors


alertTeamDeleteH :: Projects.ProjectId -> Monitors.QueryMonitorId -> UUID.UUID -> ATAuthCtx (RespHeaders Alert)
alertTeamDeleteH pid monitorId teamId = do
  _ <- dbtToEff $ Monitors.monitorRemoveTeam pid monitorId teamId
  addSuccessToast "Team removed from alert successfully" Nothing
  addRespHeaders $ AlertNoContent ""
