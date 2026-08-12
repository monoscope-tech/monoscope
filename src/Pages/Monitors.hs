module Pages.Monitors (
  alertSingleGetH,
  convertToQueryMonitor,
  alertSingleToggleActiveH,
  alertListGetH,
  alertUpsertPostH,
  alertTeamDeleteH,
  AlertUpsertForm (..),
  Alert (..),
  -- Shared alert section components
  monitorScheduleSection_,
  thresholdsSection_,
  notificationSettingsSection_,
  -- Unified monitors (formerly Testing)
  UnifiedMonitorItem (..),
  unifiedMonitorsGetH,
  unifiedMonitorOverviewH,
  statusBadge_,
  teamAlertsGetH,
  alertBulkActionH,
  alertMuteH,
  alertUnmuteH,
  alertResolveH,
  alertDeleteH,
)
where

import Data.Aeson qualified as AE
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Either.Extra (fromRight')
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Effectful.Concurrent.Async (concurrently)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Integrations qualified as Slack
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.ProjectMembers (Team (discord_channels, slack_channels))
import Models.Projects.ProjectMembers qualified as ManageMembers
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, navTabAttrs)
import Pages.Bots.Discord qualified as Discord
import Pages.Bots.Slack qualified as Slack
import Pages.Bots.Utils (Channel (channelId, channelName))
import Pages.Components (FieldCfg (..), FieldSize (..), PanelCfg (..), durationMenu_, durationQuery, formCheckbox_, formField_, formSelectField_, metadataChip_, panel_, tagInput_, untilLabel)
import Pages.Projects (TBulkActionForm (..))
import Pkg.Components.Table (BulkAction (..), Config (..), Features (..), SearchMode (..), TabFilter (..), TabFilterOpt (..), Table (..), TableRows (..), ZeroState (..), col, withAttrs)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget (Widget (..))
import Pkg.Components.Widget qualified as Widget
import Pkg.Parser (defSqlQueryCfg, finalAlertQuery, fixedUTCTime, parseQueryToAST, parseQueryToComponents, presetRollup)
import Pkg.Parser.Expr (ToQueryText (..))
import Pkg.QueryCache (rewriteBinAutoToFixed)
import Relude hiding (ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types
import Text.Time.Pretty (prettyTimeAuto)
import Utils (checkFreeTierStatus, faSprite_, formatWithCommas, prettyTimeShort, toUriStr)
import Web.FormUrlEncoded (FromForm)


data AlertUpsertForm = AlertUpsertForm
  { alertId :: Maybe Text
  , alertThreshold :: Double
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
  , -- Recovery thresholds (hysteresis)
    alertRecoveryThreshold :: Maybe Text
  , warningRecoveryThreshold :: Maybe Text
  , -- Widget alert fields
    widgetId :: Maybe Text
  , dashboardId :: Maybe Text
  , -- Renotify / stop-after
    notifyAfterCheck :: Maybe Bool
  , notifyAfter :: Maybe Text
  , stopAfterCheck :: Maybe Bool
  , stopAfter :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


convertToQueryMonitor :: Projects.ProjectId -> UTCTime -> Monitors.QueryMonitorId -> AlertUpsertForm -> Monitors.QueryMonitor
convertToQueryMonitor projectId now queryMonitorId alertForm =
  let sqlQueryCfg = (defSqlQueryCfg projectId fixedUTCTime Nothing Nothing){presetRollup = Just "5m"}
      (_, qc) = fromRight' $ parseQueryToComponents sqlQueryCfg alertForm.query
      warningThresholdD = readMaybe . toString =<< alertForm.warningThreshold

      checkInterval = maybe 5 (max 1 . fromMaybe 5 . readMaybe . toString . T.dropEnd 1) alertForm.frequency
      timeWindowMins = maybe 60 parseIntervalToMins alertForm.timeWindow

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
      alertRecoveryD = readMaybe . toString =<< alertForm.alertRecoveryThreshold
      warningRecoveryD = readMaybe . toString =<< alertForm.warningRecoveryThreshold
      dashboardUuid = UUID.fromText =<< alertForm.dashboardId
      renotifyMins = if fromMaybe False alertForm.notifyAfterCheck then Just (parseIntervalToMins $ fromMaybe "30m" alertForm.notifyAfter) else Nothing
      stopCount = if fromMaybe False alertForm.stopAfterCheck then alertForm.stopAfter <|> Just 5 else Nothing
   in Monitors.QueryMonitor
        { id = queryMonitorId
        , createdAt = now
        , updatedAt = now
        , projectId = projectId
        , checkIntervalMins = checkInterval
        , alertThreshold = if isThresholdAlert then alertForm.alertThreshold else 0
        , warningThreshold = if isThresholdAlert then warningThresholdD else Nothing
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
        , mutedUntil = Nothing
        , visualizationType = fromMaybe "timeseries" alertForm.vizType
        , teams = V.fromList alertForm.teams
        , widgetId = alertForm.widgetId
        , dashboardId = dashboardUuid
        , alertRecoveryThreshold = if isThresholdAlert then alertRecoveryD else Nothing
        , warningRecoveryThreshold = if isThresholdAlert then warningRecoveryD else Nothing
        , currentStatus = Monitors.MSNormal
        , currentValue = 0
        , renotifyIntervalMins = renotifyMins
        , stopAfterCount = stopCount
        , notificationCount = 0
        , timeWindowMins
        }


parseIntervalToMins :: Text -> Int
parseIntervalToMins = \case
  "1h" -> 60
  "6h" -> 360
  "24h" -> 1440
  t -> fromMaybe 30 $ readMaybe $ toString $ T.dropEnd 1 t


minsToInterval :: Int -> Text
minsToInterval = \case
  60 -> "1h"
  360 -> "6h"
  1440 -> "24h"
  m -> show m <> "m"


alertUpsertPostH :: Projects.ProjectId -> AlertUpsertForm -> ATAuthCtx (RespHeaders Alert)
alertUpsertPostH pid form = do
  _ <- Projects.sessionAndProject pid
  let alertId = form.alertId >>= UUID.fromText
  queryMonitorId <- Monitors.QueryMonitorId <$> maybe (liftIO UUID.nextRandom) pure alertId
  now <- Time.currentTime

  -- For widget-tied alerts, preserve the query from the widget (can't edit directly)
  existingMonitor <- maybe (pure Nothing) (Monitors.queryMonitorById . Monitors.QueryMonitorId) alertId

  let baseMonitor = convertToQueryMonitor pid now queryMonitorId form
      queryMonitor = maybe baseMonitor (\e -> baseMonitor{Monitors.logQuery = e.logQuery, Monitors.logQueryAsSql = e.logQueryAsSql}) $ mfilter (isJust . (.widgetId)) existingMonitor

  _ <- Monitors.queryMonitorUpsert queryMonitor
  when (isNothing alertId)
    $ void
    $ Hasql.interpExecute [HI.sql| UPDATE projects.projects SET onboarding_steps_completed = array_append(onboarding_steps_completed, 'created_monitor') WHERE id = #{pid} AND NOT ('created_monitor' = ANY(onboarding_steps_completed)) |]
  addSuccessToast "Monitor was updated successfully" Nothing
  addRespHeaders $ AlertNoContent ""


alertListGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders Alert)
alertListGetH pid = do
  _ <- Projects.sessionAndProject pid
  monitors <- V.fromList <$> Monitors.queryMonitorsAll pid
  addRespHeaders $ AlertListGet monitors


alertSingleToggleActiveH :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders Alert)
alertSingleToggleActiveH pid monitorId = do
  _ <- Projects.sessionAndProject pid
  void $ Monitors.monitorToggleActiveById monitorId
  redirectCS $ "/p/" <> pid.toText <> "/monitors"
  addRespHeaders $ AlertNoContent ""


alertSingleGetH :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders Alert)
alertSingleGetH pid monitorId = do
  _ <- Projects.sessionAndProject pid
  monitor <- Monitors.queryMonitorById monitorId
  addRespHeaders $ AlertSingle pid monitor


data Alert
  = AlertListGet (V.Vector Monitors.QueryMonitor)
  | AlertSingle Projects.ProjectId (Maybe Monitors.QueryMonitor)
  | AlertNoContent Text


instance ToHtml Alert where
  toHtml (AlertListGet monitors) = toHtml $ queryMonitors_ monitors
  toHtml (AlertSingle pid monitor) = toHtml $ alertSingleComp pid monitor
  toHtml (AlertNoContent msg) = toHtml msg
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
      th_ "Source"
      th_ ""
    tbody_ do
      V.forM_ monitors \monitor -> tr_ do
        let editURI = "/p/" <> monitor.projectId.toText <> "/monitors/alerts/" <> monitor.id.toText
            isWidget = isJust monitor.widgetId
            isDeactivated = isJust monitor.deactivatedAt
            sourceLabel = do
              faSprite_ (bool "file-lines" "chart-simple" isWidget) "regular" "w-3.5 h-3.5 text-iconNeutral"
              bool "Log Explorer" "Widget" isWidget
        td_
          [ class_ $ bool "" "line-through" isDeactivated
          , hxTarget_ "#alertsListContainer"
          , hxGet_ editURI
          ]
          $ toHtml monitor.alertConfig.title
        td_ [class_ "text-sm text-textWeak"]
          $ case (,) <$> monitor.widgetId <*> monitor.dashboardId of
            Just (wid, dashId) -> a_ [class_ "flex items-center gap-1 hover:text-textStrong", href_ $ "/p/" <> monitor.projectId.toText <> "/dashboards/" <> UUID.toText dashId <> "#" <> wid] sourceLabel
            Nothing -> span_ [class_ "flex items-center gap-1"] sourceLabel
        td_ [class_ "flex items-center gap-1"] do
          a_
            [ class_ "btn btn-ghost btn-xs btn-square text-iconNeutral"
            , hxTarget_ "#alertsListContainer"
            , hxGet_ editURI
            , Aria.label_ "Edit"
            ]
            $ faSprite_ "pen-to-square" "regular" "w-3.5 h-3.5"
          a_
            [ class_ "btn btn-ghost btn-xs btn-square text-iconNeutral"
            , hxTarget_ "#alertsListContainer"
            , hxPost_ $ "/p/" <> monitor.projectId.toText <> "/monitors/alerts/" <> monitor.id.toText <> "/toggle_active"
            , Aria.label_ $ bool "Delete" "Reactivate" isDeactivated
            ]
            $ faSprite_ (bool "trash" "arrow-rotate-left" isDeactivated) "regular" "w-3.5 h-3.5"


alertTeamDeleteH :: Projects.ProjectId -> Monitors.QueryMonitorId -> UUID.UUID -> ATAuthCtx (RespHeaders Alert)
alertTeamDeleteH pid monitorId teamId = do
  _ <- Projects.sessionAndProject pid
  _ <- Monitors.monitorRemoveTeam pid monitorId teamId
  addSuccessToast "Team removed from monitor successfully" Nothing
  addRespHeaders $ AlertNoContent ""


monitorScheduleSection_ :: Text -> Int -> Int -> Maybe Text -> Maybe Text -> Html ()
monitorScheduleSection_ paymentPlan defaultFrequency defaultTimeWindow conditionType chartTargetIdM = do
  let timeOpts :: [(Int, Text)]
      timeOpts = [(1, "minute"), (2, "2 minutes"), (5, "5 minutes"), (10, "10 minutes"), (15, "15 minutes"), (30, "30 minutes"), (60, "hour"), (360, "6 hours"), (720, "12 hours"), (1440, "day")]
      isByos = paymentPlan == "Bring your own storage"
      isFree = Projects.isFreeTier paymentPlan
      minFreq
        | isFree = 60
        | isByos = 1
        | otherwise = 5
      clampedFreq = max minFreq defaultFrequency
      mkFreqOpt (m, l) =
        let isDisabled = m < minFreq
            attrs = [value_ (show m <> "m")] <> [disabled_ "" | isDisabled] <> [selected_ "" | m == clampedFreq]
         in option_ attrs ("every " <> toHtml l)
      mkTimeOpt (m, l) = option_ ([value_ (show m <> "m")] <> [selected_ "" | m == defaultTimeWindow]) ("the last " <> toHtml l)
      isThresholdType = maybe True (== "threshold_exceeded") conditionType
      chartUpdateAttr = case chartTargetIdM of
        Just chartId -> term "_" [text|on change set chart to document.getElementById('${chartId}') if chart exists then call chart.updateRollup(my.value) end|]
        Nothing -> [__|on change set qb to document.querySelector('query-builder') if qb exists then call qb.updateBinInQuery('timestamp', my.value) end|]
  panel_ def{icon = Just "clock", collapsible = Just True} "Monitor Schedule" do
    when isFree $ p_ [class_ "text-xs text-textWeak mt-1"] "Free plan: hourly minimum frequency. Upgrade for faster checks."
    div_ [class_ "flex gap-2 py-2"] do
      formSelectField_ FieldSm "Execute the query" "frequency" False $ forM_ timeOpts mkFreqOpt
      formField_ FieldSm def "Include rows from" "timeWindow" False
        $ Just (select_ [class_ "select select-bordered select-sm w-full", name_ "timeWindow", id_ "timeWindow", chartUpdateAttr] $ forM_ timeOpts mkTimeOpt)
      formField_ FieldSm def "Notify me when" "conditionType" False
        $ Just
        $ select_ [name_ "conditionType", class_ "select select-bordered select-sm w-full", id_ "condType", [__|on change if my value == 'threshold_exceeded' then set #thresholds.open to true else set #thresholds.open to false end|]] do
          option_ ([value_ "threshold_exceeded"] <> [selected_ "" | isThresholdType]) "threshold is exceeded"
          option_ ([value_ "has_matches"] <> [selected_ "" | not isThresholdType]) "the query has any results"


thresholdsSection_ :: Maybe Text -> Maybe Double -> Maybe Double -> Bool -> Maybe Double -> Maybe Double -> Html ()
thresholdsSection_ chartTargetIdM alertThresholdM warningThresholdM triggerLessThan alertRecoveryM warningRecoveryM = do
  let chartUpdateAttr = case chartTargetIdM of
        Just chartId -> term "_" [text|on input set chart to document.getElementById('${chartId}') if chart exists call chart.applyThresholds({alert: parseFloat(#alertThreshold.value), warning: parseFloat(#warningThreshold.value)}) end|]
        Nothing -> [__|on input set chart to #visualization-widget if chart exists call chart.applyThresholds({alert: parseFloat(#alertThreshold.value), warning: parseFloat(#warningThreshold.value)}) end|]
      showVal = maybe "" show
  panel_ def{icon = Just "chart-line", collapsible = Just True, sectionId = Just "thresholds"} "Thresholds" do
    div_ [class_ "flex flex-row gap-2 py-2"] do
      formField_ FieldSm def{inputType = "number", dot = Just "bg-fillError-strong", suffix = Just "events", value = showVal alertThresholdM, extraAttrs = [chartUpdateAttr]} "Alert threshold" "alertThreshold" True Nothing
      formField_ FieldSm def{inputType = "number", dot = Just "bg-fillWarning-strong", suffix = Just "events", value = showVal warningThresholdM, extraAttrs = [chartUpdateAttr]} "Warning threshold" "warningThreshold" False Nothing
      formSelectField_ FieldSm "Trigger condition" "direction" False do
        option_ (value_ "above" : [selected_ "" | not triggerLessThan]) "Above threshold"
        option_ (value_ "below" : [selected_ "" | triggerLessThan]) "Below threshold"
    div_ [class_ "mt-3 pt-3 border-t border-strokeWeak space-y-2"] do
      div_ [class_ "space-y-1 text-xs text-textWeak"] do
        div_ (span_ [class_ "font-medium text-textStrong"] "Recovery thresholds " >> span_ "(optional)")
        p_ "Alert recovers only when value crosses these thresholds"
      div_ [class_ "flex flex-row gap-2 py-2"] do
        formField_ FieldSm def{inputType = "number", dot = Just "bg-fillError-strong", suffix = Just "events", placeholder = "Same as trigger", value = showVal alertRecoveryM} "Alert recovery" "alertRecoveryThreshold" False Nothing
        formField_ FieldSm def{inputType = "number", dot = Just "bg-fillWarning-strong", suffix = Just "events", placeholder = "Same as trigger", value = showVal warningRecoveryM} "Warning recovery" "warningRecoveryThreshold" False Nothing


notificationSettingsSection_ :: Maybe Text -> Maybe Text -> Maybe Text -> Bool -> V.Vector ManageMembers.Team -> V.Vector UUID.UUID -> Text -> Maybe Monitors.QueryMonitor -> Html ()
notificationSettingsSection_ severityM subjectM messageM emailAll allTeams selectedTeamIds formId monitorM = do
  let defaultSeverity = fromMaybe "Error" severityM
      defaultSubject = fromMaybe "Alert triggered" subjectM
      defaultMessage = fromMaybe "The alert threshold has been exceeded. Check the APItoolkit dashboard for details." messageM
      teamList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= x.handle, "value" AE..= x.id]) <$> allTeams
      teamName tId = maybe "Unknown Team" (.handle) $ V.find (\t -> t.id == tId) allTeams
      existingTeams = decodeUtf8 $ AE.encode $ (\tId -> AE.object ["name" AE..= teamName tId, "value" AE..= tId]) <$> selectedTeamIds
      renotifyEnabled = maybe True (isJust . (.renotifyIntervalMins)) monitorM
      renotifyVal = maybe "30m" minsToInterval $ monitorM >>= (.renotifyIntervalMins)
      stopEnabled = maybe False (isJust . (.stopAfterCount)) monitorM
      stopVal = maybe "5" show $ monitorM >>= (.stopAfterCount)
  panel_ def{icon = Just "envelope", collapsible = Just False} "Notification Settings" do
    div_ [class_ "flex items-center w-full gap-2 mb-3"] do
      formSelectField_ FieldSm "Severity" "severity" False $ forM_ ["Info", "Error", "Warning", "Critical"] \s -> option_ [selected_ "" | defaultSeverity == s] $ toHtml s
      div_ [class_ "flex-1"] $ formField_ FieldSm def{value = defaultSubject, placeholder = "e.g. Alert triggered for high error rate"} "Subject" "subject" False Nothing
    formField_ FieldSm def{inputType = "textarea", value = defaultMessage, placeholder = "Alert message details", extraAttrs = [rows_ "3"]} "Message" "message" False Nothing
    div_ [class_ "border-t border-strokeWeak pt-4 mt-4"] do
      div_ [class_ "mb-3"] do
        h4_ [class_ "font-medium text-sm text-textStrong mb-1"] "Recovery Notifications"
        p_ [class_ "text-xs text-textWeak"] "Continue notifications until monitor recovers"
      div_ [class_ "space-y-3"] do
        div_ [class_ "flex items-center"] do
          formCheckbox_ FieldSm "Renotify every" "notifyAfterCheck" $ value_ "true" : [checked_ | renotifyEnabled]
          select_ [class_ "select select-sm w-28 ml-2", name_ "notifyAfter", id_ "notifyAfterInterval"] $ forM_ @[] @_ @(Text, Text) [("10m", "10 mins"), ("20m", "20 mins"), ("30m", "30 mins"), ("1h", "1 hour"), ("6h", "6 hours"), ("24h", "24 hours")] \(v, t) -> option_ (value_ v : [selected_ "" | v == renotifyVal]) $ toHtml t
        div_ [class_ "group/stopafter flex items-center"] do
          formCheckbox_ FieldSm "Stop after" "stopAfterCheck" $ value_ "true" : id_ "stopAfterCheck" : [checked_ | stopEnabled]
          div_ [class_ "items-center gap-1.5 ml-2 hidden group-has-[#stopAfterCheck:checked]/stopafter:flex", id_ "stopAfterInput"] do
            input_ [type_ "number", class_ "input input-sm w-16", value_ stopVal, name_ "stopAfter", min_ "1", max_ "100"]
            span_ [class_ "text-xs text-textWeak"] "occurrences"
    div_ [class_ "border-t border-strokeWeak pt-4 mt-4"] do
      div_ [class_ "flex flex-col gap-1"] do
        span_ [class_ "text text-sm"] "Teams"
        span_ [class_ "text-xs text-textWeak"] "Add teams to notify (if no team is added, project level notification channels will be used)"
      tagInput_ (formId <> "-teams") "" [name_ "teams", data_ "tagify-text-prop" "name", data_ "tagify-whitelist" teamList, data_ "tagify-initial" existingTeams]
    div_ [class_ "flex items-center gap-2 mt-4 pt-3 border-t border-strokeWeak"] do
      formCheckbox_ FieldMd "Send to all team members" "recipientEmailAll" $ value_ "true" : [checked_ | emailAll]
      span_ [class_ "tooltip", term "data-tip" "Configure specific recipients in alert settings after creation"] $ faSprite_ "circle-info" "regular" "w-3.5 h-3.5 text-iconNeutral"


---------------------------------
-- Unified Monitors (formerly Pages.Monitors.Testing)

data UnifiedMonitorItem = UnifiedMonitorItem
  { monitorId :: Text
  , projectId :: Text
  , title :: Text
  , status :: Text -- "Active", "Inactive"
  , currentStatus :: Monitors.MonitorStatus
  , mutedUntil :: Maybe UTCTime
  , schedule :: Text
  , lastRun :: Maybe UTCTime
  , now :: UTCTime
  , details :: UnifiedMonitorDetails
  , teamBadges :: [(Text, Text)] -- [(teamId, teamHandle)]
  }


data UnifiedMonitorDetails = AlertDetails
  { query :: Text
  , alertThreshold :: Double
  , warningThreshold :: Maybe Double
  , triggerDirection :: Text -- "above" or "below"
  , visualizationType :: Text
  }


teamAlertsGetH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders (TableRows UnifiedMonitorItem))
teamAlertsGetH pid teamId = do
  _ <- Projects.sessionAndProject pid
  alerts <- Monitors.getAlertsByTeamHandle pid teamId
  currTime <- Time.currentTime
  teamMap <- buildTeamMap pid
  let alerts' = V.fromList $ map (toUnifiedMonitorItem teamMap pid currTime) alerts

  addRespHeaders $ TableRows [] alerts' Nothing False Nothing Nothing Nothing


alertBulkActionH :: Projects.ProjectId -> Text -> TBulkActionForm -> ATAuthCtx (RespHeaders (PageCtx (Table UnifiedMonitorItem)))
alertBulkActionH pid action form = do
  _ <- Projects.sessionAndProject pid
  let monitorIds = Monitors.QueryMonitorId <$> form.itemId
  unless (null monitorIds) do
    case action of
      "deactivate" -> void $ Monitors.monitorDeactivateByIds monitorIds
      "reactivate" -> void $ Monitors.monitorReactivateByIds monitorIds
      "mute" -> void $ Monitors.monitorMuteByIds Nothing monitorIds
      "unmute" -> void $ Monitors.monitorUnmuteByIds monitorIds
      "resolve" -> void $ Monitors.monitorResolveByIds monitorIds
      "delete" -> void $ Monitors.monitorSoftDeleteByIds monitorIds
      _ -> pass
    addTriggerEvent "monitorsListChanged" AE.Null
  unifiedMonitorsGetH pid (Just $ bool "Active" "Inactive" (action == "deactivate")) Nothing


unifiedMonitorsGetH
  :: Projects.ProjectId
  -> Maybe Text -- filter
  -> Maybe Text -- since
  -> ATAuthCtx (RespHeaders (PageCtx (Table UnifiedMonitorItem)))
unifiedMonitorsGetH pid filterTM _sinceM = do
  (_, project, bw) <- mkPageCtx pid
  currTime <- Time.currentTime

  let filterType = fromMaybe "Active" filterTM

  allAlerts <- Monitors.queryMonitorsAll pid
  teamMap <- buildTeamMap pid
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan

  let (activeAlerts, inactiveAlerts) = partition (isNothing . (.deactivatedAt)) allAlerts
      alerts = bool inactiveAlerts activeAlerts (filterType == "Active")
      sortKey i = ((statusInfo i.currentStatus).rank, isNothing i.mutedUntil)
      allItems = V.fromList $ sortOn sortKey $ toUnifiedMonitorItem teamMap pid currTime <$> alerts
      currentURL = "/p/" <> pid.toText <> "/monitors?"
      monitorsTable =
        Table
          { config = def{elemID = "monitorsListForm", containerId = Just "monitorsListContainer", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0, refreshOnEvent = Just ("monitorsListChanged", currentURL <> "filter=" <> filterType)}
          , columns =
              [ col "Name" renderNameCol & withAttrs [class_ "min-w-0"]
              , col "Teams" (\i -> forM_ i.teamBadges \(_, handle) -> span_ [class_ "badge badge-sm badge-neutral mr-1"] $ toHtml handle) & withAttrs [class_ "w-48 max-md:hidden"]
              , col "Schedule" (\i -> span_ [class_ "text-xs text-textWeak whitespace-nowrap tabular-nums"] $ toHtml i.schedule) & withAttrs [class_ "w-28 max-md:hidden"]
              , col "Last Run" (\i -> span_ [class_ "text-xs text-textWeak whitespace-nowrap tabular-nums"] $ maybe "Never" (toHtml . prettyTimeShort i.now) i.lastRun) & withAttrs [class_ "w-28 max-md:hidden"]
              , col "Threshold" renderThresholdCol & withAttrs [class_ "w-40 max-md:hidden"]
              ]
          , rows = allItems
          , features =
              def
                { search = Just ClientSide
                , rowId = Just (.monitorId)
                , rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"]
                , bulkActions = bulkActionsFor filterType pid
                , zeroState =
                    Just
                      $ ZeroState
                        { icon = "bell"
                        , title = "No monitors configured yet"
                        , description = "Get notified when your logs, spans, errors, or metrics match specific conditions"
                        , actionText = "Create monitor"
                        , destination =
                            Left $ "/p/" <> pid.toText <> "/log_explorer#create-alert-toggle"
                        }
                }
          }
      bwconf =
        bw
          { pageTitle = "Monitors"
          , menuItem = Just "Monitors"
          , docsLink = Just "https://monoscope.tech/docs/monitors/"
          , freeTierStatus = freeTierStatus
          , pageActions = Just $ div_ [class_ "flex gap-2"] do
              a_ [class_ "btn btn-sm btn-primary gap-2", href_ $ "/p/" <> pid.toText <> "/log_explorer#create-alert-toggle"] do
                faSprite_ "bell" "regular" "h-4 w-4 max-md:hidden"
                faSprite_ "plus" "regular" "h-4 w-4 md:hidden"
                span_ [class_ "max-md:hidden"] "Create monitor"
          , navTabs =
              Just
                $ toHtml
                $ TabFilter
                  { current = filterType
                  , currentURL
                  , clientSide = False
                  , options =
                      [ TabFilterOpt{name = "Active", count = Just $ length activeAlerts, targetId = Nothing}
                      , TabFilterOpt{name = "Inactive", count = Just $ length inactiveAlerts, targetId = Nothing}
                      ]
                  }
          }

  addRespHeaders $ PageCtx bwconf monitorsTable


renderNameCol :: UnifiedMonitorItem -> Html ()
renderNameCol item = do
  let base = "/p/" <> item.projectId <> "/monitors"
      alertBase = base <> "/alerts/" <> item.monitorId
      si = statusInfo item.currentStatus
      isActive = item.status == "Active"
      inlineBtn tip icon hxAction extraAttrs =
        button_ ([type_ "button", data_ "tip" tip, class_ "cursor-pointer hover:text-textBrand transition-colors tap-target tooltip tooltip-top", hxSwap_ "none", hxAction] <> extraAttrs)
          $ faSprite_ icon "regular" "h-3.5 w-3.5"
      actionBtns = do
        inlineBtn (bool "Activate" "Deactivate" isActive) (bool "play" "pause" isActive) (hxPost_ $ alertBase <> "/toggle_active") []
        if isJust item.mutedUntil
          then inlineBtn "Unmute" "bell" (hxPost_ $ alertBase <> "/unmute") []
          else durationMenu_ ("mute-pop-" <> item.monitorId) "Mute for…" (\q -> [hxPost_ $ alertBase <> "/mute" <> durationQuery "duration" q, hxSwap_ "none"]) \popId ->
            inlineBtn "Mute" "bell-slash" (term "popovertarget" popId) [style_ $ "anchor-name: --anchor-" <> popId]
        when (item.currentStatus /= Monitors.MSNormal) $ inlineBtn "Resolve" "check" (hxPost_ $ alertBase <> "/resolve") []
        inlineBtn "Delete" "trash" (hxDelete_ alertBase) [hxConfirm_ "Are you sure you want to delete this monitor?"]
  div_ [class_ "flex flex-col gap-1 py-0.5"] do
    div_ [class_ "flex items-center gap-2"] do
      span_ [class_ $ "inline-block w-2 h-2 rounded-full shrink-0 tooltip tooltip-right " <> si.dotColor <> bool "" " alert-dot" (item.currentStatus == Monitors.MSAlerting), data_ "tip" $ bool "Inactive" "Active" isActive] ""
      a_ ([href_ $ base <> "/" <> item.monitorId <> "/overview", class_ "text-sm font-medium text-textStrong hover:text-textBrand transition-colors truncate"] <> navTabAttrs) $ toHtml $ bool item.title "(Untitled)" (T.null item.title)
      when (item.currentStatus /= Monitors.MSNormal) $ statusBadge_ False si.statusLabel
      whenJust item.mutedUntil \until' ->
        let muteLabel = untilLabel "Muted" item.now until'
         in span_ [class_ "badge badge-sm badge-ghost gap-1 shrink-0 tooltip tooltip-top", data_ "tip" muteLabel] do
              faSprite_ "bell-slash" "regular" "h-3 w-3"
              toHtml muteLabel
      div_ [class_ "flex gap-1 items-center shrink-0 opacity-0 max-md:hidden group-hover/row:opacity-100 has-[:focus-within]:opacity-100 transition-opacity"] actionBtns
    div_ [class_ "flex items-center gap-1.5"]
      $ span_ [class_ "text-xs text-textStrong/70 font-mono max-md:line-clamp-1 line-clamp-2 bg-fillWeaker border border-strokeWeak rounded px-1.5 py-0.5", term "data-tippy-content" item.details.query]
      $ toHtml item.details.query
    div_ [class_ "hidden max-md:flex items-center justify-between gap-2"] do
      div_ [class_ "flex items-center gap-x-1.5 gap-y-0.5 text-xs text-textWeak flex-wrap min-w-0"] do
        span_ [class_ "tabular-nums"] $ toHtml item.schedule
        span_ [class_ "text-textWeak/40"] "\xb7"
        span_ [class_ "tabular-nums"] $ maybe "Never run" (toHtml . prettyTimeShort item.now) item.lastRun
        when (item.details.alertThreshold > 0) do
          span_ [class_ "text-textWeak/40"] "\xb7"
          span_ [class_ "tabular-nums text-iconError bg-fillError-weak rounded-full px-1.5 py-px text-2xs"] $ toHtml $ formatWithCommas item.details.alertThreshold <> " " <> item.details.triggerDirection
        forM_ item.teamBadges \(_, handle) -> span_ [class_ "badge badge-sm badge-neutral"] $ toHtml handle
      div_ [class_ "flex gap-1 items-center shrink-0"] actionBtns


data StatusInfo = StatusInfo {dotColor :: Text, statusLabel :: Text, rank :: Int, textColor :: Text}


statusInfo :: Monitors.MonitorStatus -> StatusInfo
statusInfo = \case
  Monitors.MSAlerting -> StatusInfo "bg-fillError-strong" "Alerting" 0 "text-textError"
  Monitors.MSWarning -> StatusInfo "bg-fillWarning-strong" "Warning" 1 "text-textWarning"
  Monitors.MSNormal -> StatusInfo "bg-fillSuccess-strong" "Normal" 2 "text-textSuccess"


bulkActionsFor :: Text -> Projects.ProjectId -> [BulkAction]
bulkActionsFor filterType pid =
  let bulkBase = "/p/" <> pid.toText <> "/monitors/alerts/bulk_action/"
   in case filterType of
        "Active" ->
          [ BulkAction (Just "pause") "Deactivate" (bulkBase <> "deactivate")
          , BulkAction (Just "bell-slash") "Mute" (bulkBase <> "mute")
          , BulkAction (Just "bell") "Unmute" (bulkBase <> "unmute")
          , BulkAction (Just "check") "Resolve" (bulkBase <> "resolve")
          , BulkAction (Just "trash") "Delete" (bulkBase <> "delete")
          ]
        _ ->
          [ BulkAction (Just "play") "Reactivate" (bulkBase <> "reactivate")
          , BulkAction (Just "trash") "Delete" (bulkBase <> "delete")
          ]


buildTeamMap :: Projects.ProjectId -> ATAuthCtx (Map.Map UUID.UUID Text)
buildTeamMap pid = do
  allTeams <- ManageMembers.getTeams pid
  pure $ Map.fromList $ map (\t -> (t.id, t.handle)) allTeams


monitorActionH :: ([Monitors.QueryMonitorId] -> ATAuthCtx Int64) -> Text -> Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders (Html ()))
monitorActionH action msg pid monitorId = do
  _ <- Projects.sessionAndProject pid
  void $ action [monitorId]
  addSuccessToast msg Nothing
  redirectCS $ "/p/" <> pid.toText <> "/monitors"
  addRespHeaders ""


alertMuteH :: Projects.ProjectId -> Monitors.QueryMonitorId -> Maybe Int -> ATAuthCtx (RespHeaders (Html ()))
alertMuteH pid monitorId durationMinsM =
  monitorActionH (Monitors.monitorMuteByIds durationMinsM) (maybe "Monitor muted indefinitely" (const "Monitor muted") durationMinsM) pid monitorId


alertUnmuteH, alertResolveH, alertDeleteH :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders (Html ()))
alertUnmuteH = monitorActionH Monitors.monitorUnmuteByIds "Monitor unmuted"
alertResolveH = monitorActionH Monitors.monitorResolveByIds "Monitor resolved"
alertDeleteH pid monitorId = do
  (sess, _) <- Projects.sessionAndProject pid
  void $ Monitors.monitorSoftDeleteByIds [monitorId]
  Projects.logAuditS pid Projects.AEMonitorDeleted sess Nothing
  addSuccessToast "Monitor deleted" Nothing
  redirectCS $ "/p/" <> pid.toText <> "/monitors"
  addRespHeaders ""


renderThresholdCol :: UnifiedMonitorItem -> Html ()
renderThresholdCol item =
  div_ [class_ "flex flex-col gap-1"] do
    span_ [class_ "text-xs tabular-nums whitespace-nowrap bg-fillError-weak text-iconError rounded-full px-2 py-0.5 w-fit"] $ toHtml $ formatWithCommas item.details.alertThreshold <> " (" <> item.details.triggerDirection <> ")"
    whenJust item.details.warningThreshold \w ->
      span_ [class_ "text-xs tabular-nums whitespace-nowrap bg-fillWarning-weak text-iconWarning rounded-full px-2 py-0.5 w-fit"] $ toHtml $ formatWithCommas w <> " (warn)"


toUnifiedMonitorItem :: Map.Map UUID.UUID Text -> Projects.ProjectId -> UTCTime -> Monitors.QueryMonitor -> UnifiedMonitorItem
toUnifiedMonitorItem teamMap pid currTime alert =
  UnifiedMonitorItem
    { monitorId = alert.id.toText
    , projectId = pid.toText
    , title = alert.alertConfig.title
    , status = bool "Active" "Inactive" $ isJust alert.deactivatedAt
    , currentStatus = alert.currentStatus
    , mutedUntil = mfilter (> currTime) alert.mutedUntil
    , schedule = "every " <> show alert.checkIntervalMins <> " min"
    , lastRun = Just alert.lastEvaluated
    , now = currTime
    , details =
        AlertDetails
          { query = alert.logQuery
          , alertThreshold = alert.alertThreshold
          , warningThreshold = alert.warningThreshold
          , triggerDirection = bool "above" "below" alert.triggerLessThan
          , visualizationType = alert.visualizationType
          }
    , teamBadges = mapMaybe (\tid -> (UUID.toText tid,) <$> Map.lookup tid teamMap) $ V.toList alert.teams
    }


statusBadge_ :: Bool -> Text -> Html ()
statusBadge_ isLarge status = do
  let isAlerting = status `elem` ["Alerting", "alert"]
      (badgeClass, icon) = case status of
        "Passing" -> ("badge-success", "check")
        "Failing" -> ("badge-error", "xmark")
        "Active" -> ("badge-success", "circle-check")
        "Inactive" -> ("badge-ghost", "circle-pause")
        _ | isAlerting -> ("badge-error", "bell-exclamation")
        "Warning" -> ("badge-warning", "triangle-exclamation")
        "Healthy" -> ("badge-success", "heart-pulse")
        "Pending" -> ("badge-ghost", "clock")
        "NoData" -> ("badge-ghost", "circle-question")
        _ -> ("badge-ghost", "circle")
      sizeClass = bool "badge-sm" "" isLarge
      iconSize = bool "h-3 w-3" "h-4 w-4" isLarge
  span_ [class_ $ "badge gap-1 " <> sizeClass <> " " <> badgeClass <> bool "" " alert-badge" isAlerting] do
    faSprite_ icon "regular" iconSize
    toHtml status


-- | Rewrite @bin_auto@ to a fixed @<mins>m@ interval via the KQL AST so the monitor
-- overview chart bins match the evaluation window rather than the viewer's time range.
-- Falls back to the original query on parse failure or when @mins <= 0@.
rewriteBinAutoMins :: Int -> Text -> Text
rewriteBinAutoMins mins q
  | mins <= 0 = q
  | otherwise = either (const q) (toQText . rewriteBinAutoToFixed (show mins <> "m")) $ parseQueryToAST q


unifiedMonitorOverviewH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
unifiedMonitorOverviewH pid monitorId = do
  (_, project, bw) <- mkPageCtx pid
  appCtx <- ask @AuthContext
  currTime <- Time.currentTime
  (freeTierStatus, alertM) <-
    concurrently
      (checkFreeTierStatus pid project.paymentPlan)
      (maybe (pure Nothing) (Monitors.queryMonitorById . Monitors.QueryMonitorId) (UUID.fromText monitorId))

  let baseBwconf =
        bw
          { pageTitle = "Monitor Overview"
          , prePageTitle = Just "Monitors"
          , menuItem = Just "Monitors"
          , docsLink = Just "https://monoscope.tech/docs/monitors/"
          , freeTierStatus = freeTierStatus
          }

  case alertM of
    Just alert -> do
      (teams, (slackDataM, discordDataM)) <-
        concurrently
          (ManageMembers.getTeamsById pid alert.teams)
          (concurrently (Slack.getProjectSlackData pid) (Slack.getDiscordDataByProjectId pid))
      (channels, discordChannels) <-
        concurrently
          (maybe (pure []) (\sd -> maybe [] (fromMaybe [] . (.channels)) <$> Slack.getSlackChannels sd.botToken sd.teamId) slackDataM)
          (maybe (pure []) (Discord.getDiscordChannels appCtx.env.discordBotToken . (.guildId)) discordDataM)
      let muteBase = "/p/" <> pid.toText <> "/monitors/alerts/" <> alert.id.toText
          isInactive = isJust alert.deactivatedAt
          deactLabel = bool "Deactivate" "Activate" isInactive
          deactIcon = bool "pause" "circle-play" isInactive
          needsResolve = alert.currentStatus `elem` [Monitors.MSAlerting, Monitors.MSWarning]
          bwconf =
            baseBwconf
              { pageActions = Just $ div_ [class_ "flex items-center gap-2"] do
                  div_ [class_ "max-md:hidden flex items-center gap-2"] do
                    case alert.mutedUntil of
                      Just _ -> button_ [class_ "btn btn-sm btn-ghost border border-strokeWeak tooltip tooltip-bottom", term "aria-label" "Unmute", data_ "tip" "Resume notifications for this monitor", hxPost_ $ muteBase <> "/unmute"] do
                        faSprite_ "bell" "regular" "h-4 w-4"
                        "Unmute"
                      Nothing -> durationMenu_ ("mute-btn-pop-" <> alert.id.toText) "Mute for\x2026" (\q -> [hxPost_ $ muteBase <> "/mute" <> durationQuery "duration" q, hxSwap_ "none"]) \popId ->
                        button_ [type_ "button", class_ "btn btn-sm btn-ghost border border-strokeWeak tooltip tooltip-bottom", term "aria-label" "Mute", data_ "tip" "Silence notifications for a period", term "popovertarget" popId, style_ $ "anchor-name: --anchor-" <> popId] do
                          faSprite_ "bell-slash" "regular" "h-4 w-4"
                          span_ [class_ "max-md:hidden"] "Mute"
                    when needsResolve
                      $ button_ [class_ "btn btn-sm btn-ghost border border-strokeWeak tooltip tooltip-bottom", term "aria-label" "Resolve", data_ "tip" "Mark as resolved and reset status to normal", hxPost_ $ muteBase <> "/resolve"] do
                        faSprite_ "check" "regular" "h-4 w-4"
                        "Resolve"
                    button_ [class_ "btn btn-sm btn-ghost border border-strokeWeak tooltip tooltip-bottom", term "aria-label" deactLabel, data_ "tip" $ bool "Pause this monitor — it won't evaluate or alert" "Re-enable this monitor to resume evaluations" isInactive, hxPost_ $ muteBase <> "/toggle_active"] do
                      faSprite_ deactIcon "regular" "h-4 w-4"
                      toHtml deactLabel
                    div_ [class_ "w-px bg-strokeWeak h-5 mx-0.5"] mempty
                  let mobilePopId = "monitor-actions-" <> alert.id.toText
                      mobItem_ :: Text -> Text -> Text -> Html ()
                      mobItem_ icon path label =
                        button_ [type_ "button", class_ "px-3 py-2 text-sm text-left hover:bg-fillWeaker rounded cursor-pointer w-full flex items-center gap-2", hxPost_ $ muteBase <> path, hxSwap_ "none"] do
                          faSprite_ icon "regular" "h-3.5 w-3.5"
                          toHtml label
                  div_ [class_ "md:hidden inline-block"] do
                    button_ [type_ "button", class_ "btn btn-sm btn-ghost border border-strokeWeak", term "aria-label" "Actions", term "popovertarget" mobilePopId, style_ $ "anchor-name: --anchor-" <> mobilePopId] do
                      faSprite_ "ellipsis-vertical" "regular" "h-4 w-4"
                    div_ [id_ mobilePopId, term "popover" "auto", class_ "dropdown dropdown-end menu bg-bgRaised p-1 text-sm border border-strokeWeak z-50 min-w-44 rounded-md shadow-lg mt-1", style_ $ "position-try: flip-block; position-anchor: --anchor-" <> mobilePopId] do
                      maybe (mobItem_ "bell-slash" "/mute" "Mute") (const $ mobItem_ "bell" "/unmute" "Unmute") alert.mutedUntil
                      when needsResolve $ mobItem_ "check" "/resolve" "Resolve"
                      mobItem_ deactIcon "/toggle_active" deactLabel
                  a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer?alert=" <> alert.id.toText <> "&query=" <> alert.logQuery, class_ "btn btn-sm max-md:btn-ghost max-md:border max-md:border-strokeWeak btn-primary", term "aria-label" "Edit monitor", term "data-tippy-content" "Edit query, thresholds, and notification settings"] do
                    faSprite_ "pen-to-square" "regular" "h-4 w-4"
                    span_ [class_ "max-md:hidden"] "Edit monitor"
              }
      let nameOf cs x = maybe x (.channelName) $ find ((== x) . (.channelId)) cs
          teams' = (\t -> t{slack_channels = nameOf channels <$> t.slack_channels, discord_channels = nameOf discordChannels <$> t.discord_channels}) <$> teams
      addRespHeaders $ PageCtx bwconf $ unifiedOverviewPage pid alert currTime (V.fromList teams') slackDataM discordDataM
    _ -> addRespHeaders $ PageCtx baseBwconf $ div_ [class_ "p-6 text-center"] "Monitor not found"


unifiedOverviewPage :: Projects.ProjectId -> Monitors.QueryMonitor -> UTCTime -> V.Vector ManageMembers.Team -> Maybe Slack.SlackData -> Maybe Slack.DiscordData -> Html ()
unifiedOverviewPage pid alert currTime teams slackDataM discordDataM = do
  section_ [class_ "pt-2 mx-auto max-md:px-2 px-4 w-full flex flex-col gap-4 pb-4"] do
    div_ [class_ "flex items-center gap-3"] do
      h1_ [class_ "text-2xl font-semibold text-textStrong"] $ toHtml $ bool alert.alertConfig.title "(Untitled)" (T.null alert.alertConfig.title)
      statusBadge_ False displayName

    div_ [class_ "flex flex-wrap gap-2 items-center"] do
      metadataChip_ "shield-halved" $ "Severity: " <> alert.alertConfig.severity
      metadataChip_ "clock" $ "Every " <> show alert.checkIntervalMins <> " min"
      span_ [class_ "max-md:hidden"] $ metadataChip_ "calendar" $ "Created " <> toText (prettyTimeAuto currTime alert.createdAt)
      whenJust (mfilter (> currTime) alert.mutedUntil) \until' ->
        span_ [class_ "badge badge-sm badge-warning gap-1"] do
          faSprite_ "bell-slash" "regular" "h-3 w-3"
          toHtml $ untilLabel "Muted" currTime until'

    div_ [class_ "flex max-md:flex-col gap-6 max-md:gap-3 border-t border-strokeWeak pt-4"] do
      div_ [class_ "md:hidden"] $ alertSidebar_ displayName alert currTime
      div_ [class_ "flex-1 min-w-0 flex flex-col gap-3"] do
        div_ [class_ "flex items-center gap-2 flex-wrap"] do
          TimePicker.timepicker_ Nothing Nothing Nothing
          TimePicker.refreshButton_
        div_ [class_ "border border-strokeWeak rounded-lg max-md:p-2 p-3 h-48 md:h-80 overflow-hidden"] do
          Widget.widget_
            $ (def :: Widget)
              { Widget.wType = Widget.mapChatTypeToWidgetType alert.visualizationType
              , Widget.query = Just (rewriteBinAutoMins alert.timeWindowMins alert.logQuery)
              , Widget.title = Just "Query Results"
              , Widget.standalone = Just True
              , Widget._projectId = Just pid
              , Widget.layout = Just (def{Widget.w = Just 12, Widget.h = Just 6})
              , Widget.alertThreshold = Just alert.alertThreshold
              , Widget.warningThreshold = alert.warningThreshold
              , Widget.showThresholdLines = Just "always"
              }
      div_ [class_ "max-md:hidden w-78 shrink-0"] $ alertSidebar_ displayName alert currTime

    tabbedSection_ "monitor-tabs" [("Execution History", monitorHistoryTab_ pid alert.id), ("Notification Channels", alertNotificationsTab_ alert teams)]
  where
    displayName = bool (statusInfo alert.currentStatus).statusLabel "Inactive" (isJust alert.deactivatedAt)


tabbedSection_ :: Text -> [(Text, Html ())] -> Html ()
tabbedSection_ containerId tabs = do
  div_ [role_ "tablist", class_ "w-full", id_ containerId] do
    div_ [class_ "w-full flex border-b border-strokeWeak"] do
      forM_ (zip [0 :: Int ..] tabs) \(idx, (label, _)) -> do
        let tabId = containerId <> "-tab-" <> show idx
        button_
          [ class_ $ "cursor-pointer shrink-0 tab-btn tab-box text-sm font-medium px-3 py-2.5 text-textWeak border-b-2 border-b-transparent" <> bool "" " t-tab-active" (idx == 0)
          , role_ "tab"
          , term "aria-label" label
          , term "_" [text|on click set tl to #${containerId} then remove .t-tab-active from <.t-tab-active/> in tl then add .t-tab-active to me then add .hidden to <.t-tab-content/> in tl then remove .hidden from #${tabId}|]
          ]
          $ toHtml label
    forM_ (zip [0 :: Int ..] tabs) \(idx, (_, content)) ->
      div_ [role_ "tabpanel", class_ $ "overflow-y-auto t-tab-content" <> bool " hidden" "" (idx == 0), id_ $ containerId <> "-tab-" <> show idx] content


monitorHistoryTab_ :: Projects.ProjectId -> Monitors.QueryMonitorId -> Html ()
monitorHistoryTab_ pid alertId = do
  let query = "kind==\"alert\" and parent_id==\"" <> alertId.toText <> "\""
      initialUrl = "/p/" <> pid.toText <> "/log_explorer/data?json=true&query=" <> toUriStr query
  div_ [class_ "mt-2 p-2 border border-strokeWeak rounded-lg overflow-x-auto h-[400px] max-md:h-[300px]"] do
    termRaw "log-list" [id_ "resultTable", class_ "w-full divide-y shrink-1 flex flex-col h-full min-w-0 rr-block", term "windowTarget" "logList", term "projectId" pid.toText, term "initialFetchUrl" initialUrl] ("" :: Text)


alertSidebar_ :: Text -> Monitors.QueryMonitor -> UTCTime -> Html ()
alertSidebar_ displayName alert currTime = do
  div_ [class_ "w-full border border-strokeWeak rounded-lg divide-y divide-strokeWeak"] do
    div_ [class_ "md:hidden divide-y divide-strokeWeak text-sm"] do
      div_ [class_ "px-3 py-2 flex items-center justify-between"] do
        statusBadge_ True displayName
        span_ [class_ $ statusColor <> " tabular-nums text-xl font-bold"] $ toHtml $ formatWithCommas alert.currentValue
      mobileRow_ "Trigger" $ span_ [class_ "tabular-nums"] $ toHtml $ direction <> " " <> formatWithCommas alert.alertThreshold
      whenJust alert.warningThreshold \w -> mobileRow_ "Warning" $ span_ [class_ "tabular-nums text-textWarning"] $ toHtml $ direction <> " " <> formatWithCommas w
      label_ [class_ "px-3 py-1.5 flex flex-col gap-1 cursor-pointer"] do
        input_ [type_ "checkbox", class_ "peer sr-only"]
        span_ [class_ "text-xs text-textWeak"] "Query \x203a"
        pre_ [class_ "text-xs font-mono text-textStrong/70 whitespace-pre-wrap break-all line-clamp-1 peer-checked:line-clamp-none"] $ toHtml alert.logQuery
      mobileRow_ "Last eval" $ span_ [] $ toHtml $ toText (prettyTimeAuto currTime alert.lastEvaluated)
      mobileRow_ "Last triggered" $ span_ [class_ "text-textWeak"] $ toHtml $ maybe "Never" (toText . prettyTimeAuto currTime) alert.alertLastTriggered
    div_ [class_ "max-md:hidden divide-y divide-strokeWeak"] do
      sidebarItem_ "Status" $ statusBadge_ True displayName
      sidebarItem_ "Current Value" $ span_ [class_ $ statusColor <> " tabular-nums text-lg font-semibold"] $ toHtml $ formatWithCommas alert.currentValue
      sidebarItem_ "Query" $ pre_ [class_ "text-xs font-mono text-textStrong/70 overflow-x-auto whitespace-pre-wrap max-h-24"] $ toHtml alert.logQuery
      sidebarItem_ "Thresholds" $ div_ [class_ "flex flex-col gap-1 text-sm"] do
        span_ [class_ "text-textStrong tabular-nums"] $ toHtml $ "Trigger: " <> direction <> " " <> formatWithCommas alert.alertThreshold
        whenJust alert.warningThreshold \w -> span_ [class_ "text-textWarning tabular-nums"] $ toHtml $ "Warning: " <> direction <> " " <> formatWithCommas w
        whenJust alert.alertRecoveryThreshold \r -> span_ [class_ "text-textWeak tabular-nums"] $ toHtml $ "Recovery: " <> formatWithCommas r
      sidebarItem_ "Evaluation" $ div_ [class_ "flex flex-col gap-1 text-sm"] do
        span_ [class_ "text-textStrong"] $ toHtml $ "Last: " <> toText (prettyTimeAuto currTime alert.lastEvaluated)
        span_ [class_ "text-textWeak"] $ toHtml $ "Last Triggered: " <> maybe "Never" (toText . prettyTimeAuto currTime) alert.alertLastTriggered
      sidebarItem_ "Notifications" $ div_ [class_ "flex flex-col gap-1 text-sm"] do
        span_ [class_ "text-textStrong tabular-nums"] $ toHtml $ "Sent: " <> show @Text alert.notificationCount
        whenJust alert.renotifyIntervalMins \mins -> span_ [class_ "text-textWeak"] $ toHtml $ "Renotify: " <> show @Text mins <> " min"
        whenJust alert.stopAfterCount \count -> span_ [class_ "text-textWeak"] $ toHtml $ "Stop after: " <> show @Text count
  where
    direction = bool ">" "<" alert.triggerLessThan
    statusColor = (statusInfo alert.currentStatus).textColor
    mobileRow_ :: Html () -> Html () -> Html ()
    mobileRow_ label val = div_ [class_ "px-3 py-1.5 flex items-center justify-between gap-2"] do
      span_ [class_ "text-xs text-textWeak shrink-0"] label
      val
    sidebarItem_ :: Html () -> Html () -> Html ()
    sidebarItem_ label val = div_ [class_ "p-3 flex flex-col gap-1"] do
      span_ [class_ "text-xs font-medium text-textWeak uppercase tracking-wider"] label
      val


alertNotificationsTab_ :: Monitors.QueryMonitor -> V.Vector ManageMembers.Team -> Html ()
alertNotificationsTab_ alert teams = do
  div_ [class_ "pt-6 pb-3"] do
    div_ [class_ "mb-6"] do
      h4_ [class_ "text-base font-medium text-textStrong mb-2 flex items-center"] $ faSprite_ "users" "regular" "h-4 w-4 mr-2" >> "Teams"
      if V.null teams
        then do
          div_ [class_ "text-sm text-textWeak"] "No teams configured for this monitor."
          div_ [class_ "pt-2 flex items-center gap-1"] do
            span_ [class_ "text-sm text-textWeak"] "Project level notification integrations will be used."
            a_ [href_ $ "/p/" <> alert.projectId.toText <> "/settings/integrations", class_ "text-sm text-textBrand hover:underline"] "Configure integrations"
        else div_ [class_ "flex flex-wrap gap-4 mb-4"] $ forM_ teams \team ->
          div_ [class_ "flex flex-col border rounded-lg gap-4 border-strokeWeak p-6 relative w-96", id_ team.handle] do
            button_
              [ type_ "button"
              , class_ "absolute top-3 cursor-pointer right-3 text-iconNeutral hover:text-iconBrand transition-colors"
              , term "data-tippy-content" "Remove team"
              , hxDelete_ $ "/p/" <> alert.projectId.toText <> "/monitors/alerts/" <> alert.id.toText <> "/teams/" <> UUID.toText team.id
              , hxTarget_ $ "#" <> team.handle
              , hxSwap_ "outerHTML"
              ]
              $ faSprite_ "trash" "regular" "h-3 w-3"
            span_ [class_ "text-sm font-medium"] $ toHtml team.name
            forM_ ([("envelope", "regular", team.notify_emails), ("slack", "solid", ("#" <>) <$> team.slack_channels), ("discord", "brand", ("#" <>) <$> team.discord_channels)] :: [(Text, Text, V.Vector Text)]) \(icon, style, entries) ->
              forM_ entries \entry ->
                div_ [class_ "flex items-center gap-2"]
                  $ span_ [class_ "text-sm text-textWeak"]
                  $ faSprite_ icon style "h-3 w-3 mr-2 text-iconNeutral"
                  >> toHtml entry

    div_ [class_ "pt-6 pb-3"] do
      h4_ [class_ "font-medium text-textStrong mb-2"] "Notification Template"
      div_ [class_ "rounded-lg py-2 space-y-2"] do
        div_ [class_ "flex items-center gap-1"] do
          span_ [class_ "text-sm font-medium text-textStrong"] "Subject: "
          span_ [class_ "text-sm text-textWeak"] $ toHtml alert.alertConfig.subject
        div_ [class_ "flex flex-col gap-1"] do
          span_ [class_ "text-sm font-medium text-textStrong"] "Message"
          p_ [class_ "text-sm text-textWeak mt-1"] $ toHtml alert.alertConfig.message
