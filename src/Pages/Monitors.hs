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
  -- Shared alert UI helpers
  thresholdInput_,
  recoveryInput_,
  directionSelect_,
  frequencySelect_,
  collapsibleSection_,
  -- Shared alert section components
  monitorScheduleSection_,
  thresholdsSection_,
  notificationSettingsSection_,
)
where

import Data.Aeson qualified as AE
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Either.Extra (fromRight')
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pkg.Parser (defSqlQueryCfg, finalAlertQuery, fixedUTCTime, parseQueryToComponents, presetRollup)
import Relude hiding (ask)
import System.Config (AuthContext (..))
import System.Types
import Utils (checkFreeTierExceeded, faSprite_)
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
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


convertToQueryMonitor :: Projects.ProjectId -> UTCTime -> Monitors.QueryMonitorId -> AlertUpsertForm -> Monitors.QueryMonitor
convertToQueryMonitor projectId now queryMonitorId alertForm =
  let sqlQueryCfg = (defSqlQueryCfg projectId fixedUTCTime Nothing Nothing){presetRollup = Just "5m"}
      (_, qc) = fromRight' $ parseQueryToComponents sqlQueryCfg alertForm.query
      warningThresholdD = readMaybe . toString =<< alertForm.warningThreshold

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
      alertRecoveryD = readMaybe . toString =<< alertForm.alertRecoveryThreshold
      warningRecoveryD = readMaybe . toString =<< alertForm.warningRecoveryThreshold
      dashboardUuid = UUID.fromText =<< alertForm.dashboardId
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
        , visualizationType = fromMaybe "timeseries" alertForm.vizType
        , teams = V.fromList alertForm.teams
        , -- Widget alert fields
          widgetId = alertForm.widgetId
        , dashboardId = dashboardUuid
        , alertRecoveryThreshold = if isThresholdAlert then alertRecoveryD else Nothing
        , warningRecoveryThreshold = if isThresholdAlert then warningRecoveryD else Nothing
        , currentStatus = Monitors.MSNormal
        , currentValue = 0
        }


alertUpsertPostH :: Projects.ProjectId -> AlertUpsertForm -> ATAuthCtx (RespHeaders Alert)
alertUpsertPostH pid form = do
  let alertId = form.alertId >>= UUID.fromText
  queryMonitorId <- liftIO $ case alertId of
    Just alertId' -> pure (Monitors.QueryMonitorId alertId')
    Nothing -> Monitors.QueryMonitorId <$> UUID.nextRandom
  now <- Time.currentTime

  -- For widget-tied alerts, preserve the query from the widget (can't edit directly)
  existingMonitor <- case alertId of
    Just _ -> Monitors.queryMonitorById queryMonitorId
    Nothing -> pure Nothing

  let baseMonitor = convertToQueryMonitor pid now queryMonitorId form
      queryMonitor = case existingMonitor of
        Just existing
          | isJust existing.widgetId ->
              (baseMonitor :: Monitors.QueryMonitor){Monitors.logQuery = existing.logQuery, Monitors.logQueryAsSql = existing.logQueryAsSql}
        _ -> baseMonitor

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
      th_ "Source"
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
        td_ [class_ "text-sm text-textWeak"] do
          case monitor.widgetId of
            Just wid -> case monitor.dashboardId of
              Just dashId ->
                a_ [class_ "flex items-center gap-1 hover:text-textStrong", href_ $ "/p/" <> monitor.projectId.toText <> "/dashboards/" <> UUID.toText dashId <> "#" <> wid] do
                  faSprite_ "chart-simple" "regular" "w-3.5 h-3.5"
                  "Widget"
              Nothing -> span_ [class_ "flex items-center gap-1"] do
                faSprite_ "chart-simple" "regular" "w-3.5 h-3.5"
                "Widget"
            Nothing -> span_ [class_ "flex items-center gap-1"] do
              faSprite_ "file-lines" "regular" "w-3.5 h-3.5"
              "Log Explorer"
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
        div_ [class_ "badge badge-lg badge-ghost tabular-nums"] $ toHtml $ "Active: " <> show (V.length activeMonitors)
        div_ [class_ "badge badge-lg badge-ghost tabular-nums"] $ toHtml $ "Inactive: " <> show (V.length inactiveMonitors)
      div_ [id_ "alertsListContainer"] do
        queryMonitors_ monitors


alertTeamDeleteH :: Projects.ProjectId -> Monitors.QueryMonitorId -> UUID.UUID -> ATAuthCtx (RespHeaders Alert)
alertTeamDeleteH pid monitorId teamId = do
  _ <- Monitors.monitorRemoveTeam pid monitorId teamId
  addSuccessToast "Team removed from alert successfully" Nothing
  addRespHeaders $ AlertNoContent ""


-- Shared Alert UI Helpers

thresholdInput_ :: Text -> Text -> Text -> Bool -> Text -> [Attribute] -> Maybe Double -> Html ()
thresholdInput_ nm color lbl req inputCls extraAttrs vM = fieldset_ [class_ "fieldset flex-1 min-w-0"] do
  label_ [class_ "label flex items-center gap-1.5 text-xs mb-1"] do
    div_ [class_ $ "w-1.5 h-1.5 rounded-full " <> color] ""
    span_ [class_ "font-medium"] $ toHtml lbl
    when req $ span_ [class_ "text-textWeak"] "*"
  div_ [class_ "relative"] do
    input_ $ [type_ "number", name_ nm, id_ nm, class_ $ "input w-full " <> inputCls <> " pr-14"] <> [required_ "" | req] <> [value_ (show v) | Just v <- [vM]] <> extraAttrs
    span_ [class_ "absolute right-2 top-1/2 -translate-y-1/2 text-xs text-textWeak"] "events"


recoveryInput_ :: Text -> Text -> Text -> Text -> Maybe Double -> Html ()
recoveryInput_ nm color lbl inputCls vM = fieldset_ [class_ "fieldset flex-1 min-w-0"] do
  label_ [class_ "label flex items-center gap-1.5 text-xs mb-1"] $ div_ [class_ $ "w-1.5 h-1.5 rounded-full " <> color] "" >> span_ [class_ "font-medium"] (toHtml lbl)
  div_ [class_ "relative"] do
    input_ $ [type_ "number", name_ nm, class_ $ "input w-full " <> inputCls <> " pr-14", placeholder_ "Same as trigger"] <> [value_ (show v) | Just v <- [vM]]
    span_ [class_ "absolute right-2 top-1/2 -translate-y-1/2 text-xs text-textWeak"] "events"


directionSelect_ :: Bool -> Text -> Html ()
directionSelect_ belowSelected selectCls = fieldset_ [class_ "fieldset flex-1 min-w-0"] do
  label_ [class_ "label text-xs font-medium mb-1"] "Trigger condition"
  select_ [name_ "direction", class_ $ "select w-full " <> selectCls] do
    option_ (value_ "above" : [selected_ "" | not belowSelected]) "Above threshold"
    option_ (value_ "below" : [selected_ "" | belowSelected]) "Below threshold"


frequencySelect_ :: Int -> Bool -> Text -> Html ()
frequencySelect_ defaultMins isByos selectCls = fieldset_ [class_ "fieldset flex-1 min-w-0"] do
  label_ [class_ "label text-xs"] "Check interval"
  select_ [name_ "frequency", class_ $ "select w-full " <> selectCls] $ forM_ timeOpts mkOpt
  where
    timeOpts :: [(Int, Text)]
    timeOpts = [(1, "1 minute"), (2, "2 minutes"), (5, "5 minutes"), (10, "10 minutes"), (15, "15 minutes"), (30, "30 minutes"), (60, "1 hour"), (360, "6 hours"), (720, "12 hours"), (1440, "1 day")]
    mkOpt (m, l) = option_ ([value_ (show m <> "m")] <> [disabled_ "" | not isByos && m < 5] <> [selected_ "" | m == defaultMins]) ("Every " <> toHtml l)


collapsibleSection_ :: Text -> Maybe Text -> Html () -> Html ()
collapsibleSection_ title subtitleM content =
  details_ [class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden"] do
    summary_
      [ class_ "p-4 cursor-pointer list-none flex items-center justify-between gap-2"
      , [__|on click toggle .rotate-180 on the next <svg/> in me|]
      ]
      do
        div_ [class_ "flex items-center gap-2"] do
          span_ [class_ "font-medium text-sm"] $ toHtml title
          whenJust subtitleM $ span_ [class_ "text-xs text-textWeak"] . toHtml
        faSprite_ "chevron-down" "regular" "w-3.5 h-3.5 text-iconNeutral transition-transform duration-150"
    div_ [class_ "px-4 pb-4"] content


-- | Shared Monitor Schedule section for alert forms
-- Parameters: isByos, defaultFrequency, defaultTimeWindow, conditionType, chartTargetId
monitorScheduleSection_ :: Bool -> Int -> Int -> Maybe Text -> Maybe Text -> Html ()
monitorScheduleSection_ isByos defaultFrequency defaultTimeWindow conditionType chartTargetIdM = do
  let timeOpts :: [(Int, Text)]
      timeOpts = [(1, "minute"), (2, "2 minutes"), (5, "5 minutes"), (10, "10 minutes"), (15, "15 minutes"), (30, "30 minutes"), (60, "hour"), (360, "6 hours"), (720, "12 hours"), (1440, "day")]
      mkFreqOpt (m, l) =
        let isDisabled = not isByos && m < 5
            attrs = [value_ (show m <> "m")] <> [disabled_ "" | isDisabled] <> [selected_ "" | m == defaultFrequency] <> [term "data-tippy-content" "Upgrade to a higher plan to access this frequency" | isDisabled]
         in option_ attrs ("every " <> toHtml l)
      mkTimeOpt (m, l) = option_ ([value_ (show m <> "m")] <> [selected_ "" | m == defaultTimeWindow]) ("the last " <> toHtml l)
      isThresholdType = conditionType == Just "threshold_exceeded" || isNothing conditionType
      chartUpdateAttrM = case chartTargetIdM of
        Just chartId -> Just $ term "_" [text|on change set chart to document.getElementById('${chartId}') if chart exists then call chart.updateRollup(my.value) end|]
        Nothing -> Just [__|on change set qb to document.querySelector('query-builder') if qb exists then call qb.updateBinInQuery('timestamp', my.value) end|]
  div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden"] do
    label_ [class_ "flex items-center justify-between p-3 cursor-pointer hover:bg-fillWeak transition-colors peer"] do
      div_ [class_ "flex items-center gap-2"] do
        faSprite_ "clock" "regular" "w-4 h-4 text-iconNeutral"
        span_ [class_ "text-sm font-medium text-textStrong"] "Monitor Schedule"
      input_ [type_ "checkbox", class_ "hidden peer", checked_]
      faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral peer-checked:rotate-180 transition-transform"
    div_ [class_ "gap-3 p-3 pt-0 peer-has-[:checked]:flex hidden"] do
      fieldset_ [class_ "fieldset flex-1 min-w-0"] do
        label_ [class_ "label text-xs"] "Execute the query"
        select_ [name_ "frequency", class_ "select select-sm w-full"] $ forM_ timeOpts mkFreqOpt
      fieldset_ [class_ "fieldset flex-1 min-w-0"] do
        label_ [class_ "label text-xs"] "Include rows from"
        select_ ([name_ "timeWindow", class_ "select select-sm w-full", id_ "timeWindow"] <> maybeToList chartUpdateAttrM) $ forM_ timeOpts mkTimeOpt
      fieldset_ [class_ "fieldset flex-1 min-w-0"] do
        label_ [class_ "label text-xs"] "Notify me when"
        select_ [name_ "conditionType", class_ "select select-sm w-full", id_ "condType", [__|on change if my value == 'threshold_exceeded' then remove .hidden from #thresholds else add .hidden to #thresholds end|]] do
          option_ ([value_ "threshold_exceeded"] <> [selected_ "" | isThresholdType]) "threshold is exceeded"
          option_ ([value_ "has_matches"] <> [selected_ "" | not isThresholdType]) "the query has any results"


-- | Shared Thresholds section for alert forms
-- Parameters: chartTargetId, alertThreshold, warningThreshold, triggerLessThan, alertRecovery, warningRecovery
thresholdsSection_ :: Maybe Text -> Maybe Double -> Maybe Double -> Bool -> Maybe Double -> Maybe Double -> Html ()
thresholdsSection_ chartTargetIdM alertThresholdM warningThresholdM triggerLessThan alertRecoveryM warningRecoveryM = do
  let chartUpdateAttr = case chartTargetIdM of
        Just chartId -> term "_" [text|on input set chart to document.getElementById('${chartId}') if chart exists call chart.applyThresholds({alert: parseFloat(#alertThreshold.value), warning: parseFloat(#warningThreshold.value)}) end|]
        Nothing -> [__|on input set chart to #visualization-widget if chart exists call chart.applyThresholds({alert: parseFloat(#alertThreshold.value), warning: parseFloat(#warningThreshold.value)}) end|]
  div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden", id_ "thresholds"] do
    label_ [class_ "flex items-center justify-between p-3 cursor-pointer hover:bg-fillWeak transition-colors peer"] do
      div_ [class_ "flex items-center gap-2"] do
        faSprite_ "chart-line" "regular" "w-4 h-4 text-iconNeutral"
        span_ [class_ "text-sm font-medium text-textStrong"] "Thresholds"
      input_ [type_ "checkbox", class_ "hidden peer", checked_]
      faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral peer-checked:rotate-180 transition-transform"
    div_ [class_ "p-3 pt-0 peer-has-[:checked]:block hidden"] do
      div_ [class_ "flex flex-row gap-3"] do
        thresholdInput_ "alertThreshold" "bg-fillError-strong" "Alert threshold" True "input-sm" [chartUpdateAttr] alertThresholdM
        thresholdInput_ "warningThreshold" "bg-fillWarning-strong" "Warning threshold" False "input-sm" [chartUpdateAttr] warningThresholdM
        directionSelect_ triggerLessThan "select-sm"
      div_ [class_ "mt-3 pt-3 border-t border-strokeWeak"] do
        div_ [class_ "mb-2"] do
          label_ [class_ "text-xs font-medium text-textStrong"] "Recovery thresholds "
          span_ [class_ "text-xs text-textWeak"] "(optional)"
          p_ [class_ "text-xs text-textWeak mt-0.5"] "Alert recovers only when value crosses these thresholds"
        div_ [class_ "flex flex-row gap-3"] do
          recoveryInput_ "alertRecoveryThreshold" "bg-fillError-weak" "Alert recovery" "input-sm" alertRecoveryM
          recoveryInput_ "warningRecoveryThreshold" "bg-fillWarning-weak" "Warning recovery" "input-sm" warningRecoveryM


-- | Shared Notification Settings section for alert forms
-- Parameters: severity, subject, message, emailAll, allTeams, selectedTeamIds, formId
notificationSettingsSection_ :: Maybe Text -> Maybe Text -> Maybe Text -> Bool -> V.Vector ProjectMembers.Team -> V.Vector UUID.UUID -> Text -> Html ()
notificationSettingsSection_ severityM subjectM messageM emailAll allTeams selectedTeamIds formId = do
  let defaultSeverity = fromMaybe "Error" severityM
      defaultSubject = fromMaybe "Alert triggered" subjectM
      defaultMessage = fromMaybe "The alert threshold has been exceeded. Check the APItoolkit dashboard for details." messageM
      teamList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= x.handle, "value" AE..= x.id]) <$> allTeams
      teamName tId = case V.find (\t -> t.id == tId) allTeams of
        Just t -> t.handle
        Nothing -> "Unknown Team"
      existingTeams = decodeUtf8 $ AE.encode $ (\tId -> AE.object ["name" AE..= teamName tId, "value" AE..= tId]) <$> selectedTeamIds
  div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden"] do
    label_ [class_ "flex items-center justify-between p-3 cursor-pointer hover:bg-fillWeak transition-colors peer"] do
      div_ [class_ "flex items-center gap-2"] do
        faSprite_ "envelope" "regular" "w-4 h-4 text-iconNeutral"
        span_ [class_ "text-sm font-medium text-textStrong"] "Notification Settings"
      input_ [type_ "checkbox", class_ "hidden peer"]
      faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral peer-checked:rotate-180 transition-transform"
    div_ [class_ "p-3 pt-0 peer-has-[:checked]:block hidden"] do
      div_ [class_ "flex items-center w-full gap-2 mb-3"] do
        fieldset_ [class_ "fieldset"] do
          label_ [class_ "label text-xs font-medium mb-1"] "Severity"
          select_ [class_ "select select-sm w-28", name_ "severity"] do
            option_ [selected_ "" | defaultSeverity == "Info"] "Info"
            option_ [selected_ "" | defaultSeverity == "Error"] "Error"
            option_ [selected_ "" | defaultSeverity == "Warning"] "Warning"
            option_ [selected_ "" | defaultSeverity == "Critical"] "Critical"
        fieldset_ [class_ "fieldset w-full"] do
          label_ [class_ "label text-xs font-medium mb-1"] "Subject"
          input_ [placeholder_ "e.g. Alert triggered for high error rate", class_ "input input-sm w-full", name_ "subject", value_ defaultSubject]
      fieldset_ [class_ "fieldset w-full mb-3"] do
        label_ [class_ "label text-xs font-medium mb-1"] "Message"
        textarea_ [placeholder_ "Alert message details", class_ "textarea textarea-sm p-2 rounded-lg w-full", name_ "message", rows_ "3"] $ toHtml defaultMessage
      div_ [class_ "border-t border-strokeWeak pt-4 mt-4"] do
        div_ [class_ "mb-3"] do
          h4_ [class_ "font-medium text-sm text-textStrong mb-1"] "Recovery Notifications"
          p_ [class_ "text-xs text-textWeak"] "Continue notifications until monitor recovers"
        div_ [class_ "space-y-3"] do
          div_ [class_ "flex items-center"] do
            label_ [class_ "flex items-center gap-2 text-xs"] do
              input_ [type_ "checkbox", class_ "checkbox checkbox-sm", name_ "notifyAfterCheck"]
              span_ [] "Renotify every"
            select_ [class_ "select select-sm w-28 ml-2", name_ "notifyAfter", id_ "notifyAfterInterval"] $ zipWithM_ (\v t -> option_ (value_ v : [selected_ "" | v == "30m"]) (toHtml t)) ["10m", "20m", "30m", "1h", "6h", "24h"] ["10 mins" :: Text, "20 mins", "30 mins", "1 hour", "6 hours", "24 hours"]
          div_ [class_ "flex items-center"] do
            label_ [class_ "flex items-center gap-2 text-xs"] do
              input_ [type_ "checkbox", class_ "checkbox checkbox-sm", name_ "stopAfterCheck", [__|on change if my.checked then remove .hidden from #stopAfterInput else add .hidden to #stopAfterInput end|]]
              span_ [] "Stop after"
            div_ [class_ "flex items-center gap-1.5 ml-2 hidden", id_ "stopAfterInput"] do
              input_ [type_ "number", class_ "input input-sm w-16", value_ "5", name_ "stopAfter", min_ "1", max_ "100"]
              span_ [class_ "text-xs text-textWeak"] "occurrences"
      div_ [class_ "border-t border-strokeWeak pt-4 mt-4"] do
        div_ [class_ "flex flex-col gap-1"] do
          span_ [class_ "text text-sm"] "Teams"
          span_ [class_ "text-xs text-textWeak"] "Add teams to notify (if no team is added, project level notification channels will be used)"
        textarea_ [class_ "input max-h-max w-full mt-2 resize-none", name_ "teams"] ""
      script_
        [text|
        window.addEventListener('DOMContentLoaded', () => {
          const tagify = createTagify('#${formId} textarea[name="teams"]', {
            tagTextProp: 'name',
            whitelist: $teamList,
          });
          tagify.addTags($existingTeams);
        })
        const getSelectedTeams = () => {
          return tagify.value.map(item => item.value);
        }
      |]
      div_ [class_ "flex items-center gap-2 mt-4 pt-3 border-t border-strokeWeak"] do
        label_ [class_ "label cursor-pointer flex items-center gap-2"] do
          input_ $ [type_ "checkbox", class_ "checkbox checkbox-sm", name_ "recipientEmailAll", value_ "true"] ++ [checked_ | emailAll]
          span_ [class_ "text-sm"] "Send to all team members"
        span_ [class_ "tooltip", term "data-tip" "Configure specific recipients in alert settings after creation"] $ faSprite_ "circle-info" "regular" "w-3.5 h-3.5 text-iconNeutral"
