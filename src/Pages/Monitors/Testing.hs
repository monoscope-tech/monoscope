module Pages.Monitors.Testing (
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

import Control.Lens (view, _2, _3)
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx
import Models.Apis.Integrations qualified as Slack
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.ProjectMembers (Team (discord_channels, slack_channels))
import Models.Projects.ProjectMembers qualified as ManageMembers
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Bots.Discord qualified as Discord
import Pages.Bots.Slack qualified as Slack
import Pages.Bots.Slack qualified as SlackP
import Pages.Bots.Utils (Channel (channelId, channelName))
import Pkg.Components.TimePicker qualified as TimePicker
import Pages.LogExplorer.Log (virtualTable)
import Pages.Projects (TBulkActionForm (..))
import Pkg.Components.Table (BulkAction (..), Config (..), Features (..), SearchMode (..), TabFilter (..), TabFilterOpt (..), Table (..), TableRows (..), ZeroState (..), col, withAttrs)
import Pkg.Components.Widget (Widget (..))
import Pkg.Components.Widget qualified as Widget
import Relude hiding (ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, addSuccessToast, redirectCS)
import Text.Time.Pretty (prettyTimeAuto)
import Utils (checkFreeTierExceeded, faSprite_, formatWithCommas, toUriStr)


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
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  alerts <- Monitors.getAlertsByTeamHandle pid teamId
  currTime <- Time.currentTime
  teamMap <- buildTeamMap pid
  let alerts' = V.fromList $ map (toUnifiedMonitorItem teamMap pid currTime) alerts

  addRespHeaders $ TableRows [] alerts' Nothing False Nothing Nothing Nothing


alertBulkActionH :: Projects.ProjectId -> Text -> TBulkActionForm -> ATAuthCtx (RespHeaders (PageCtx (Table UnifiedMonitorItem)))
alertBulkActionH pid action form = do
  let monitorIds = Monitors.QueryMonitorId <$> form.itemId
  unless (null monitorIds) $ case action of
    "deactivate" -> void $ Monitors.monitorDeactivateByIds monitorIds
    "reactivate" -> void $ Monitors.monitorReactivateByIds monitorIds
    "mute" -> void $ Monitors.monitorMuteByIds Nothing monitorIds
    "unmute" -> void $ Monitors.monitorUnmuteByIds monitorIds
    "resolve" -> void $ Monitors.monitorResolveByIds monitorIds
    "delete" -> void $ Monitors.monitorSoftDeleteByIds monitorIds
    _ -> pass
  let filterTab = case action of
        "deactivate" -> "Inactive"
        "reactivate" -> "Active"
        "delete" -> "Active"
        _ -> "Active"
  unifiedMonitorsGetH pid (Just filterTab) Nothing


-- | Unified handler for monitors endpoint showing both alerts and multi-step monitors
unifiedMonitorsGetH
  :: Projects.ProjectId
  -> Maybe Text -- filter
  -> Maybe Text -- since
  -> ATAuthCtx (RespHeaders (PageCtx (Table UnifiedMonitorItem)))
unifiedMonitorsGetH pid filterTM sinceM = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  currTime <- Time.currentTime

  -- Parse filter
  let filterType = fromMaybe "Active" filterTM

  -- Fetch alerts (query monitors)
  allAlertsList <- Monitors.queryMonitorsAll pid
  teamMap <- buildTeamMap pid
  let allAlerts = V.fromList allAlertsList
      activeAlerts = V.filter (isNothing . (.deactivatedAt)) allAlerts
      inactiveAlerts = V.filter (isJust . (.deactivatedAt)) allAlerts

  alerts <- case filterType of
    "Active" -> pure activeAlerts
    _ -> pure inactiveAlerts

  -- Convert to unified items, sorted with alerting/muted monitors first
  let sortKey i = (view _3 $ statusInfo i.currentStatus, isNothing i.mutedUntil)
      allItems = V.fromList $ sortOn sortKey $ V.toList $ V.map (toUnifiedMonitorItem teamMap pid currTime) alerts

  let totalInactive = V.length inactiveAlerts

  freeTierExceeded <- checkFreeTierExceeded pid project.paymentPlan

  let currentURL = "/p/" <> pid.toText <> "/monitors?"
  let monitorsTable =
        Table
          { config = def{elemID = "monitorsListForm", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0}
          , columns =
              [ col "Name" renderNameCol & withAttrs [class_ "min-w-0"]
              , col "Teams" (\i -> forM_ i.teamBadges \(_, handle) -> span_ [class_ "badge badge-sm badge-neutral mr-1"] $ toHtml handle) & withAttrs [class_ "w-48"]
              , col "Schedule" (\i -> span_ [class_ "text-sm text-textWeak whitespace-nowrap"] $ toHtml i.schedule) & withAttrs [class_ "w-28"]
              , col "Last Run" renderLastRunCol & withAttrs [class_ "w-28"]
              , col "Threshold" renderThresholdCol & withAttrs [class_ "w-40"]
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
                        { icon = "empty-set"
                        , title = "No monitors configured yet"
                        , description = "Create alerts"
                        , actionText = "Create alert"
                        , destination =
                            Left $ "/p/" <> pid.toText <> "/log_explorer#create-alert-toggle"
                        }
                }
          }

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
          , navTabs =
              Just
                $ toHtml
                $ TabFilter
                  { current = filterType
                  , currentURL
                  , clientSide = False
                  , options =
                      [ TabFilterOpt{name = "Active", count = Just $ V.length activeAlerts, targetId = Nothing}
                      , TabFilterOpt{name = "Inactive", count = Just totalInactive, targetId = Nothing}
                      ]
                  }
          }

  addRespHeaders $ PageCtx bwconf monitorsTable


-- Table column renderers

renderNameCol :: UnifiedMonitorItem -> Html ()
renderNameCol item = do
  let base = monitorBase item
      (dotColor, displayName, _) = statusInfo item.currentStatus
      isMuted = isJust item.mutedUntil
  div_ [class_ "flex flex-col gap-1 py-0.5"] do
    div_ [class_ "flex items-center gap-2"] do
      span_ [class_ $ "inline-block w-2 h-2 rounded-full shrink-0 " <> dotColor, term "data-tippy-content" $ bool "Inactive" "Active" isActive] ""
      a_ [href_ $ base <> "/" <> item.monitorId <> "/overview", class_ "text-sm font-medium text-textStrong hover:text-textBrand transition-colors"] $ toHtml $ if T.null item.title then "(Untitled)" else item.title
      when (item.currentStatus /= Monitors.MSNormal) $ statusBadge_ False displayName
      whenJust item.mutedUntil \until' ->
        let muteLabel = mutedLabel item.now until'
         in span_ [class_ "badge badge-sm badge-ghost gap-1", term "data-tippy-content" muteLabel] do
              faSprite_ "bell-slash" "regular" "h-3 w-3"
              toHtml muteLabel
      div_ [class_ "flex gap-1 items-center opacity-0 group-hover/row:opacity-100 has-[:focus-within]:opacity-100 transition-opacity"] do
        inlineBtn (bool "Activate" "Deactivate" isActive) (bool "play" "pause" isActive) (hxPost_ $ base <> "/alerts/" <> item.monitorId <> "/toggle_active") []
        if isMuted
          then inlineBtn "Unmute" "bell" (hxPost_ $ base <> "/alerts/" <> item.monitorId <> "/unmute") []
          else muteDropdown_ item.monitorId (base <> "/alerts/" <> item.monitorId <> "/mute")
        when (item.currentStatus /= Monitors.MSNormal) $ inlineBtn "Resolve" "check" (hxPost_ $ base <> "/alerts/" <> item.monitorId <> "/resolve") []
        inlineBtn "Delete" "trash" (hxDelete_ $ base <> "/alerts/" <> item.monitorId) [hxConfirm_ "Are you sure you want to delete this monitor?"]
    div_ [class_ "flex items-center gap-1.5"] do
      span_ [class_ "text-xs text-textWeak font-mono line-clamp-2 bg-fillWeaker border border-strokeWeak rounded px-1.5 py-0.5", term "data-tippy-content" item.details.query] $ toHtml item.details.query
  where
    isActive = item.status == "Active"
    inlineBtn tip icon hxAction extraAttrs =
      button_ ([type_ "button", term "data-tippy-content" tip, class_ "cursor-pointer hover:text-textBrand transition-colors tap-target", hxSwap_ "none", hxAction] <> extraAttrs)
        $ faSprite_ icon "regular" "h-3.5 w-3.5"


muteDropdown_ :: Text -> Text -> Html ()
muteDropdown_ monitorId muteUrl = do
  let popId = "mute-pop-" <> monitorId
  div_ [class_ "inline-block"] do
    button_
      [ type_ "button"
      , term "data-tippy-content" "Mute"
      , class_ "cursor-pointer hover:text-textBrand transition-colors tap-target"
      , term "popovertarget" popId
      , style_ $ "anchor-name: --anchor-" <> popId
      ]
      $ faSprite_ "bell-slash" "regular" "h-3.5 w-3.5"
    div_
      [ id_ popId
      , term "popover" "auto"
      , class_ "dropdown dropdown-start menu bg-bgRaised p-1 text-sm border border-strokeWeak z-50 min-w-36 rounded-md shadow-lg mt-1"
      , style_ $ "position-try: flip-block; position-anchor: --anchor-" <> popId
      ]
      do
        span_ [class_ "px-3 py-1 text-xs font-medium text-textWeak"] "Mute for..."
        forM_ muteDurations \(mins, label) ->
          button_ [type_ "button", class_ "px-3 py-1.5 text-sm text-left hover:bg-fillWeaker rounded cursor-pointer w-full", hxPost_ $ muteUrl <> "?duration=" <> show mins, hxSwap_ "none"] $ toHtml label
        button_ [type_ "button", class_ "px-3 py-1.5 text-sm text-left hover:bg-fillWeaker rounded cursor-pointer w-full border-t border-strokeWeak", hxPost_ muteUrl, hxSwap_ "none"] "Indefinitely"


muteDurations :: [(Int, Text)]
muteDurations = [(60, "1 hour"), (240, "4 hours"), (480, "8 hours"), (1440, "1 day"), (10080, "1 week")]


mutedLabel :: UTCTime -> UTCTime -> Text
mutedLabel now until'
  | diffMins > 525600 = "Muted indefinitely"
  | diffMins >= 1440 = "Muted \xb7 " <> show (diffMins `div` 1440) <> "d left"
  | diffMins >= 60 = "Muted \xb7 " <> show (diffMins `div` 60) <> "h left"
  | otherwise = "Muted \xb7 " <> show (max 1 diffMins) <> "m left"
  where
    diffMins = round (diffUTCTime until' now / 60) :: Int


monitorBase :: UnifiedMonitorItem -> Text
monitorBase item = "/p/" <> item.projectId <> "/monitors"


-- (dotColor, displayName, sortOrder) for each monitor status
statusInfo :: Monitors.MonitorStatus -> (Text, Text, Int)
statusInfo = \case
  Monitors.MSAlerting -> ("bg-red-500", "Alerting", 0)
  Monitors.MSWarning -> ("bg-yellow-500", "Warning", 1)
  Monitors.MSNormal -> ("bg-green-500", "Normal", 2)


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


-- Inline action handlers — perform action, toast, redirect back to monitors page
monitorActionH :: ([Monitors.QueryMonitorId] -> ATAuthCtx Int64) -> Text -> Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders (Html ()))
monitorActionH action msg pid monitorId = do
  void $ action [monitorId]
  addSuccessToast msg Nothing
  redirectCS $ "/p/" <> pid.toText <> "/monitors"
  addRespHeaders ""


alertMuteH :: Projects.ProjectId -> Monitors.QueryMonitorId -> Maybe Int -> ATAuthCtx (RespHeaders (Html ()))
alertMuteH pid monitorId durationMinsM = do
  void $ Monitors.monitorMuteByIds durationMinsM [monitorId]
  let msg = maybe "Monitor muted indefinitely" (\_ -> "Monitor muted") durationMinsM
  addSuccessToast msg Nothing
  redirectCS $ "/p/" <> pid.toText <> "/monitors"
  addRespHeaders ""


alertUnmuteH, alertResolveH, alertDeleteH :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders (Html ()))
alertUnmuteH = monitorActionH Monitors.monitorUnmuteByIds "Monitor unmuted"
alertResolveH = monitorActionH Monitors.monitorResolveByIds "Monitor resolved"
alertDeleteH = monitorActionH Monitors.monitorSoftDeleteByIds "Monitor deleted"


renderLastRunCol :: UnifiedMonitorItem -> Html ()
renderLastRunCol item = span_ [class_ "text-sm text-textWeak whitespace-nowrap"] $ maybe "Never" (toHtml . prettyTimeAuto item.now) item.lastRun


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
          , triggerDirection = if alert.triggerLessThan then "below" else "above"
          , visualizationType = alert.visualizationType
          }
    , teamBadges = mapMaybe (\tid -> (UUID.toText tid,) <$> Map.lookup tid teamMap) $ V.toList alert.teams
    }



-- | Status badge component with size option
-- Includes icons for all statuses to ensure accessibility (not relying on color alone)
statusBadge_ :: Bool -> Text -> Html ()
statusBadge_ isLarge status = do
  let (badgeClass, icon) = case status of
        "Passing" -> ("badge-success", "check")
        "Failing" -> ("badge-error", "xmark")
        "Active" -> ("badge-success", "circle-check")
        "Inactive" -> ("badge-ghost", "circle-pause")
        "Alerting" -> ("badge-error", "bell-exclamation")
        "Warning" -> ("badge-warning", "triangle-exclamation")
        "alert" -> ("badge-error", "bell-exclamation")
        "Healthy" -> ("badge-success", "heart-pulse")
        "Pending" -> ("badge-ghost", "clock")
        "NoData" -> ("badge-ghost", "circle-question")
        _ -> ("badge-ghost", "circle")
      sizeClass = if isLarge then "" else "badge-sm"
      iconSize = if isLarge then "h-4 w-4" else "h-3 w-3"
  span_ [class_ $ sizeClass <> " " <> badgeClass <> " gap-1 badge"] do
    faSprite_ icon "regular" iconSize
    toHtml status


-- | Unified monitor overview handler that works for both alerts and collections
unifiedMonitorOverviewH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
unifiedMonitorOverviewH pid monitorId = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  currTime <- Time.currentTime
  freeTierExceeded <- checkFreeTierExceeded pid project.paymentPlan

  -- Try to find as alert first
  alertM <- case UUID.fromText monitorId of
    Just uuid -> do
      alerts <- Monitors.queryMonitorsById (V.singleton $ Monitors.QueryMonitorId uuid)
      pure $ listToMaybe alerts
    Nothing -> pure Nothing

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Alerts Overview"
          , prePageTitle = Just "Alerts"
          , menuItem = Just "Alerts"
          , docsLink = Just "https://monoscope.tech/docs/monitors/"
          , freeTierExceeded = freeTierExceeded
          , config = appCtx.config
          }

  case alertM of
    Just alert -> do
      teams <- ManageMembers.getTeamsById pid alert.teams
      slackDataM <- Slack.getProjectSlackData pid
      channels <- case slackDataM of
        Just slackData -> do
          channels' <- SlackP.getSlackChannels appCtx.env.slackBotToken slackData.teamId
          case channels' of
            Just chs -> return chs.channels
            Nothing -> return []
        Nothing -> return []
      discordDataM <- Slack.getDiscordDataByProjectId pid
      discordChannels <- case discordDataM of
        Just discordData -> Discord.getDiscordChannels appCtx.env.discordBotToken discordData.guildId
        Nothing -> return []
      let findChannel xx x = fromMaybe x (find (\c -> c.channelId == x) xx >>= (\a -> Just a.channelName))
      let teams' = (\x -> x{slack_channels = findChannel channels <$> x.slack_channels, discord_channels = (\xx -> fromMaybe xx (find (\c -> c.channelId == xx) discordChannels >>= (\a -> Just a.channelName))) <$> x.discord_channels}) <$> teams
      addRespHeaders $ PageCtx bwconf $ unifiedOverviewPage pid alert currTime (V.fromList teams') slackDataM discordDataM
    _ -> addRespHeaders $ PageCtx bwconf $ div_ [class_ "p-6 text-center"] "Alert not found"


-- | Unified overview page that handles both monitor types
unifiedOverviewPage :: Projects.ProjectId -> Monitors.QueryMonitorEvaled -> UTCTime -> V.Vector ManageMembers.Team -> Maybe Slack.SlackData -> Maybe Slack.DiscordData -> Html ()
unifiedOverviewPage pid alert currTime teams slackDataM discordDataM = do
  section_ [class_ "pt-2 mx-auto px-4 w-full flex flex-col gap-4 h-full overflow-y-auto"] do
    -- Header section
    div_ [class_ "flex justify-between items-start gap-4"] do
      div_ [class_ "flex flex-col gap-2"] do
        div_ [class_ "flex items-center gap-3"] do
          statusBadge_ True displayName
          h1_ [class_ "text-2xl font-semibold text-textStrong"] $ toHtml $ bool alert.alertConfig.title "(Untitled)" (T.null alert.alertConfig.title)
        div_ [class_ "flex flex-wrap gap-2 items-center"] do
          metadataChip_ "shield-halved" $ "Severity: " <> alert.alertConfig.severity
          metadataChip_ "clock" $ "Every " <> show alert.checkIntervalMins <> " min"
          metadataChip_ "calendar" $ "Created " <> toText (prettyTimeAuto currTime alert.createdAt)
          whenJust alert.mutedUntil \until' ->
            span_ [class_ "badge badge-sm badge-warning gap-1"] do
              faSprite_ "bell-slash" "regular" "h-3 w-3"
              toHtml $ mutedLabel currTime until'

      -- Action buttons
      div_ [class_ "flex gap-2 items-center shrink-0"] do
        case alert.mutedUntil of
          Just _ -> button_ [class_ "btn btn-sm btn-outline", hxPost_ $ muteBase <> "/unmute"] do
            faSprite_ "bell" "regular" "h-4 w-4"
            "Unmute"
          Nothing -> muteDropdown_ alert.id.toText (muteBase <> "/mute")
        when (alert.currentStatus `elem` [Monitors.MSAlerting, Monitors.MSWarning]) do
          button_ [class_ "btn btn-sm btn-outline btn-success", hxPost_ $ muteBase <> "/resolve"] do
            faSprite_ "check" "regular" "h-4 w-4"
            "Resolve"
        let isInactive = isJust alert.deactivatedAt
        button_ [class_ "btn btn-sm btn-outline", hxPost_ $ muteBase <> "/toggle_active"] do
          faSprite_ (bool "circle-pause" "circle-play" isInactive) "regular" "h-4 w-4"
          bool "Deactivate" "Activate" isInactive
        a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer?alert=" <> alert.id.toText <> "&query=" <> alert.logQuery, class_ "btn btn-sm btn-primary"] do
          faSprite_ "pen-to-square" "regular" "h-4 w-4"
          "Edit Alert"

    -- Stats section
    div_ [class_ "border-b border-strokeWeak pb-3"] $ alertStats_ alert currTime

    -- Content tabs
    tabbedSection_ "monitor-tabs" [("Query & Visualization", alertQueryTab_ pid alert), ("Execution History", monitorHistoryTab_ pid alert.id), ("Notification Channels", alertNotificationsTab_ alert teams)]
  where
    displayName = bool (view _2 $ statusInfo alert.currentStatus) "Inactive" (isJust alert.deactivatedAt)
    muteBase = "/p/" <> pid.toText <> "/monitors/alerts/" <> alert.id.toText
    metadataChip_ icon label = span_ [class_ "inline-flex items-center gap-1.5 text-xs text-textWeak bg-fillWeaker rounded-full px-2.5 py-1"] do
      faSprite_ icon "regular" "h-3 w-3"
      toHtml @Text label


-- | Reusable tabbed section component
tabbedSection_ :: Text -> [(Text, Html ())] -> Html ()
tabbedSection_ containerId tabs = do
  div_ [role_ "tablist", class_ "w-full", id_ containerId] do
    div_ [class_ "w-full flex"] do
      forM_ (zip [0 ..] tabs) $ \(idx, (label, _)) -> do
        let tabId = containerId <> "-tab-" <> show idx
        button_
          [ class_ $ "cursor-pointer shrink-0 tab-btn  tab-box  text-sm font-medium p-2 text-textWeak border-b-2 border-b-transparent" <> if idx == 0 then " t-tab-active" else ""
          , role_ "tab"
          , term "aria-label" label
          , onclick_ $ "navigateTab(this, '#" <> tabId <> "', '#" <> containerId <> "')"
          ]
          $ toHtml label
    forM_ (zip [0 ..] tabs) $ \(idx, (_, content)) -> do
      let tabId = containerId <> "-tab-" <> show idx
      div_ [role_ "tabpanel", class_ $ "overflow-y-auto t-tab-content" <> if idx /= 0 then " hidden" else "", id_ tabId] do
        content

  -- Add navigation script once
  script_
    [text|
    function navigateTab(tab, contentId, containerId) { 
        const container = document.querySelector(containerId); 
        container.querySelectorAll('.t-tab-active').forEach(t => t.classList.remove('t-tab-active')); 
        tab.classList.add('t-tab-active'); 
        container.querySelectorAll('.t-tab-content').forEach(c => c.classList.add('hidden')); 
        document.querySelector(contentId).classList.remove('hidden'); 
      }
    |]


-- | Shared history tab
monitorHistoryTab_ :: Projects.ProjectId -> Monitors.QueryMonitorId -> Html ()
monitorHistoryTab_ pid alertId = do
  let query = "kind==\"alert\" and parent_id==\"" <> alertId.toText <> "\""
      initialUrl = "/p/" <> pid.toText <> "/log_explorer?json=true&query=" <> toUriStr query
  div_ [class_ "mt-2 p-2 border border-strokeWeak rounded-lg"] do
    virtualTable pid (Just initialUrl) Nothing


-- | Alert details as a compact key-value row with dividers
alertStats_ :: Monitors.QueryMonitorEvaled -> UTCTime -> Html ()
alertStats_ alert currTime = do
  div_ [class_ "flex flex-wrap items-center gap-y-2 py-2 text-sm"] do
    detailItem_ "Current Value" $ span_ [class_ $ statusColor <> " tabular-nums"] $ toHtml $ formatWithCommas alert.evalResult
    sep_
    detailItem_ "Threshold" $ toHtml $ direction <> " " <> formatWithCommas alert.alertThreshold
    whenJust alert.warningThreshold \w -> sep_ >> detailItem_ "Warning" (toHtml $ direction <> " " <> formatWithCommas w)
    whenJust alert.alertRecoveryThreshold \r -> sep_ >> detailItem_ "Recovery" (toHtml $ formatWithCommas r)
    sep_
    detailItem_ "Last Evaluated" $ toHtml $ prettyTimeAuto currTime alert.lastEvaluated
    sep_
    detailItem_ "Last Triggered" $ toHtml $ maybe "Never" (toText . prettyTimeAuto currTime) alert.alertLastTriggered
    sep_
    detailItem_ "Notifications" $ span_ [class_ "tabular-nums"] $ toHtml $ show @Text alert.notificationCount
    whenJust alert.renotifyIntervalMins \mins -> sep_ >> detailItem_ "Renotify" (toHtml $ show @Text mins <> " min")
    whenJust alert.stopAfterCount \count -> sep_ >> detailItem_ "Stop After" (toHtml $ show @Text count)
  where
    direction = bool ">" "<" alert.triggerLessThan
    statusColor = case alert.currentStatus of
      Monitors.MSAlerting -> "text-red-500 font-semibold"
      Monitors.MSWarning -> "text-yellow-600 font-semibold"
      Monitors.MSNormal -> "text-green-600 font-semibold"
    sep_ = div_ [class_ "w-px h-4 bg-strokeWeak mx-3"] ""
    detailItem_ label val = div_ [class_ "flex items-center gap-1.5"] do
      _ <- span_ [class_ "text-textWeak"] label
      span_ [class_ "text-textStrong font-medium"] val


-- | Alert query tab content
alertQueryTab_ :: Projects.ProjectId -> Monitors.QueryMonitorEvaled -> Html ()
alertQueryTab_ pid alert = do
  div_ [class_ "pt-4 pb-3 flex flex-col gap-4"] do
    -- Query + timepicker row
    div_ [class_ "flex items-start justify-between gap-4"] do
      div_ [class_ "bg-fillWeaker rounded-lg px-3 py-2 border border-strokeWeak min-w-0 flex-1"] do
        pre_ [class_ "text-sm font-mono text-textWeak overflow-x-auto whitespace-pre-wrap"] $ toHtml alert.logQuery
      div_ [class_ "flex items-center shrink-0"] do
        TimePicker.timepicker_ Nothing Nothing Nothing
        TimePicker.refreshButton_

    -- Chart
    div_ [class_ "border border-strokeWeak rounded-lg p-3", style_ "aspect-ratio: 5 / 2; max-height: 420px;"] do
      Widget.widget_
        $ (def :: Widget)
          { Widget.wType = Widget.mapChatTypeToWidgetType alert.visualizationType
          , Widget.query = Just alert.logQuery
          , Widget.title = Just "Alert Query Visualization"
          , Widget.standalone = Just True
          , Widget._projectId = Just pid
          , Widget.layout = Just (def{Widget.w = Just 12, Widget.h = Just 6})
          , Widget.alertThreshold = Just alert.alertThreshold
          , Widget.warningThreshold = alert.warningThreshold
          , Widget.showThresholdLines = Just "always"
          }


-- | Alert notifications tab content (copied from Alerts module)
alertNotificationsTab_ :: Monitors.QueryMonitorEvaled -> V.Vector ManageMembers.Team -> Html ()
alertNotificationsTab_ alert teams = do
  div_ [class_ "pt-6 pb-3"] do
    -- Email recipients
    div_ [class_ "mb-6"] do
      h4_ [class_ "text-base font-medium text-textStrong mb-2 flex items-center"] $ faSprite_ "users" "regular" "h-4 w-4 mr-2" >> "Teams"
      when (null teams) $ do
        div_ [class_ "text-sm text-textWeak"] "No teams configured for this alert."
        div_ [class_ "pt-2 flex items-center gap-1"] do
          span_ [class_ "text-sm text-textWeak"] "Project level notification integrations will be used."
          a_ [href_ $ "/p/" <> alert.projectId.toText <> "/integrations", class_ "text-sm text-textBrand hover:underline"] "Configure integrations"
      unless (V.null teams) $ do
        div_ [class_ "flex flex-wrap gap-4 mb-4"] do
          forM_ teams $ \team -> do
            div_ [class_ "flex flex-col border rounded-lg gap-4 border-strokeWeak p-6 relative w-96", id_ team.handle] do
              button_
                [ type_ "button"
                , class_ "absolute top-3 cursor-pointer right-3 text-iconNeutral hover:text-iconBrand transition-colors"
                , term "data-tippy-content" "Remove team"
                , hxDelete_ $ "/p/" <> alert.projectId.toText <> "/monitors/alerts/" <> alert.id.toText <> "/teams/" <> UUID.toText team.id
                , hxTarget_ $ "#" <> team.handle
                , hxSwap_ "outerHTML"
                ]
                do
                  faSprite_ "trash" "regular" "h-3 w-3"
              span_ [class_ "text-sm font-medium"] $ toHtml team.name
              forM_ team.notify_emails $ \email ->
                div_ [class_ "flex items-center gap-2"] do
                  span_ [class_ "text-sm text-textWeak"] $ faSprite_ "envelope" "regular" "h-3 w-3 mr-2 text-iconNeutral" >> toHtml email
              forM_ team.slack_channels $ \channel ->
                div_ [class_ "flex items-center gap-2"] do
                  span_ [class_ "text-sm text-textWeak"] $ faSprite_ "slack" "solid" "h-3 w-3 mr-2 text-iconNeutral" >> toHtml ("#" <> channel)
              forM_ team.discord_channels $ \channel ->
                div_ [class_ "flex items-center gap-2"] do
                  span_ [class_ "text-sm text-textWeak"] $ faSprite_ "discord" "brand" "h-3 w-3 mr-2 text-iconNeutral" >> toHtml ("#" <> channel)

    -- Message template
    div_ [class_ "pt-6 pb-3"] do
      h4_ [class_ "font-medium text-textStrong mb-2"] "Notification Template"
      div_ [class_ "rounded-lg py-2 space-y-2"] do
        div_ [class_ "flex items-center gap-1"] do
          span_ [class_ "text-sm font-medium text-textStrong"] "Subject: "
          span_ [class_ "text-sm text-textWeak"] $ toHtml alert.alertConfig.subject
        div_ [class_ "flex flex-col gap-1"] do
          span_ [class_ "text-sm font-medium text-textStrong"] "Message"
          p_ [class_ "text-sm text-textWeak mt-1"] $ toHtml alert.alertConfig.message
