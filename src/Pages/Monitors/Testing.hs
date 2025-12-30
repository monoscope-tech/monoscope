module Pages.Monitors.Testing (
  MonitorType (..),
  UnifiedMonitorItem (..),
  unifiedMonitorsGetH,
  unifiedMonitorOverviewH,
  statusBadge_,
  teamAlertsGetH,
)
where

import Data.Default (def)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Fmt.Internal.Core (fmt)
import Fmt.Internal.Numeric (commaizeF)
import Lucid
import Lucid.Htmx
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.Slack qualified as Slack
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
import Pages.Components (statBox_)
import Pages.LogExplorer.Log (virtualTable)
import Pkg.Components.Table (Config (..), Features (..), SearchMode (..), TabFilter (..), TabFilterOpt (..), Table (..), TableRows (..), ZeroState (..), col, withAttrs)
import Pkg.Components.Widget (Widget (..))
import Pkg.Components.Widget qualified as Widget
import Relude hiding (ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Text.Time.Pretty (prettyTimeAuto)
import Utils (checkFreeTierExceeded, faSprite_, toUriStr)


-- | Types for unified monitor view
data MonitorType = MTAlert
  deriving (Eq, Show)


data UnifiedMonitorItem = UnifiedMonitorItem
  { monitorType :: MonitorType
  , monitorId :: Text
  , projectId :: Text
  , title :: Text
  , status :: Text -- "Passing", "Failing", "Active", "Inactive"
  , schedule :: Text
  , lastRun :: Maybe UTCTime
  , createdAt :: UTCTime
  , now :: UTCTime
  , tags :: [Text]
  , hosts :: [Text]
  , details :: UnifiedMonitorDetails
  }


data UnifiedMonitorDetails
  = AlertDetails
  { query :: Text
  , alertThreshold :: Int
  , warningThreshold :: Maybe Int
  , triggerDirection :: Text -- "above" or "below"
  , lastEvaluated :: Maybe UTCTime
  , alertLastTriggered :: Maybe UTCTime
  , visualizationType :: Text
  }


teamAlertsGetH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders (TableRows UnifiedMonitorItem))
teamAlertsGetH pid teamId = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  alerts <- Monitors.getAlertsByTeamHandle pid teamId
  currTime <- Time.currentTime
  let alerts' = V.fromList $ map (toUnifiedMonitorItem pid currTime) alerts

  addRespHeaders $ TableRows [] alerts' Nothing False Nothing Nothing Nothing


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
  let allAlerts = V.fromList allAlertsList
      activeAlerts = V.filter (isNothing . (.deactivatedAt)) allAlerts
      inactiveAlerts = V.filter (isJust . (.deactivatedAt)) allAlerts

  alerts <- case filterType of
    "Active" -> pure activeAlerts
    _ -> pure inactiveAlerts

  -- Convert to unified items using the generic converter
  let allItems = V.map (toUnifiedMonitorItem pid currTime) alerts

  let totalInactive = V.length inactiveAlerts

  freeTierExceeded <- checkFreeTierExceeded pid project.paymentPlan

  let currentURL = "/p/" <> pid.toText <> "/monitors?"
  let monitorsTable =
        Table
          { config = def{elemID = "monitorsListForm", addPadding = True}
          , columns =
              [ col "" renderMonitorIcon & withAttrs [class_ "shrink-0"]
              , col "" renderMonitorContent & withAttrs [class_ "w-full"]
              ]
          , rows = allItems
          , features =
              def
                { search = Just ClientSide
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


-- | Convert any monitor to unified item using a more generic approach
toUnifiedMonitorItem :: Projects.ProjectId -> UTCTime -> Monitors.QueryMonitor -> UnifiedMonitorItem
toUnifiedMonitorItem pid currTime = alertToUnifiedItem pid
  where
    -- \| Convert alert to unified monitor item
    alertToUnifiedItem _ alert =
      let isActive = isNothing alert.deactivatedAt
          config = alert.alertConfig
       in UnifiedMonitorItem
            { monitorType = MTAlert
            , monitorId = alert.id.toText
            , projectId = pid.toText
            , title = config.title
            , status = if isActive then "Active" else "Inactive"
            , schedule = "every " <> show alert.checkIntervalMins <> " min"
            , lastRun = Just alert.lastEvaluated
            , createdAt = alert.createdAt
            , tags = []
            , hosts = []
            , now = currTime
            , details =
                AlertDetails
                  { query = alert.logQuery
                  , alertThreshold = alert.alertThreshold
                  , warningThreshold = alert.warningThreshold
                  , triggerDirection = if alert.triggerLessThan then "below" else "above"
                  , lastEvaluated = Just alert.lastEvaluated
                  , alertLastTriggered = alert.alertLastTriggered
                  , visualizationType = alert.visualizationType
                  }
            }


-- | Render monitor icon column
renderMonitorIcon :: UnifiedMonitorItem -> Html ()
renderMonitorIcon item = do
  div_ [class_ "mt-2 pl-4 shrink-0"] do
    div_ [class_ $ "w-10 h-10 rounded-lg flex items-center justify-center " <> typeColorClass] do
      faSprite_ typeIcon "regular" "h-5 w-5"
  where
    (typeIcon, _, typeColorClass) = ("bell", "Alert", "bg-fillWarning-weak text-iconWarning")


-- | Render monitor content column
renderMonitorContent :: UnifiedMonitorItem -> Html ()
renderMonitorContent item = do
  div_ [class_ "w-full flex flex-col gap-2 shrink-1"] do
    -- Title and tags row
    div_ [class_ "flex gap-10 items-center"] do
      a_ [href_ detailsUrl, class_ "font-medium text-textStrong text-base hover:text-textBrand transition-colors"] $ toHtml $ if T.null item.title then "(Untitled)" else item.title
      div_ [class_ "flex gap-1 items-center text-sm"] do
        -- Monitor type badge
        span_ [class_ "badge badge-sm badge-ghost"] $ toHtml typeLabel
        -- Tags
        forM_ item.tags $ \tag -> do
          span_ [class_ "badge badge-sm badge-neutral"] $ toHtml tag

    -- Details row
    div_ [class_ "w-full flex"] do
      div_ [class_ "flex flex-col gap-4 w-1/3"] do
        -- Hosts or query preview
        div_ [class_ "flex gap-2 items-center w-full"] do
          case item.details of
            AlertDetails{query} -> do
              span_ [class_ "text-sm text-textWeak p-1 bg-fillWeak monospace truncate", term "data-tippy-content" query] $ toHtml $ T.take 50 query

        -- Status and schedule
        div_ [class_ "flex gap-4 w-full items-center"] do
          statusBadge item.status
          div_ [class_ "flex items-center shrink-0 gap-1"] do
            faSprite_ "clock" "regular" "h-4 w-4"
            span_ [class_ "shrink-0 text-sm"] $ toHtml item.schedule

      div_ [class_ "w-2/3 flex justify-between gap-10 items-center"] do
        div_ [class_ "flex gap-4 items-center"] do
          -- Created date
          div_ [class_ "flex gap-1.5 items-center"] do
            faSprite_ "calendar" "regular" "h-6 w-6 fill-none"
            div_ [class_ "flex flex-col"] do
              span_ [class_ "text-textWeak text-xs"] "Created"
              span_ [class_ "text-sm font-medium text-textStrong"] $ toHtml $ prettyTimeAuto item.now item.createdAt

          -- Last run
          div_ [class_ "flex gap-1.5 items-center"] do
            faSprite_ "play" "regular" "h-6 w-6 fill-none text-iconNeutral"
            div_ [class_ "flex flex-col"] do
              span_ [class_ "text-textWeak text-xs"] "Last run"
              span_ [class_ "text-sm font-medium text-textStrong"] do
                case item.lastRun of
                  Just t -> toHtml $ prettyTimeAuto item.createdAt t
                  Nothing -> "Never"

        -- Type-specific details
        case item.details of
          AlertDetails{alertThreshold, warningThreshold, triggerDirection} ->
            thresholdBox_ alertThreshold warningThreshold triggerDirection

        -- Actions
        div_ [class_ "flex gap-2 px-4 py-2 items-center rounded-3xl opacity-0 group-hover/card:opacity-100 transition-opacity"] do
          a_ [href_ editUrl, term "data-tippy-content" "Edit", class_ "hover:text-textBrand transition-colors"] do
            faSprite_ "pen-to-square" "regular" "h-5 w-5"
          button_
            [ type_ "button"
            , term "data-tippy-content" $ if item.status `elem` ["Active", "Passing"] then "Deactivate" else "Activate"
            , class_ "hover:text-textBrand transition-colors"
            , hxPost_ toggleUrl
            , hxTarget_ "closest .itemsListItem"
            , hxSwap_ "outerHTML"
            ]
            do
              faSprite_ (if item.status `elem` ["Active", "Passing"] then "pause" else "play") "regular" "h-5 w-5"
  where
    (_, typeLabel, _) = ("bell", "Alert", "bg-fillWarning-weak text-iconWarning")

    -- Use unified overview route for both types
    detailsUrl = "/p/" <> item.projectId <> "/monitors/" <> item.monitorId <> "/overview"

    editUrl = case item.details of
      AlertDetails{query, visualizationType} ->
        "/p/"
          <> item.projectId
          <> "/log_explorer/?alert="
          <> item.monitorId
          <> "&query="
          <> toUriStr query
          <> "&viz_type="
          <> visualizationType

    toggleUrl = "/p/" <> item.projectId <> "/monitors/alerts/" <> item.monitorId <> "/toggle_active"


instance ToHtml UnifiedMonitorItem where
  toHtml = toHtmlRaw
  toHtmlRaw item =
    div_ [class_ "border-b flex p-4 gap-4 itemsListItem hover:bg-fillWeak transition-colors group/card"] do
      toHtmlRaw $ renderMonitorIcon item
      toHtmlRaw $ renderMonitorContent item


-- | Shared status badge component used across monitors
statusBadge :: Text -> Html ()
statusBadge = statusBadge_ False


-- | Status badge component with size option
statusBadge_ :: Bool -> Text -> Html ()
statusBadge_ isLarge status = do
  let (badgeClass, icon) = case status of
        "Passing" -> ("badge-success", Just "check")
        "Failing" -> ("badge-error", Just "xmark")
        "Active" -> ("badge-success", Nothing)
        "Inactive" -> ("badge-ghost", Nothing)
        "Alerting" -> ("badge-error", Nothing)
        "Warning" -> ("badge-warning", Nothing)
        "alert" -> ("badge-error", Nothing)
        _ -> ("badge-ghost", Nothing)
      sizeClass = if isLarge then "" else "badge-sm"
  span_ [class_ $ sizeClass <> " " <> badgeClass <> " gap-1 badge"] do
    whenJust icon $ \i -> faSprite_ i "regular" "h-3 w-3"
    toHtml status


-- | Threshold box for alerts
thresholdBox_ :: Int -> Maybe Int -> Text -> Html ()
thresholdBox_ alert warning direction = do
  div_ [class_ "flex gap-2 p-3 items-center border rounded-3xl"] do
    div_ [class_ "flex items-center gap-3"] do
      -- Direction indicator
      div_ [class_ "flex items-center gap-1"] do
        -- faSprite_ (if direction == "above" then "arrow-down" else "arrow-down") "regular" "h-4 w-4"
        span_ [class_ "text-xs text-textWeak"] $ toHtml direction
      -- Alert threshold
      div_ [class_ "text-center flex items-center gap-2"] do
        div_ [class_ "text-textError font-medium"] $ show alert
        small_ [class_ "block text-xs text-textWeak"] "Alert"
      -- Warning threshold
      whenJust warning $ \w -> do
        div_ [class_ "text-center flex items-center gap-2"] do
          div_ [class_ "text-textWarning font-medium"] $ show w
          small_ [class_ "block text-xs text-textWeak"] "Warning"


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
      let bwconf' = bwconf{navTabs = Just $ monitorOverviewTabs pid monitorId "alert"}
      let findChannel xx x = fromMaybe x (find (\c -> c.channelId == x) xx >>= (\a -> Just a.channelName))
      let teams' = (\x -> x{slack_channels = findChannel channels <$> x.slack_channels, discord_channels = (\xx -> fromMaybe xx (find (\c -> c.channelId == xx) discordChannels >>= (\a -> Just a.channelName))) <$> x.discord_channels}) <$> teams
      addRespHeaders $ PageCtx bwconf' $ unifiedOverviewPage pid alert currTime (V.fromList teams') slackDataM discordDataM
    _ -> addRespHeaders $ PageCtx bwconf $ div_ [class_ "p-6 text-center"] "Alert not found"


-- | Unified overview tabs
monitorOverviewTabs :: Projects.ProjectId -> Text -> Text -> Html ()
monitorOverviewTabs pid monitorId monitorType = do
  let overviewUrl = "/p/" <> pid.toText <> "/monitors/" <> monitorId <> "/overview"
      editUrl = case monitorType of
        "alert" -> "/p/" <> pid.toText <> "/monitors/alerts/" <> monitorId
        _ -> "/p/" <> pid.toText <> "/monitors/collection?col_id=" <> monitorId
  div_ [class_ "tabs tabs-box tabs-outline items-center"] do
    a_ [href_ overviewUrl, role_ "tab", class_ "tab h-auto! tab-active text-textStrong"] "Overview"
    a_ [href_ editUrl, role_ "tab", class_ "tab h-auto! "] "Configuration"


-- | Unified overview page that handles both monitor types
unifiedOverviewPage :: Projects.ProjectId -> Monitors.QueryMonitorEvaled -> UTCTime -> V.Vector ManageMembers.Team -> Maybe Slack.SlackData -> Maybe Slack.DiscordData -> Html ()
unifiedOverviewPage pid alert currTime teams slackDataM discordDataM = do
  section_ [class_ "pt-2 mx-auto px-4 w-full flex flex-col gap-4 h-full overflow-y-auto"] do
    -- Header section
    div_ [class_ "flex justify-between items-center"] do
      monitorHeader alert.alertConfig.title (isJust alert.deactivatedAt) ("Severity: " <> alert.alertConfig.severity)

      -- Action buttons
      div_ [class_ "flex gap-2"] do
        button_
          [ class_ "btn btn-sm btn-outline"
          , hxPost_ $ "/p/" <> pid.toText <> "/monitors/alerts/" <> alert.id.toText <> "/toggle_active"
          , hxTarget_ "body"
          , hxSwap_ "innerHTML"
          ]
          $ if isJust alert.deactivatedAt
            then "Activate"
            else "Deactivate"
        a_
          [href_ $ "/p/" <> pid.toText <> "/log_explorer?alert=" <> alert.id.toText <> "&query=" <> alert.logQuery, class_ "btn btn-sm btn-primary"]
          do
            faSprite_ "pen-to-square" "regular" "h-4 w-4"
            "Edit Alert"

    -- Stats section
    div_ [class_ "relative p-1 flex gap-10 items-start"] do
      alertStats_ pid alert currTime

    -- Content tabs
    tabbedSection_ "monitor-tabs" [("Query & Visualization", alertQueryTab_ pid alert), ("Execution History", monitorHistoryTab_ pid alert.id), ("Notification Channels", alertNotificationsTab_ alert teams)]
  where
    monitorHeader title isInactive subtitle = do
      div_ [class_ "flex flex-col gap-2"] do
        _ <- h1_ [class_ "text-2xl font-semibold text-textStrong"] $ toHtml title
        div_ [class_ "flex gap-2 items-center text-sm"] do
          statusBadge $ if isInactive then "Inactive" else "Active"
          span_ [class_ "text-textWeak"] "•"
          span_ [class_ "text-textWeak"] $ toHtml subtitle


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
    virtualTable pid (Just initialUrl)


-- | Alert statistics boxes (copied from Alerts module for consolidation)
alertStats_ :: Projects.ProjectId -> Monitors.QueryMonitorEvaled -> UTCTime -> Html ()
alertStats_ pid alert currTime = do
  section_ [class_ "space-y-3 shrink-0 w-full"] do
    div_ [class_ "flex gap-2"] do
      statBox_ (Just pid) Nothing "Check Interval" "How often the alert query is evaluated" (show alert.checkIntervalMins <> " min") Nothing Nothing
      statBox_ (Just pid) Nothing "Alert Threshold" ("Trigger alert when value is " <> direction) (fmt (commaizeF alert.alertThreshold)) Nothing Nothing
      whenJust alert.warningThreshold $ \warning ->
        statBox_ (Just pid) Nothing "Warning Threshold" ("Trigger warning when value is " <> direction) (fmt (commaizeF warning)) Nothing Nothing
      statBox_ (Just pid) Nothing "Last Evaluated" "When the alert was last checked" (toText $ prettyTimeAuto currTime alert.lastEvaluated) Nothing Nothing
      statBox_ (Just pid) Nothing "Last Triggered" "When the alert was last triggered" (maybe "Never" (toText . prettyTimeAuto currTime) alert.alertLastTriggered) Nothing Nothing
  where
    direction = if alert.triggerLessThan then "below" else "above"


-- | Alert query tab content (copied from Alerts module)
alertQueryTab_ :: Projects.ProjectId -> Monitors.QueryMonitorEvaled -> Html ()
alertQueryTab_ pid alert = do
  div_ [class_ "pt-6 pb-3"] do
    -- Query display
    div_ [class_ "mb-6"] do
      h3_ [class_ "font-medium text-textStrong mb-2 text-sm"] "Alert query"
      div_ [class_ "bg-fillWeaker rounded-lg p-2 border-strokeWeak"] do
        pre_ [class_ "text-sm font-mono text-textWeak overflow-x-auto"] $ toHtml alert.logQuery

    -- Visualization
    div_ [] do
      div_ [class_ "", style_ "aspect-ratio: 4 / 2;"] do
        Widget.widget_
          $ (def :: Widget)
            { Widget.wType = Widget.mapChatTypeToWidgetType alert.visualizationType
            , Widget.query = Just alert.logQuery
            , Widget.title = Just "Alert Query Visualization"
            , Widget.standalone = Just True
            , Widget._projectId = Just pid
            , Widget.layout = Just (def{Widget.w = Just 12, Widget.h = Just 6})
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
                , class_ "absolute top-3 cursor-pointer right-3 text-red-600 text-textWeak hover:text-textBrand transition-colors"
                , term "data-tippy-content" "Remove team"
                , hxDelete_ $ "/p/" <> alert.projectId.toText <> "/monitors/alerts/" <> alert.id.toText <> "/teams/" <> UUID.toText team.id
                , hxTarget_ $ "#" <> team.handle
                , hxSwap_ "outerHTML"
                ]
                do
                  faSprite_ "trash" "regular" "h-3 w-3 stroke-red-400"
              span_ [class_ "text-sm font-medium"] $ toHtml team.name
              forM_ team.notify_emails $ \email ->
                div_ [class_ "flex items-center gap-2"] do
                  span_ [class_ "text-sm text-textWeak"] $ faSprite_ "envelope" "regular" "h-3 w-3 mr-2" >> toHtml email
              forM_ team.slack_channels $ \channel ->
                div_ [class_ "flex items-center gap-2"] do
                  span_ [class_ "text-sm text-textWeak"] $ faSprite_ "slack" "solid" "h-3 w-3 mr-2" >> toHtml ("#" <> channel)
              forM_ team.discord_channels $ \channel ->
                div_ [class_ "flex items-center gap-2"] do
                  span_ [class_ "text-sm text-textWeak"] $ faSprite_ "discord" "brand" "h-3 w-3 mr-2" >> toHtml ("#" <> channel)

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
