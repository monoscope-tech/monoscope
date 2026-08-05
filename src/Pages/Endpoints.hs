module Pages.Endpoints (apiCatalogH, HostEventsVM (..), endpointListGetH, CatalogList (..), EndpointRequestStatsVM (..), EnpReqStatsVM (..), apiCatalogBulkActionH, HostBulkActionForm (..), CatalogBulkAction (..)) where

import Data.Aeson qualified as AE
import Data.Cache qualified as Cache
import Data.Default (def)
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)
import Data.Vector qualified as V
import Effectful.Concurrent.Async (concurrently)
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Log (logAttention)
import Lucid
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, navTabAttrs)
import Pages.Components (compactTimeAgo, periodToggle_, sparkline_)
import Pkg.Components.Table (BulkAction (..), Column (..), Config (..), Features (..), Pagination (..), SearchMode (..), TabFilter (..), TabFilterOpt (..), Table (..), TableHeaderActions (..), TableRows (..), ZeroState (..), col, withAttrs, withColHeaderExtra)
import PyF qualified
import Relude hiding (ask, asks)
import Servant (err400, errBody)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, addTriggerEvent)
import Text.Time.Pretty (prettyTimeAuto)
import Utils (checkFreeTierStatus, faSprite_, formatWithCommas, toUriStr)
import Web.FormUrlEncoded (FromForm)


apiCatalogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders CatalogList)
apiCatalogH pid sortM timeFilter currentTabM periodM skipM filterTabM statsM = do
  (_, project, bw) <- mkPageCtx pid

  -- Legacy request_type=… kept alongside the unified ?filter=… for shared links.
  let normTab = guarded (`elem` ["Incoming", "Outgoing", "Archived"])
      currentTab = fromMaybe "Incoming" $ asum $ map (>>= normTab) [filterTabM, currentTabM]
      currentSort = fromMaybe "-events" sortM
      filterV = fromMaybe "24H" timeFilter
      period = fromMaybe "24h" periodM
      showArchived = currentTab == "Archived"
      outgoingM = directionOf currentTab
      sortV = bool "events" "name" (currentSort `elem` ["-name", "+name"])

  appCtx <- ask @AuthContext
  -- The host list is a cheap Postgres read; the per-host counts and sparkline scan a
  -- full window of spans in the telemetry store and can take tens of seconds. So the
  -- first request renders a shell and HTMX immediately re-fetches it with stats=true,
  -- and that (much slower) response is memoised for a few minutes so tab and period
  -- toggles — and every other viewer of the project — read it for free.
  let statsMode = if statsM == Just "true" then Endpoints.WithStats else Endpoints.ShellOnly
      skip = fromMaybe 0 skipM
      cacheKey = (pid, currentTab, sortV, filterV, period, skip)
      fetch mode = Endpoints.dependenciesAndEventsCount mode appCtx.env.enableTimefusionReads pid outgoingM sortV skip filterV period showArchived
  hostsAndEvents <- case statsMode of
    Endpoints.ShellOnly -> fetch Endpoints.ShellOnly
    Endpoints.WithStats ->
      liftIO (Cache.lookup appCtx.hostStatsCache cacheKey)
        >>= maybe (fetch Endpoints.WithStats >>= \fresh -> fresh <$ liftIO (Cache.insert appCtx.hostStatsCache cacheKey fresh)) pure
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan

  currTime <- Time.currentTime

  let baseUrl = "/p/" <> pid.toText <> "/api_catalog?filter=" <> currentTab <> "&sort=" <> currentSort <> "&period=" <> period
      statsUrl = baseUrl <> "&stats=true"
      -- On the Archived tab the action spans both directions, so we omit
      -- request_type and let the handler resolve direction per row.
      bulkActionItem =
        if showArchived
          then BulkAction{icon = Just "rotate-left", title = "Unarchive", uri = "/p/" <> pid.toText <> "/api_catalog/bulk_action/unarchive"}
          else BulkAction{icon = Just "archive", title = "Archive", uri = "/p/" <> pid.toText <> "/api_catalog/bulk_action/archive?request_type=" <> currentTab}
      hostsVM = V.fromList $ map (\events -> HostEventsVM{events, currTime, statsMode}) hostsAndEvents
      cols = catalogColumns pid baseUrl period
      hostRowId = Just \(vm :: HostEventsVM) -> vm.events.host
      hostRowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"]
      tableActions =
        TableHeaderActions
          { baseUrl
          , targetId = "apiCatalogContainer"
          , sortOptions =
              [ ("Most Active", "Most recently accessed", "-events")
              , ("Alphabetical", "Sort by dependency name", "+name")
              ]
          , currentSort
          , filterMenus = []
          , activeFilters = []
          , headerExtra = Nothing
          }
      catalogTable =
        Table
          { config = def{elemID = "apiCatalogForm", containerId = Just "apiCatalogContainer", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0, refreshOnEvent = Just ("apiCatalogChanged", statsUrl), deferredUrl = statsUrl <$ guard (statsMode == Endpoints.ShellOnly)}
          , columns = cols
          , rows = hostsVM
          , features =
              def
                { rowId = hostRowId
                , rowAttrs = hostRowAttrs
                , bulkActions = [bulkActionItem]
                , search = Just ClientSide
                , tableHeaderActions = Just tableActions
                , -- Hosts per project rarely exceed dozens; the model caps the result at 200.
                  -- If we need more, mirror the endpoints page: route per_page + countHostsForProject.
                  pagination = Nothing
                , zeroState =
                    Just
                      $ ZeroState
                        { icon = "empty-set"
                        , title = "No " <> currentTab <> " Requests Monitored."
                        , description = "Once you integrate an SDK, your " <> T.toLower currentTab <> " requests appear here automatically."
                        , actionText = "View SDK setup guides"
                        , destination = Right "https://monoscope.tech/docs/sdks/"
                        }
                }
          }
      bwconf =
        bw
          { pageTitle = "API Catalog"
          , freeTierStatus
          , navTabs =
              Just
                $ toHtml
                $ TabFilter
                  { current = currentTab
                  , currentURL = "/p/" <> pid.toText <> "/api_catalog?sort=" <> currentSort <> "&period=" <> period
                  , clientSide = False
                  , options =
                      [ TabFilterOpt{name = "Incoming", count = Nothing, targetId = Nothing}
                      , TabFilterOpt{name = "Outgoing", count = Nothing, targetId = Nothing}
                      , TabFilterOpt{name = "Archived", count = Nothing, targetId = Nothing}
                      ]
                  }
          }
  addRespHeaders case skipM of
    Just _ -> CatalogListRows TableRows{columns = cols, rows = hostsVM, emptyState = Nothing, renderAsTable = True, rowId = hostRowId, rowAttrs = hostRowAttrs, pagination = Nothing}
    Nothing -> CatalogListPage $ PageCtx bwconf catalogTable


-- | A catalog row: its traffic, the clock the "last seen" column renders against, and
-- whether stats have arrived yet (shell rows render skeletons instead of zeros).
data HostEventsVM = HostEventsVM
  { events :: Endpoints.HostEvents
  , currTime :: UTCTime
  , statsMode :: Endpoints.StatsMode
  }


catalogColumns :: Projects.ProjectId -> Text -> Text -> [Column HostEventsVM]
catalogColumns pid baseUrl period =
  [ col "Dependency" (renderCatalogMainCol pid) & withAttrs [class_ "min-w-0 max-w-0 w-full"]
  , col ("Events (" <> period <> ")") (\vm -> statCell_ vm.statsMode $ eventsCountCell_ (fromIntegral vm.events.eventCount)) & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Last Seen" (\vm -> statCell_ vm.statsMode $ lastSeenCell_ vm.currTime vm.events.last_seen) & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Activity" (\vm -> statCell_ vm.statsMode $ activityCell_ vm.events.activityBuckets) & withAttrs [class_ "w-40 max-md:hidden"] & withColHeaderExtra (periodToggle_ baseUrl "apiCatalogContainer" period)
  ]


logExplorerHref :: Projects.ProjectId -> Text -> Text
logExplorerHref pid q = "/p/" <> pid.toText <> "/log_explorer?query=" <> toUriStr q


-- | Parse a request-direction tab label into the `outgoing` flag it filters on.
directionOf :: Text -> Maybe Bool
directionOf = (`lookup` [("Outgoing", True), ("Incoming", False)])


-- | The two labels that vary by request direction: (kindVal, sourceLabel).
directionLabels :: Bool -> (Text, Text)
directionLabels outgoing = (bool "server" "client" outgoing, bool "Served by:" "Called by:" outgoing)


servicesBadges_ :: Text -> (Text -> Text) -> [Text] -> Html ()
servicesBadges_ sourceLabel badgeHref svcs =
  unless (null svcs) $ div_ [class_ "flex items-center gap-1 flex-wrap min-w-0"] do
    span_ [class_ "text-xs text-textWeak shrink-0"] $ toHtml sourceLabel
    forM_ svcs \svc ->
      a_
        [ href_ $ badgeHref svc
        , class_ "badge badge-sm badge-ghost text-xs whitespace-nowrap hover:text-textBrand transition-colors"
        , term "data-tippy-content" $ "Filter logs by service: " <> svc
        ]
        $ toHtml svc


renderCatalogMainCol :: Projects.ProjectId -> HostEventsVM -> Html ()
renderCatalogMainCol pid vm = do
  let he = vm.events
      outgoing = he.outgoing
      reqTypeLabel = bool "Incoming" "Outgoing" outgoing :: Text
      (kindVal, sourceLabel) = directionLabels outgoing
      (arrowIcon, arrowClass) = bool ("arrow-down-left", "h-3 w-3 fill-iconNeutral shrink-0") ("arrow-up-right", "h-3 w-3 fill-iconBrand shrink-0") outgoing
  div_ [class_ "flex flex-col gap-1 min-w-0"] do
    div_ [class_ "flex items-center gap-2 min-w-0"] do
      span_ [class_ "tooltip tooltip-right shrink-0 inline-flex", term "data-tip" $ reqTypeLabel <> " request"] $ faSprite_ arrowIcon "solid" arrowClass
      a_ ([href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> he.host <> "&request_type=" <> reqTypeLabel, class_ "font-medium text-textStrong hover:text-textBrand transition-colors truncate min-w-0"] <> navTabAttrs) $ toHtml (T.replace "http://" "" $ T.replace "https://" "" he.host)
      a_ ([href_ $ logExplorerHref pid $ "attributes.net.host.name==\"" <> he.host <> "\"", class_ "shrink-0 text-xs text-textBrand hover:text-textStrong transition-colors"] <> navTabAttrs) "View logs"
    servicesBadges_
      sourceLabel
      (\svc -> logExplorerHref pid $ "resource.service.name==\"" <> svc <> "\" AND kind==\"" <> kindVal <> "\"")
      (V.toList he.services)


data CatalogList = CatalogListPage (PageCtx (Table HostEventsVM)) | CatalogListRows (TableRows HostEventsVM)


instance ToHtml CatalogList where
  toHtml (CatalogListPage pg) = toHtml pg
  toHtml (CatalogListRows r) = toHtml r
  toHtmlRaw = toHtml


endpointListGetH
  :: Projects.ProjectId
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ATAuthCtx (RespHeaders EndpointRequestStatsVM)
endpointListGetH pid pageM perPageM _layoutM filterTM hostM currentTabM sortM periodM _hxRequestM _hxBoostedM _hxCurrentURL loadMoreM searchM statsM = do
  (_, project, bw) <- mkPageCtx pid
  let archived = filterTM == Just "Archived"
      currentFilterTab = bool "Endpoints" "Archived" archived
      hostParam = guarded (/= "") =<< hostM
      host = maybeToMonoid hostParam
      page = fromMaybe 0 $ readMaybe . toString =<< pageM
      perPage = max 1 $ min 200 $ fromMaybe 25 $ readMaybe . toString =<< perPageM
      currentTab = fromMaybe "Incoming" currentTabM
      isOutgoing = currentTab == "Outgoing"
      currentSort = fromMaybe "-events" sortM
      period = fromMaybe "24h" periodM
      sortV = Just $ bool "events" "name" (currentSort `elem` ["-name", "+name"])
  appCtx <- ask @AuthContext
  let useTf = appCtx.env.enableTimefusionReads
      -- Same shell/deferred/memoise dance as apiCatalogH: the telemetry aggregate takes
      -- seconds, so the first paint renders skeletons and HTMX re-fetches with stats=true.
      -- Row-only responses (load-more, search) can't defer, so they always carry stats.
      statsMode = if statsM == Just "true" || isJust loadMoreM || isJust searchM then Endpoints.WithStats else Endpoints.ShellOnly
      cacheKey = ((pid, currentTab, host, currentSort), (fromMaybe "" searchM, page, perPage, period))
      fetchStats mode = Endpoints.endpointRequestStatsByProject mode useTf pid archived hostParam sortV searchM page perPage (fromMaybe "" currentTabM) period
      fetchStatsCached = case statsMode of
        Endpoints.ShellOnly -> fetchStats Endpoints.ShellOnly
        Endpoints.WithStats ->
          liftIO (Cache.lookup appCtx.endpointStatsCache cacheKey)
            >>= maybe (fetchStats Endpoints.WithStats >>= \fresh -> fresh <$ liftIO (Cache.insert appCtx.endpointStatsCache cacheKey fresh)) pure
  (endpointStats, totalCount) <-
    concurrently
      fetchStatsCached
      (Endpoints.countEndpointsForHost pid isOutgoing archived hostParam searchM)
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan

  let baseUrl = [PyF.fmt|/p/{pid.toText}/endpoints?filter={currentFilterTab}&request_type={currentTab}&host={host}&sort={currentSort}&period={period}|]
      bwconf =
        bw
          { prePageTitle = Just "API Catalog"
          , pageTitle = "Endpoints for " <> host
          , freeTierStatus
          , navTabs =
              Just
                $ toHtml
                $ TabFilter
                  { current = currentFilterTab
                  , currentURL = baseUrl
                  , clientSide = False
                  , options =
                      [ TabFilterOpt{name = "Endpoints", count = Nothing, targetId = Nothing}
                      , TabFilterOpt{name = "Archived", count = Nothing, targetId = Nothing}
                      ]
                  }
          }

  currTime <- Time.currentTime
  let statsUrl = baseUrl <> [PyF.fmt|&stats=true&page={page}&per_page={perPage}|]
      endpReqVM = V.map (EnpReqStatsVM currTime statsMode) endpointStats
      cols = endpointColumns pid baseUrl period currentTab
      endpRowId = Just \(EnpReqStatsVM _ _ enp) -> enp.endpointHash
      endpRowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"]
      pagination' = Just Pagination{currentPage = page, perPage, totalCount, baseUrl, targetId = "endpointsListContainer"}
      tableActions =
        TableHeaderActions
          { baseUrl
          , targetId = "endpointsListContainer"
          , sortOptions =
              [ ("Most Active", "Most requests", "-events")
              , ("Alphabetical", "Sort by endpoint path", "+name")
              ]
          , currentSort
          , filterMenus = []
          , activeFilters = []
          , headerExtra = Nothing
          }
      endpointsTable =
        Table
          { config = def{elemID = "endpointsForm", containerId = Just "endpointsListContainer", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0, refreshOnEvent = Just ("endpointsListChanged", statsUrl), deferredUrl = statsUrl <$ guard (statsMode == Endpoints.ShellOnly)}
          , columns = cols
          , rows = endpReqVM
          , features =
              def
                { rowId = endpRowId
                , rowAttrs = endpRowAttrs
                , bulkActions = [BulkAction{icon = Just "archive", title = "Archive", uri = "/p/" <> pid.toText <> "/endpoints/bulk_action/archive"}]
                , search = Just (ServerSide baseUrl)
                , tableHeaderActions = Just tableActions
                , pagination = pagination'
                , zeroState =
                    Just
                      $ ZeroState
                        { icon = "empty-set"
                        , title = "Waiting for events"
                        , description = "Once you integrate an SDK, your endpoints appear here automatically."
                        , actionText = "View SDK setup guides"
                        , destination = Right "https://monoscope.tech/docs/sdks/"
                        }
                , header = Just $ div_ [class_ "mb-4"] $ maybe "Endpoints" (\h -> span_ [] "Endpoints for: " >> span_ [class_ "text-textBrand font-bold"] (toHtml h)) hostM
                }
          }
  addRespHeaders
    $ if isJust loadMoreM || isJust searchM
      then EndpointsListRows TableRows{columns = cols, rows = endpReqVM, emptyState = Nothing, renderAsTable = True, rowId = endpRowId, rowAttrs = endpRowAttrs, pagination = pagination'}
      else EndpointsListPage $ PageCtx bwconf endpointsTable


data EnpReqStatsVM = EnpReqStatsVM UTCTime Endpoints.StatsMode Endpoints.EndpointRequestStats
  deriving stock (Show)


endpointColumns :: Projects.ProjectId -> Text -> Text -> Text -> [Column EnpReqStatsVM]
endpointColumns pid baseUrl period currentTab =
  [ col "Endpoint" (renderEndpointMainCol pid currentTab) & withAttrs [class_ "min-w-0 max-w-0 w-full"]
  , col ("Events (" <> period <> ")") (\(EnpReqStatsVM _ sm enp) -> statCell_ sm $ eventsCountCell_ enp.totalRequests) & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Last Seen" (\(EnpReqStatsVM currTime sm enp) -> statCell_ sm $ lastSeenCell_ currTime enp.lastSeen) & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Activity" (\(EnpReqStatsVM _ sm enp) -> statCell_ sm $ activityCell_ enp.activityBuckets) & withAttrs [class_ "w-40 max-md:hidden"] & withColHeaderExtra (periodToggle_ baseUrl "endpointsListContainer" period)
  ]


-- Shared column cell renderers for both catalog and endpoint tables

-- | Stats columns are empty in the shell render, so show a skeleton rather than a
-- misleading "0"/"-" until the deferred stats response swaps in.
statCell_ :: Endpoints.StatsMode -> Html () -> Html ()
statCell_ Endpoints.WithStats h = h
statCell_ Endpoints.ShellOnly _ = span_ [class_ "block h-4 w-12 rounded bg-fillWeak animate-pulse"] ""


eventsCountCell_ :: Int -> Html ()
eventsCountCell_ n =
  span_ [class_ $ "tabular-nums font-medium text-sm " <> color] $ toHtml $ formatWithCommas (fromIntegral n)
  where
    color
      | n >= 100 = "text-fillError-strong"
      | n >= 10 = "text-fillWarning-strong"
      | otherwise = "text-textStrong"


lastSeenCell_ :: UTCTime -> Maybe ZonedTime -> Html ()
lastSeenCell_ currTime = \case
  Just t -> span_ [class_ "text-xs text-textWeak"] $ toHtml $ compactTimeAgo $ toText $ prettyTimeAuto currTime $ zonedTimeToUTC t
  Nothing -> span_ [class_ "text-textWeak text-xs"] "-"


activityCell_ :: V.Vector Int -> Html ()
activityCell_ = sparkline_ . V.toList


renderEndpointMainCol :: Projects.ProjectId -> Text -> EnpReqStatsVM -> Html ()
renderEndpointMainCol pid currentTab (EnpReqStatsVM _ _ enp) = do
  let outgoing = currentTab == "Outgoing"
      hostAttr = bool "attributes.net.host.name" "attributes.server.address" outgoing
      (kindVal, sourceLabel) = directionLabels outgoing
      q = hostAttr <> "==\"" <> enp.host <> "\" AND kind==\"" <> kindVal <> "\" AND attributes.http.route==\"" <> enp.urlPath <> "\" AND attributes.http.request.method==\"" <> enp.method <> "\""
  div_ [class_ "flex flex-col gap-1 min-w-0"] do
    div_ [class_ "flex items-center gap-2 min-w-0"] do
      a_ ([class_ "inline-flex items-center gap-1.5 font-medium text-textStrong hover:text-textBrand transition-colors truncate min-w-0", href_ ("/p/" <> pid.toText <> "/endpoints/details?var-endpointHash=" <> enp.endpointHash <> "&var-host=" <> enp.host)] <> navTabAttrs) $ do
        span_ [class_ $ "endpoint endpoint-" <> T.toLower enp.method <> " shrink-0 !w-auto !p-0.5 !px-1.5 !m-0 !text-xs !rounded", data_ "enp-urlMethod" enp.method] $ toHtml enp.method
        span_ [class_ "inconsolata text-sm truncate", data_ "enp-urlPath" enp.urlPath] $ toHtml $ if T.null enp.urlPath then "/" else T.take 150 enp.urlPath
      a_ ([class_ "shrink-0 text-xs text-textBrand hover:text-textStrong transition-colors", href_ (logExplorerHref pid q)] <> navTabAttrs) "View logs"
    servicesBadges_
      sourceLabel
      (\svc -> logExplorerHref pid $ "resource.service.name==\"" <> svc <> "\" AND kind==\"" <> kindVal <> "\" AND attributes.http.route==\"" <> enp.urlPath <> "\"")
      (V.toList enp.services)


data EndpointRequestStatsVM
  = EndpointsListPage (PageCtx (Table EnpReqStatsVM))
  | EndpointsListRows (TableRows EnpReqStatsVM)


instance ToHtml EndpointRequestStatsVM where
  toHtml (EndpointsListPage pg) = toHtml pg
  toHtml (EndpointsListRows rows) = toHtml rows
  toHtmlRaw = toHtml


-- Host bulk archive/unarchive --------------------------------------------------

newtype HostBulkActionForm = HostBulk {itemId :: [Text]}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data CatalogBulkAction = CatalogBulkDone


instance ToHtml CatalogBulkAction where
  toHtml CatalogBulkDone = ""
  toHtmlRaw = toHtml


apiCatalogBulkActionH
  :: Projects.ProjectId -> Text -> Maybe Text -> HostBulkActionForm -> ATAuthCtx (RespHeaders CatalogBulkAction)
apiCatalogBulkActionH pid action currentTabM items = do
  -- TODO: emit a host-activity log entry per item once the activity feed
  -- accepts non-issue events (mirrors anomalyBulkActionsPostH's per-item
  -- Issues.logIssueActivity). Keeps archive/unarchive auditable.
  (sess, _project) <- Projects.sessionAndProject pid
  -- request_type=Incoming/Outgoing scopes the action; absent (e.g. on the
  -- Archived tab) means apply to whichever direction the host carries.
  let outgoingM = directionOf =<< currentTabM
      requested = length items.itemId
      logCtx extra = AE.object $ ["project_id" AE..= pid.toText, "action" AE..= action, "requested_count" AE..= requested] <> extra
  if requested == 0
    then addErrorToast "No hosts selected" Nothing
    else do
      affected <- case action of
        "archive" -> Endpoints.archiveHosts pid outgoingM (Just sess.user.id) items.itemId
        "unarchive" -> Endpoints.unarchiveHosts pid outgoingM items.itemId
        _ -> throwError err400{errBody = "unhandled api_catalog bulk action: " <> encodeUtf8 action}
      let touched = fromIntegral affected :: Int
          noun = bool "hosts" "host" (requested == 1)
      if touched == 0
        then do
          logAttention "api_catalog bulk action affected 0 rows" $ logCtx ["outgoing" AE..= outgoingM]
          addErrorToast ("Could not " <> action <> " " <> noun) (Just "Already in that state, or rows were removed")
        else do
          when (touched < requested)
            $ logAttention "api_catalog bulk action partially applied"
            $ logCtx ["affected_count" AE..= touched]
          addSuccessToast (action <> "d " <> show touched <> bool (" of " <> show requested) "" (touched == requested) <> " " <> noun) Nothing
          addTriggerEvent "apiCatalogChanged" AE.Null
  addRespHeaders CatalogBulkDone
