module Pages.Endpoints (apiCatalogH, HostEventsVM (..), endpointListGetH, CatalogList (..), EndpointRequestStatsVM (..), EnpReqStatsVM (..), apiCatalogBulkActionH, HostBulkActionForm (..), CatalogBulkAction (..)) where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Effectful.Concurrent.Async (concurrently)
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Log (logAttention)
import Lucid
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), navTabAttrs)
import Pages.Components (compactTimeAgo, periodToggle_, sparkline_)
import Pkg.Components.Table (BulkAction (..), Column (..), Config (..), Features (..), Pagination (..), SearchMode (..), TabFilter (..), TabFilterOpt (..), Table (..), TableHeaderActions (..), TableRows (..), ZeroState (..), col, withAttrs, withColHeaderExtra)
import PyF qualified
import Relude hiding (ask, asks)
import Servant (err400, errBody)
import System.Config (AuthContext (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, addTriggerEvent)
import Text.Time.Pretty (prettyTimeAuto)
import Utils (checkFreeTierStatus, faSprite_, formatWithCommas, toUriStr)
import Web.FormUrlEncoded (FromForm)


apiCatalogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> ATAuthCtx (RespHeaders CatalogList)
apiCatalogH pid sortM timeFilter currentTabM periodM skipM filterTabM = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext

  -- Legacy request_type=… kept alongside the unified ?filter=… for shared links.
  let normTab t = if t `elem` ["Incoming", "Outgoing", "Archived"] then Just t else Nothing
      currentTab = fromMaybe "Incoming" $ asum $ map (>>= normTab) [filterTabM, currentTabM]
      currentSort = fromMaybe "-events" sortM
      filterV = fromMaybe "24H" timeFilter
      period = fromMaybe "24h" periodM
      showArchived = currentTab == "Archived"
      outgoingM :: Maybe Bool
      outgoingM = case currentTab of
        "Outgoing" -> Just True
        "Incoming" -> Just False
        _ -> Nothing
      -- Map new sort format to old format for DB query
      sortV = case currentSort of
        "-events" -> "events"
        "+events" -> "events"
        "-name" -> "name"
        "+name" -> "name"
        _ -> "events"

  hostsAndEvents <- Endpoints.dependenciesAndEventsCount pid outgoingM sortV (fromMaybe 0 skipM) filterV period showArchived
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan

  currTime <- Time.currentTime

  let baseUrl = "/p/" <> pid.toText <> "/api_catalog?filter=" <> currentTab <> "&sort=" <> currentSort <> "&period=" <> period
      -- On the Archived tab the action spans both directions, so we omit
      -- request_type and let the handler resolve direction per row.
      bulkActionItem =
        if showArchived
          then BulkAction{icon = Just "rotate-left", title = "Unarchive", uri = "/p/" <> pid.toText <> "/api_catalog/bulk_action/unarchive"}
          else BulkAction{icon = Just "archive", title = "Archive", uri = "/p/" <> pid.toText <> "/api_catalog/bulk_action/archive?request_type=" <> currentTab}
      hostsVM = V.fromList $ map (\host -> HostEventsVM pid host filterV currentTab period currTime) hostsAndEvents
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
  let catalogTable =
        Table
          { config = def{elemID = "apiCatalogForm", containerId = Just "apiCatalogContainer", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0, refreshOnEvent = Just ("apiCatalogChanged", baseUrl)}
          , columns = catalogColumns pid currentTab baseUrl period
          , rows = hostsVM
          , features =
              def
                { rowId = Just \(HostEventsVM _ he _ _ _ _) -> he.host
                , rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"]
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

  let bwconf =
        def
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "API Catalog"
          , freeTierStatus = freeTierStatus
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
  case skipM of
    Just _ -> addRespHeaders $ CatalogListRows $ TableRows{columns = catalogColumns pid currentTab baseUrl period, rows = hostsVM, emptyState = Nothing, renderAsTable = True, rowId = Just \(HostEventsVM _ he _ _ _ _) -> he.host, rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"], pagination = Nothing}
    _ -> addRespHeaders $ CatalogListPage $ PageCtx bwconf catalogTable


data HostEventsVM = HostEventsVM Projects.ProjectId Endpoints.HostEvents Text Text Text UTCTime


catalogColumns :: Projects.ProjectId -> Text -> Text -> Text -> [Column HostEventsVM]
catalogColumns pid currentTab baseUrl period =
  [ col "Dependency" (renderCatalogMainCol pid currentTab) & withAttrs [class_ "min-w-0 max-w-0 w-full"]
  , col ("Events (" <> period <> ")") (\(HostEventsVM _ he _ _ _ _) -> eventsCountCell_ (fromIntegral he.eventCount)) & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Last Seen" (\(HostEventsVM _ he _ _ _ currTime) -> lastSeenCell_ currTime he.last_seen) & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Activity" (\(HostEventsVM _ he _ _ _ _) -> activityCell_ he.activityBuckets) & withAttrs [class_ "w-40 max-md:hidden"] & withColHeaderExtra (periodToggle_ baseUrl "apiCatalogContainer" period)
  ]


logExplorerHref :: Projects.ProjectId -> Text -> Text
logExplorerHref pid q = "/p/" <> pid.toText <> "/log_explorer?query=" <> toUriStr q


servicesBadges_ :: Text -> Text -> (Text -> Text) -> [Text] -> Html ()
servicesBadges_ sourceLabel kindVal badgeHref svcs =
  unless (null svcs) $ div_ [class_ "flex items-center gap-1 flex-wrap min-w-0"] do
    span_ [class_ "text-xs text-textWeak shrink-0"] $ toHtml sourceLabel
    forM_ svcs \svc ->
      a_
        [ href_ $ badgeHref svc
        , class_ "badge badge-sm badge-ghost text-xs whitespace-nowrap hover:text-textBrand transition-colors"
        , term "data-tippy-content" $ "Filter logs by service: " <> svc
        ]
        $ toHtml svc


renderCatalogMainCol :: Projects.ProjectId -> Text -> HostEventsVM -> Html ()
renderCatalogMainCol pid _currentTab (HostEventsVM _ he _ _ _ _) = do
  let PGArray svcs = he.services
      outgoing = he.outgoing
      reqTypeLabel = bool "Incoming" "Outgoing" outgoing :: Text
      sourceLabel = bool "Served by:" "Called by:" outgoing
      kindVal = bool "server" "client" outgoing
      (arrowIcon, arrowClass, arrowTip) =
        if outgoing
          then ("arrow-up-right", "h-3 w-3 fill-iconBrand shrink-0", "Outgoing request" :: Text)
          else ("arrow-down-left", "h-3 w-3 fill-iconNeutral shrink-0", "Incoming request" :: Text)
  div_ [class_ "flex flex-col gap-1 min-w-0"] do
    div_ [class_ "flex items-center gap-2 min-w-0"] do
      span_ [class_ "tooltip tooltip-right shrink-0 inline-flex", term "data-tip" arrowTip] $ faSprite_ arrowIcon "solid" arrowClass
      a_ ([href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> he.host <> "&request_type=" <> reqTypeLabel, class_ "font-medium text-textStrong hover:text-textBrand transition-colors truncate min-w-0"] <> navTabAttrs) $ toHtml (T.replace "http://" "" $ T.replace "https://" "" he.host)
      a_ ([href_ $ logExplorerHref pid $ "attributes.net.host.name==\"" <> he.host <> "\"", class_ "shrink-0 text-xs text-textBrand hover:text-textStrong transition-colors"] <> navTabAttrs) "View logs"
    servicesBadges_
      sourceLabel
      kindVal
      (\svc -> logExplorerHref pid $ "resource.service.name==\"" <> svc <> "\" AND kind==\"" <> kindVal <> "\"")
      svcs


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
  -> ATAuthCtx (RespHeaders EndpointRequestStatsVM)
endpointListGetH pid pageM perPageM layoutM filterTM hostM currentTabM sortM periodM hxRequestM hxBoostedM hxCurrentURL loadMoreM searchM = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  let (ackd, archived, currentFilterTab) = case filterTM of
        -- Legacy "Active"/"Inbox" links route to the default tab.
        Just "Archived" -> (False, True, "Archived")
        _ -> (True, False, "Endpoints")

  let host = maybeToMonoid $ hostM >>= \t -> if t == "" then Nothing else Just t
      page = fromMaybe 0 $ readMaybe (toString $ fromMaybe "" pageM)
      perPage = max 1 $ min 200 $ fromMaybe 25 $ readMaybe (toString $ fromMaybe "" perPageM)
      hostParam = hostM >>= \h -> if h == "" then Nothing else Just h
      isOutgoing = fromMaybe "Incoming" currentTabM == "Outgoing"
      currentSort = fromMaybe "-events" sortM
      period = fromMaybe "24h" periodM
      -- Map new sort format to old format for DB query
      sortV = case currentSort of
        "-events" -> Just "events"
        "+events" -> Just "events"
        "-name" -> Just "name"
        "+name" -> Just "name"
        _ -> Just "events"
  (endpointStats, totalCount) <-
    concurrently
      (Endpoints.endpointRequestStatsByProject pid ackd archived hostParam sortV searchM page perPage (fromMaybe "" currentTabM) period)
      (Endpoints.countEndpointsForHost pid isOutgoing hostParam searchM)
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan

  let currentTab = fromMaybe "Incoming" currentTabM
      baseUrl = [PyF.fmt|/p/{pid.toText}/endpoints?filter={currentFilterTab}&request_type={currentTab}&host={host}&sort={currentSort}&period={period}|]
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , prePageTitle = Just "API Catalog"
          , pageTitle = "Endpoints for " <> host
          , freeTierStatus = freeTierStatus
          , config = appCtx.env
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
  let endpReqVM = V.map (EnpReqStatsVM False currTime period) endpointStats
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
  let endpointsTable =
        Table
          { config = def{elemID = "endpointsForm", containerId = Just "endpointsListContainer", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0, refreshOnEvent = Just ("endpointsListChanged", baseUrl)}
          , columns = endpointColumns pid baseUrl period currentTab
          , rows = endpReqVM
          , features =
              def
                { rowId = Just \(EnpReqStatsVM _ _ _ enp) -> enp.endpointHash
                , rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"]
                , bulkActions = [BulkAction{icon = Just "archive", title = "Archive", uri = "/p/" <> pid.toText <> "/endpoints/bulk_action/archive"}]
                , search = Just (ServerSide baseUrl)
                , tableHeaderActions = Just tableActions
                , pagination = Just Pagination{currentPage = page, perPage = perPage, totalCount = totalCount, baseUrl = baseUrl, targetId = "endpointsListContainer"}
                , zeroState =
                    Just
                      $ ZeroState
                        { icon = "empty-set"
                        , title = "Waiting for events"
                        , description = "Once you integrate an SDK, your endpoints appear here automatically."
                        , actionText = "View SDK setup guides"
                        , destination = Right "https://monoscope.tech/docs/sdks/"
                        }
                , header = Just $ div_ [class_ "mb-4"] $ case hostM of
                    Just h -> span_ [] "Endpoints for: " >> span_ [class_ "text-textBrand font-bold"] (toHtml h)
                    Nothing -> "Endpoints"
                }
          }
  let endpRowId = Just \(EnpReqStatsVM _ _ _ enp) -> enp.endpointHash
      endpRowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"]
      rowsOnly = EndpointsListRows $ TableRows{columns = endpointColumns pid baseUrl period currentTab, rows = endpReqVM, emptyState = Nothing, renderAsTable = True, rowId = endpRowId, rowAttrs = endpRowAttrs, pagination = Just Pagination{currentPage = page, perPage = perPage, totalCount = totalCount, baseUrl = baseUrl, targetId = "endpointsListContainer"}}
  addRespHeaders $ if isJust loadMoreM || isJust searchM then rowsOnly else EndpointsListPage $ PageCtx bwconf endpointsTable


data EnpReqStatsVM = EnpReqStatsVM Bool UTCTime Text Endpoints.EndpointRequestStats
  deriving stock (Show)


endpointColumns :: Projects.ProjectId -> Text -> Text -> Text -> [Column EnpReqStatsVM]
endpointColumns pid baseUrl period currentTab =
  [ col "Endpoint" (renderEndpointMainCol pid currentTab) & withAttrs [class_ "min-w-0 max-w-0 w-full"]
  , col ("Events (" <> period <> ")") (\(EnpReqStatsVM _ _ _ enp) -> eventsCountCell_ enp.totalRequests) & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Last Seen" (\(EnpReqStatsVM _ currTime _ enp) -> lastSeenCell_ currTime enp.lastSeen) & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Activity" (\(EnpReqStatsVM _ _ _ enp) -> activityCell_ enp.activityBuckets) & withAttrs [class_ "w-40 max-md:hidden"] & withColHeaderExtra (periodToggle_ baseUrl "endpointsListContainer" period)
  ]


-- Shared column cell renderers for both catalog and endpoint tables
eventsCountCell_ :: Int -> Html ()
eventsCountCell_ n =
  span_ [class_ $ "tabular-nums font-medium " <> style] $ toHtml $ formatWithCommas (fromIntegral n)
  where
    style
      | n >= 100 = "text-sm text-fillError-strong"
      | n >= 10 = "text-sm text-fillWarning-strong"
      | otherwise = "text-sm text-textStrong"


lastSeenCell_ :: UTCTime -> Maybe ZonedTime -> Html ()
lastSeenCell_ currTime = \case
  Just t -> span_ [class_ "text-xs text-textWeak"] $ toHtml $ compactTimeAgo $ toText $ prettyTimeAuto currTime $ zonedTimeToUTC t
  Nothing -> span_ [class_ "text-textWeak text-xs"] "-"


activityCell_ :: PGArray Int -> Html ()
activityCell_ (PGArray buckets) = sparkline_ buckets


renderEndpointMainCol :: Projects.ProjectId -> Text -> EnpReqStatsVM -> Html ()
renderEndpointMainCol pid currentTab (EnpReqStatsVM _ _ _ enp) = do
  let outgoing = currentTab == "Outgoing"
      hostAttr = bool "attributes.net.host.name" "attributes.server.address" outgoing
      kindVal = bool "server" "client" outgoing :: Text
      sourceLabel = bool "Served by:" "Called by:" outgoing
      q = hostAttr <> "==\"" <> enp.host <> "\" AND kind==\"" <> kindVal <> "\" AND attributes.http.route==\"" <> enp.urlPath <> "\" AND attributes.http.request.method==\"" <> enp.method <> "\""
      PGArray svcs = enp.services
  div_ [class_ "flex flex-col gap-1 min-w-0"] do
    div_ [class_ "flex items-center gap-2 min-w-0"] do
      a_ ([class_ "inline-flex items-center gap-1.5 font-medium text-textStrong hover:text-textBrand transition-colors truncate min-w-0", href_ ("/p/" <> pid.toText <> "/endpoints/details?var-endpointHash=" <> enp.endpointHash <> "&var-host=" <> enp.host)] <> navTabAttrs) $ do
        span_ [class_ $ "endpoint endpoint-" <> T.toLower enp.method <> " shrink-0 !w-auto !p-0.5 !px-1.5 !m-0 !text-xs !rounded", data_ "enp-urlMethod" enp.method] $ toHtml enp.method
        span_ [class_ "inconsolata text-sm truncate", data_ "enp-urlPath" enp.urlPath] $ toHtml $ if T.null enp.urlPath then "/" else T.take 150 enp.urlPath
      a_ ([class_ "shrink-0 text-xs text-textBrand hover:text-textStrong transition-colors", href_ (logExplorerHref pid q)] <> navTabAttrs) "View logs"
    servicesBadges_
      sourceLabel
      kindVal
      (\svc -> logExplorerHref pid $ "resource.service.name==\"" <> svc <> "\" AND kind==\"" <> kindVal <> "\" AND attributes.http.route==\"" <> enp.urlPath <> "\"")
      svcs


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
  let outgoingM = case currentTabM of
        Just "Outgoing" -> Just True
        Just "Incoming" -> Just False
        _ -> Nothing
      requested = length items.itemId
  if requested == 0
    then do
      addErrorToast "No hosts selected" Nothing
      addRespHeaders CatalogBulkDone
    else do
      affected <- case action of
        "archive" -> Endpoints.archiveHosts pid outgoingM (Just sess.user.id) items.itemId
        "unarchive" -> Endpoints.unarchiveHosts pid outgoingM items.itemId
        _ -> throwError err400{errBody = "unhandled api_catalog bulk action: " <> encodeUtf8 action}
      let touched = fromIntegral affected :: Int
          verb = action <> "d"
          noun = if requested == 1 then "host" else "hosts"
      if touched == 0
        then do
          logAttention "api_catalog bulk action affected 0 rows"
            $ AE.object
              [ "project_id" AE..= pid.toText
              , "action" AE..= action
              , "requested_count" AE..= requested
              , "outgoing" AE..= outgoingM
              ]
          addErrorToast ("Could not " <> action <> " " <> noun) (Just "Already in that state, or rows were removed")
        else do
          when (touched < requested)
            $ logAttention "api_catalog bulk action partially applied"
            $ AE.object
              [ "project_id" AE..= pid.toText
              , "action" AE..= action
              , "requested_count" AE..= requested
              , "affected_count" AE..= touched
              ]
          let summary =
                if touched == requested
                  then verb <> " " <> show touched <> " " <> noun
                  else verb <> " " <> show touched <> " of " <> show requested <> " " <> noun
          addSuccessToast summary Nothing
          addTriggerEvent "apiCatalogChanged" AE.Null
      addRespHeaders CatalogBulkDone
