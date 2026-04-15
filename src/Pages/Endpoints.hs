module Pages.Endpoints (apiCatalogH, HostEventsVM (..), endpointListGetH, CatalogList (..), EndpointRequestStatsVM (..), EnpReqStatsVM (..)) where

import Data.Default (def)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Lucid
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), navTabAttrs)
import Pages.Components (compactTimeAgo, periodToggle_, sparkline_)
import Pkg.Components.Table (BulkAction (..), Column (..), Config (..), Features (..), SearchMode (..), TabFilter (..), TabFilterOpt (..), Table (..), TableHeaderActions (..), TableRows (..), ZeroState (..), col, withAttrs, withColHeaderExtra)
import PyF qualified
import Relude hiding (ask, asks)
import System.Config (AuthContext (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Text.Time.Pretty (prettyTimeAuto)
import Utils (checkFreeTierStatus, formatWithCommas, toUriStr)


apiCatalogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> ATAuthCtx (RespHeaders CatalogList)
apiCatalogH pid sortM timeFilter requestTypeM periodM skipM = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext

  let requestType = fromMaybe "Incoming" requestTypeM
      currentSort = fromMaybe "-events" sortM
      filterV = fromMaybe "24H" timeFilter
      period = fromMaybe "24h" periodM
      -- Map new sort format to old format for DB query
      sortV = case currentSort of
        "-events" -> "events"
        "+events" -> "events"
        "-name" -> "name"
        "+name" -> "name"
        _ -> "events"

  hostsAndEvents <- Endpoints.dependenciesAndEventsCount pid requestType sortV (fromMaybe 0 skipM) filterV period
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan

  currTime <- Time.currentTime

  let baseUrl = "/p/" <> pid.toText <> "/api_catalog?request_type=" <> requestType <> "&sort=" <> currentSort <> "&period=" <> period
      hostsVM = V.fromList $ map (\host -> HostEventsVM pid host filterV requestType period currTime) hostsAndEvents
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
          { config = def{elemID = "apiCatalogForm", containerId = Just "apiCatalogContainer", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0}
          , columns = catalogColumns pid requestType baseUrl period
          , rows = hostsVM
          , features =
              def
                { rowId = Just \(HostEventsVM _ he _ _ _ _) -> he.host
                , rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"]
                , bulkActions = [BulkAction{icon = Just "archive", title = "Archive", uri = "/p/" <> pid.toText <> "/api_catalog/bulk_action/archive"}]
                , search = Just ClientSide
                , tableHeaderActions = Just tableActions
                , pagination = Nothing -- TODO: Add proper pagination
                , zeroState =
                    Just
                      $ ZeroState
                        { icon = "empty-set"
                        , title = "No " <> requestType <> " Requests Monitored."
                        , description = "Once you integrate an SDK, your " <> T.toLower requestType <> " requests appear here automatically."
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
          , navTabs = Just $ div_ [class_ "tabs tabs-box tabs-outline items-center"] do
              a_ ([href_ $ "/p/" <> pid.toText <> "/api_catalog?sort=" <> currentSort <> "&request_type=Incoming", role_ "tab", class_ $ "tab h-auto! " <> if requestType == "Incoming" then "tab-active text-textStrong" else ""] <> navTabAttrs) "Incoming"
              a_ ([href_ $ "/p/" <> pid.toText <> "/api_catalog?sort=" <> currentSort <> "&request_type=Outgoing", role_ "tab", class_ $ "tab h-auto! " <> if requestType == "Outgoing" then "tab-active text-textStrong" else ""] <> navTabAttrs) "Outgoing"
          }
  case skipM of
    Just _ -> addRespHeaders $ CatalogListRows $ TableRows{columns = catalogColumns pid requestType baseUrl period, rows = hostsVM, emptyState = Nothing, renderAsTable = True, rowId = Just \(HostEventsVM _ he _ _ _ _) -> he.host, rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"], pagination = Nothing}
    _ -> addRespHeaders $ CatalogListPage $ PageCtx bwconf catalogTable


data HostEventsVM = HostEventsVM Projects.ProjectId Endpoints.HostEvents Text Text Text UTCTime


catalogColumns :: Projects.ProjectId -> Text -> Text -> Text -> [Column HostEventsVM]
catalogColumns pid requestType baseUrl period =
  [ col "Dependency" (renderCatalogMainCol pid requestType) & withAttrs [class_ "min-w-0 max-w-0 w-full"]
  , col ("Events (" <> period <> ")") (\(HostEventsVM _ he _ _ _ _) -> eventsCountCell_ (fromIntegral he.eventCount)) & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Last Seen" (\(HostEventsVM _ he _ _ _ currTime) -> lastSeenCell_ currTime he.last_seen) & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Activity" (\(HostEventsVM _ he _ _ _ _) -> activityCell_ he.activityBuckets) & withAttrs [class_ "w-40 max-md:hidden"] & withColHeaderExtra (periodToggle_ baseUrl "apiCatalogContainer" period)
  ]


renderCatalogMainCol :: Projects.ProjectId -> Text -> HostEventsVM -> Html ()
renderCatalogMainCol pid requestType (HostEventsVM _ he _ _ _ _) = do
  let PGArray svcs = he.services
      outgoing = requestType == "Outgoing"
      sourceLabel = if outgoing then "Called by:" else "Served by:"
      kindVal = if outgoing then "client" else "server"
      logsHref q = "/p/" <> pid.toText <> "/log_explorer?query=" <> toUriStr q
  div_ [class_ "flex flex-col gap-1 min-w-0"] do
    div_ [class_ "flex items-center gap-2 min-w-0"] do
      a_ ([href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> he.host <> "&request_type=" <> requestType, class_ "font-medium text-textStrong hover:text-textBrand transition-colors truncate min-w-0"] <> navTabAttrs) $ toHtml (T.replace "http://" "" $ T.replace "https://" "" he.host)
      a_ ([href_ $ logsHref $ "attributes.net.host.name==\"" <> he.host <> "\"", class_ "shrink-0 text-xs text-textBrand hover:text-textStrong transition-colors"] <> navTabAttrs) "View logs"
    unless (null svcs) $ div_ [class_ "flex items-center gap-1 flex-wrap min-w-0"] do
      span_ [class_ "text-xs text-textWeak shrink-0"] sourceLabel
      forM_ svcs \svc ->
        a_
          [ href_ $ logsHref $ "resource.service.name==\"" <> svc <> "\" AND kind==\"" <> kindVal <> "\""
          , class_ "badge badge-sm badge-ghost text-xs whitespace-nowrap hover:text-textBrand transition-colors"
          , term "data-tippy-content" $ "Filter logs by service: " <> svc
          ]
          $ toHtml svc


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
  -> ATAuthCtx (RespHeaders EndpointRequestStatsVM)
endpointListGetH pid pageM layoutM filterTM hostM requestTypeM sortM periodM hxRequestM hxBoostedM hxCurrentURL loadMoreM searchM = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  let (ackd, archived, currentFilterTab) = case filterTM of
        Just "Active" -> (True, False, "Active")
        Just "Inbox" -> (False, False, "Inbox")
        Just "Archived" -> (False, True, "Archived")
        _ -> (True, False, "Active")

  let host = maybeToMonoid $ hostM >>= \t -> if t == "" then Nothing else Just t
      page = fromMaybe 0 $ readMaybe (toString $ fromMaybe "" pageM)
      hostParam = hostM >>= \h -> if h == "" then Nothing else Just h
      currentSort = fromMaybe "-events" sortM
      period = fromMaybe "24h" periodM
      -- Map new sort format to old format for DB query
      sortV = case currentSort of
        "-events" -> Just "events"
        "+events" -> Just "events"
        "-name" -> Just "name"
        "+name" -> Just "name"
        _ -> Just "events"
  endpointStats <- Endpoints.endpointRequestStatsByProject pid ackd archived hostParam sortV searchM page (fromMaybe "" requestTypeM) period
  inboxCount <- Endpoints.countEndpointInbox pid host (fromMaybe "Incoming" requestTypeM)
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan

  let requestType = fromMaybe "Incoming" requestTypeM
      baseUrl = [PyF.fmt|/p/{pid.toText}/endpoints?filter={currentFilterTab}&request_type={requestType}&host={host}&sort={currentSort}&period={period}|]
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
                      [ TabFilterOpt{name = "Active", count = Nothing, targetId = Nothing}
                      , TabFilterOpt{name = "Inbox", count = Just inboxCount, targetId = Nothing}
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
          { config = def{elemID = "endpointsForm", containerId = Just "endpointsListContainer", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0}
          , columns = endpointColumns pid baseUrl period
          , rows = endpReqVM
          , features =
              def
                { rowId = Just \(EnpReqStatsVM _ _ _ enp) -> enp.endpointHash
                , rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"]
                , bulkActions = [BulkAction{icon = Just "archive", title = "Archive", uri = "/p/" <> pid.toText <> "/endpoints/bulk_action/archive"}]
                , search = Just (ServerSide baseUrl)
                , tableHeaderActions = Just tableActions
                , pagination = Nothing -- TODO: Add proper pagination
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
      rowsOnly = EndpointsListRows $ TableRows{columns = endpointColumns pid baseUrl period, rows = endpReqVM, emptyState = Nothing, renderAsTable = True, rowId = endpRowId, rowAttrs = endpRowAttrs, pagination = Nothing}
  addRespHeaders $ if isJust loadMoreM || isJust searchM then rowsOnly else EndpointsListPage $ PageCtx bwconf endpointsTable


data EnpReqStatsVM = EnpReqStatsVM Bool UTCTime Text Endpoints.EndpointRequestStats
  deriving stock (Show)


endpointColumns :: Projects.ProjectId -> Text -> Text -> [Column EnpReqStatsVM]
endpointColumns pid baseUrl period =
  [ col "Endpoint" (renderEndpointMainCol pid) & withAttrs [class_ "min-w-0 max-w-0 w-full"]
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


renderEndpointMainCol :: Projects.ProjectId -> EnpReqStatsVM -> Html ()
renderEndpointMainCol pid (EnpReqStatsVM _ _ _ enp) =
  div_ [class_ "flex items-center gap-2 min-w-0"] do
    a_ ([class_ "inline-flex items-center gap-1.5 font-medium text-textStrong hover:text-textBrand transition-colors truncate min-w-0", href_ ("/p/" <> pid.toText <> "/endpoints/details?var-endpointHash=" <> enp.endpointHash <> "&var-host=" <> enp.host)] <> navTabAttrs) $ do
      span_ [class_ $ "endpoint endpoint-" <> T.toLower enp.method <> " shrink-0 !w-auto !p-0.5 !px-1.5 !m-0 !text-xs !rounded", data_ "enp-urlMethod" enp.method] $ toHtml enp.method
      span_ [class_ "inconsolata text-sm truncate", data_ "enp-urlPath" enp.urlPath] $ toHtml $ if T.null enp.urlPath then "/" else T.take 150 enp.urlPath
    a_ ([class_ "shrink-0 text-xs text-textBrand hover:text-textStrong transition-colors", href_ ("/p/" <> pid.toText <> "/log_explorer?query=" <> "attributes.http.route==\"" <> enp.urlPath <> "\"")] <> navTabAttrs) "View logs"


data EndpointRequestStatsVM
  = EndpointsListPage (PageCtx (Table EnpReqStatsVM))
  | EndpointsListRows (TableRows EnpReqStatsVM)


instance ToHtml EndpointRequestStatsVM where
  toHtml (EndpointsListPage pg) = toHtml pg
  toHtml (EndpointsListRows rows) = toHtml rows
  toHtmlRaw = toHtml
