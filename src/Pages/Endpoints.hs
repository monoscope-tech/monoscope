module Pages.Endpoints (apiCatalogH, CatalogTab (..), tabParam, parseTab, parseTabM, HostEventsVM (..), endpointListGetH, CatalogList (..), EndpointRequestStatsVM (..), EnpReqStatsVM (..), apiCatalogBulkActionH, HostBulkActionForm (..), HostBulkAction (..), CatalogBulkAction (..), apiDocsH, ApiDocsPage (..), apiSpecJsonH, apiSpecYamlH, docsHref) where

import Data.Aeson qualified as AE
import Data.Cache qualified as Cache
import Data.Default (def)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)
import Data.Vector qualified as V
import Effectful.Concurrent.Async (concurrently)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Log (logAttention)
import Lucid
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.SchemaCatalog qualified as SchemaCatalog
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.RUM qualified as RUMData
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, navTabAttrs)
import Pages.Components (RowAction (..), compactTimeAgo, copyButton_, detailTab_, periodToggle_, rowActions_, sparkline_, tabPanel_)
import Pkg.Components.Table (BulkAction (..), Column (..), Config (..), EmptyStateAction (..), Features (..), Pagination (..), SearchMode (..), TabFilter (..), TabFilterOpt (..), Table (..), TableHeaderActions (..), TableRows (..), ZeroState (..), col, withAttrs, withColHeaderExtra)
import Pkg.DeriveUtils (WrappedEnumSC (..), assetUrl, bulkActionSlug)
import Pkg.OpenApi qualified as OpenApi
import PyF qualified
import Relude hiding (ask, asks)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, addTriggerEvent)
import Text.Time.Pretty (prettyTimeAuto)
import Utils (checkFreeTierStatus, faSprite_, formatWithCommas, toUriStr)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (FromHttpApiData, parseUrlPiece)


apiCatalogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders CatalogList)
apiCatalogH pid sortM timeFilter currentTabM periodM skipM filterTabM statsM = do
  (_, project, bw) <- mkPageCtx pid

  -- Legacy request_type=… kept alongside the unified ?filter=… for shared links.
  let currentTab = fromMaybe TabIncoming $ asum $ map (>>= parseTabM) [filterTabM, currentTabM]
      currentTabT = tabParam currentTab
      currentSort = fromMaybe "-events" sortM
      -- Unrecognised values keep their pre-typed meaning (the default window), so a
      -- stale shared link still renders instead of 400ing.
      filterV = parseOr Endpoints.Since24h timeFilter
      period = parseOr Endpoints.Window24h periodM
      showArchived = currentTab == TabArchived
      outgoingM = directionOf currentTab
      sortV = bool Endpoints.SortEvents Endpoints.SortName (currentSort `elem` ["-name", "+name"])

  appCtx <- ask @AuthContext
  -- The host list is a cheap Postgres read; the per-host counts and sparkline scan a
  -- full window of spans in the telemetry store and can take tens of seconds. So the
  -- first request renders a shell and HTMX immediately re-fetches it with stats=true,
  -- and that (much slower) response is memoised for a few minutes so tab and period
  -- toggles — and every other viewer of the project — read it for free.
  let statsMode = if statsM == Just "true" then Endpoints.WithStats else Endpoints.ShellOnly
      skip = fromMaybe 0 skipM
      cacheKey = (pid, currentTabT, sortV, filterV, period, skip)
      hostQuery = Endpoints.HostQuery{direction = outgoingM, archived = showArchived, sort = sortV, skip, since = filterV, period}
      fetch mode = Endpoints.dependenciesAndEventsCount mode appCtx.env.enableTimefusionReads pid hostQuery
  hostsAndEvents <- case statsMode of
    Endpoints.ShellOnly -> fetch Endpoints.ShellOnly
    -- Memory first, then the fleet-shared table, so a cold replica doesn't re-pay the
    -- multi-second span scan another replica already ran this TTL (same layering as RUM).
    Endpoints.WithStats -> Cache.fetchWithCache appCtx.hostStatsCache cacheKey \_ ->
      RUMData.withSharedCache ("hostStats:" <> show cacheKey) 300 (fetch Endpoints.WithStats)
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan

  currTime <- Time.currentTime

  let periodT = display period
      baseUrl = "/p/" <> pid.toText <> "/api_catalog?filter=" <> currentTabT <> "&sort=" <> currentSort <> "&period=" <> periodT
      statsUrl = baseUrl <> "&stats=true"
      -- On the Archived tab the action spans both directions, so we omit
      -- request_type and let the handler resolve direction per row.
      bulkActionItem =
        if showArchived
          then BulkAction{icon = Just "rotate-left", title = "Unarchive", uri = "/p/" <> pid.toText <> "/api_catalog/bulk_action/" <> bulkActionSlug BAUnarchive}
          else BulkAction{icon = Just "archive", title = "Archive", uri = "/p/" <> pid.toText <> "/api_catalog/bulk_action/" <> bulkActionSlug BAArchive <> "?request_type=" <> currentTabT}
      hostsVM = V.fromList $ map (\events -> HostEventsVM{events, currTime, statsMode}) hostsAndEvents
      cols = catalogColumns pid baseUrl periodT
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
                        , title = "No " <> currentTabT <> " Requests Monitored."
                        , description = "Once you integrate an SDK, your " <> T.toLower currentTabT <> " requests appear here automatically."
                        , action = ESLink "https://monoscope.tech/docs/sdks/" "View SDK setup guides"
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
                  { current = currentTabT
                  , currentURL = "/p/" <> pid.toText <> "/api_catalog?sort=" <> currentSort <> "&period=" <> periodT
                  , options = [TabFilterOpt{name = tabParam t, count = Nothing} | t <- [minBound .. maxBound]]
                  }
          }
  addRespHeaders case skipM of
    Just _ -> CatalogListRows TableRows{columns = cols, rows = hostsVM, renderAsTable = True, rowId = hostRowId, rowAttrs = hostRowAttrs, pagination = Nothing}
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
  , col "Last Seen" (\vm -> statCell_ vm.statsMode $ lastSeenCell_ vm.currTime vm.events.last_seen) & withAttrs [class_ "w-28 max-md:hidden"]
  , col "Activity" (\vm -> statCell_ vm.statsMode $ activityCell_ vm.events.activityBuckets) & withAttrs [class_ "w-40 max-md:hidden"] & withColHeaderExtra (periodToggle_ baseUrl "apiCatalogContainer" period)
  ]


logExplorerHref :: Projects.ProjectId -> Text -> Text
logExplorerHref pid q = "/p/" <> pid.toText <> "/log_explorer?query=" <> toUriStr q


-- | Parse a query param, falling back on anything unrecognised. Query params here
-- come from shared/stale links, so a bad value must render the default view rather
-- than 400 the way a typed 'QueryParam' would.
parseOr :: FromHttpApiData a => a -> Maybe Text -> a
parseOr d = maybe d (fromRight d . parseUrlPiece)


-- | The api_catalog / endpoints direction tab. The wire spelling is capitalised and
-- written out rather than derived, because live shared links carry it.
--
-- >>> map tabParam [minBound .. maxBound]
-- ["Incoming","Outgoing","Archived"]
data CatalogTab = TabIncoming | TabOutgoing | TabArchived
  deriving stock (Bounded, Enum, Eq, Ord, Show)


tabParam :: CatalogTab -> Text
tabParam TabIncoming = "Incoming"
tabParam TabOutgoing = "Outgoing"
tabParam TabArchived = "Archived"


-- | Strict parse — 'Nothing' for an unrecognised spelling. Use where absence and
-- "unrecognised" must stay distinguishable (the bulk handler treats absent as
-- "both directions").
--
-- >>> (parseTabM "Outgoing", parseTabM "outgoing", parseTabM "nope")
-- (Just TabOutgoing,Nothing,Nothing)
parseTabM :: Text -> Maybe CatalogTab
parseTabM = inverseMap tabParam


-- | Tolerant parse for page handlers: a stale shared link renders the default tab
-- instead of 400ing, which is why these stay 'QueryParam' @Text@ rather than a typed
-- capture.
--
-- >>> (parseTab (Just "Archived"), parseTab (Just "bogus"), parseTab Nothing)
-- (TabArchived,TabIncoming,TabIncoming)
parseTab :: Maybe Text -> CatalogTab
parseTab = fromMaybe TabIncoming . (parseTabM =<<)


-- | The direction a tab filters on; @Nothing@ (the Archived tab) spans both.
directionOf :: CatalogTab -> Maybe Endpoints.Direction
directionOf TabOutgoing = Just Endpoints.Outgoing
directionOf TabIncoming = Just Endpoints.Incoming
directionOf TabArchived = Nothing


-- | The two labels that vary by request direction: (kindVal, sourceLabel).
directionLabels :: Bool -> (Text, Text)
directionLabels outgoing = (bool "server" "client" outgoing, bool "Served by:" "Called by:" outgoing)


-- | The services behind a row, as inline chips on the name line rather than a
-- second line. Half the rows have no services, so a wrapped sub-line gave the
-- table two different row heights and broke the vertical scan that is the whole
-- point of a dense list. The "Served by:" / "Called by:" sense moves into the
-- tooltip; the row's direction arrow already carries it visually.
servicesBadges_ :: Text -> (Text -> Text) -> [Text] -> Html ()
servicesBadges_ sourceLabel badgeHref svcs =
  unless (null svcs) $ div_ [class_ "flex items-center gap-1 min-w-0 overflow-hidden max-md:hidden"] do
    forM_ svcs \svc ->
      a_
        [ href_ $ badgeHref svc
        , -- Explicit tokens, not `badge-ghost`: the ghost variant has no fill,
          -- so on the dark surface the chip vanished into bare text sitting
          -- beside the host name with nothing to say it was a service.
          class_ "shrink-0 rounded-sm border border-strokeWeak bg-fillWeak px-1.5 py-0.5 text-xs text-textWeak whitespace-nowrap transition-colors hover:border-strokeBrand-weak hover:text-textBrand"
        , term "data-tippy-content" $ sourceLabel <> " " <> svc <> " — filter logs by this service"
        ]
        $ toHtml svc


renderCatalogMainCol :: Projects.ProjectId -> HostEventsVM -> Html ()
renderCatalogMainCol pid vm = do
  let he = vm.events
      outgoing = he.outgoing
      reqTypeLabel = bool "Incoming" "Outgoing" outgoing :: Text
      (kindVal, sourceLabel) = directionLabels outgoing
      (arrowIcon, arrowClass) = bool ("arrow-down-left", "h-3 w-3 fill-iconNeutral shrink-0") ("arrow-up-right", "h-3 w-3 fill-iconBrand shrink-0") outgoing
  -- One line, name left and actions right, so the actions form a fixed lane
  -- down the table. Brand blue is spent on the host — the row's primary target
  -- — and the secondary actions stay neutral until hovered.
  div_ [class_ "flex items-center justify-between gap-3 min-w-0"] do
    div_ [class_ "flex items-center gap-2 min-w-0"] do
      span_ [class_ "tooltip tooltip-right shrink-0 inline-flex", term "data-tip" $ reqTypeLabel <> " request"] $ faSprite_ arrowIcon "solid" arrowClass
      a_ ([href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> he.host <> "&request_type=" <> reqTypeLabel, class_ "font-medium text-textBrand hover:underline underline-offset-2 decoration-from-font truncate min-w-0"] <> navTabAttrs) $ toHtml (T.replace "http://" "" $ T.replace "https://" "" he.host)
      servicesBadges_
        sourceLabel
        (\svc -> logExplorerHref pid $ "resource.service.name==\"" <> svc <> "\" AND kind==\"" <> kindVal <> "\"")
        (V.toList he.services)
    rowActions_
      [ RowAction{icon = "explore", label = "Logs", href = logExplorerHref pid $ "attributes.server.address==\"" <> he.host <> "\"", attrs = navTabAttrs}
      , -- Plain link: the docs page loads Swagger UI from headContent, which an
        -- HTMX content-only swap would never fetch.
        RowAction{icon = "brackets-curly", label = "API docs", href = docsHref pid he.host reqTypeLabel Nothing, attrs = []}
      ]


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
      -- Was an unvalidated 'fromMaybe "Incoming"' here while apiCatalogH validated the
      -- same parameter, so the two handlers disagreed on what a bad tab meant.
      currentTab = parseTab currentTabM
      currentTabT = tabParam currentTab
      direction = fromMaybe Endpoints.Incoming $ directionOf currentTab
      currentSort = fromMaybe "-events" sortM
      period = parseOr Endpoints.Window24h periodM
      periodT = display period
      sortV = bool Endpoints.SortEvents Endpoints.SortName (currentSort `elem` ["-name", "+name"])
  appCtx <- ask @AuthContext
  let useTf = appCtx.env.enableTimefusionReads
      endpointQuery = Endpoints.EndpointQuery{direction, archived, host = hostParam, search = searchM, sort = sortV, page, perPage, period}
      -- Same shell/deferred/memoise dance as apiCatalogH: the telemetry aggregate takes
      -- seconds, so the first paint renders skeletons and HTMX re-fetches with stats=true.
      -- Row-only responses (load-more, search) can't defer, so they always carry stats.
      statsMode = if statsM == Just "true" || isJust loadMoreM || isJust searchM then Endpoints.WithStats else Endpoints.ShellOnly
      cacheKey = ((pid, currentTabT, host, currentSort), (fromMaybe "" searchM, page, perPage, period))
      fetchStats mode = Endpoints.endpointRequestStatsByProject mode useTf pid endpointQuery
      fetchStatsCached = case statsMode of
        Endpoints.ShellOnly -> fetchStats Endpoints.ShellOnly
        Endpoints.WithStats -> Cache.fetchWithCache appCtx.endpointStatsCache cacheKey \_ ->
          RUMData.withSharedCache ("endpointStats:" <> show cacheKey) 300 (fetchStats Endpoints.WithStats)
  (endpointStats, totalCount) <-
    concurrently
      fetchStatsCached
      (Endpoints.countEndpointsForHost pid endpointQuery)
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan

  let baseUrl = [PyF.fmt|/p/{pid.toText}/endpoints?filter={currentFilterTab}&request_type={currentTabT}&host={host}&sort={currentSort}&period={periodT}|]
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
                  , options =
                      [ TabFilterOpt{name = "Endpoints", count = Nothing}
                      , TabFilterOpt{name = "Archived", count = Nothing}
                      ]
                  }
          }

  currTime <- Time.currentTime
  let statsUrl = baseUrl <> [PyF.fmt|&stats=true&page={page}&per_page={perPage}|]
      endpReqVM = V.map (EnpReqStatsVM currTime statsMode) endpointStats
      cols = endpointColumns pid baseUrl periodT currentTab
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
                        , action = ESLink "https://monoscope.tech/docs/sdks/" "View SDK setup guides"
                        }
                , header = Just $ div_ [class_ "mb-2"] $ maybe "Endpoints" (\h -> span_ [] "Endpoints for: " >> span_ [class_ "text-textBrand font-bold"] (toHtml h)) hostM
                }
          }
  addRespHeaders
    $ if isJust loadMoreM || isJust searchM
      then EndpointsListRows TableRows{columns = cols, rows = endpReqVM, renderAsTable = True, rowId = endpRowId, rowAttrs = endpRowAttrs, pagination = pagination'}
      else EndpointsListPage $ PageCtx bwconf endpointsTable


data EnpReqStatsVM = EnpReqStatsVM UTCTime Endpoints.StatsMode Endpoints.EndpointRequestStats
  deriving stock (Show)


endpointColumns :: Projects.ProjectId -> Text -> Text -> CatalogTab -> [Column EnpReqStatsVM]
endpointColumns pid baseUrl period currentTab =
  [ col "Endpoint" (renderEndpointMainCol pid currentTab) & withAttrs [class_ "min-w-0 max-w-0 w-full"]
  , col ("Events (" <> period <> ")") (\(EnpReqStatsVM _ sm enp) -> statCell_ sm $ eventsCountCell_ enp.totalRequests) & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Last Seen" (\(EnpReqStatsVM currTime sm enp) -> statCell_ sm $ lastSeenCell_ currTime enp.lastSeen) & withAttrs [class_ "w-28 max-md:hidden"]
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
  -- nowrap: the column is narrow enough that "50 secs ago" broke onto a second
  -- line and put back the ragged row heights the single-line row just removed.
  Just t -> span_ [class_ "text-xs text-textWeak whitespace-nowrap"] $ toHtml $ compactTimeAgo $ toText $ prettyTimeAuto currTime $ zonedTimeToUTC t
  Nothing -> span_ [class_ "text-textWeak text-xs"] "-"


activityCell_ :: V.Vector Int -> Html ()
activityCell_ = sparkline_ . V.toList


renderEndpointMainCol :: Projects.ProjectId -> CatalogTab -> EnpReqStatsVM -> Html ()
renderEndpointMainCol pid currentTab (EnpReqStatsVM _ _ enp) = do
  let outgoing = currentTab == TabOutgoing
      (kindVal, sourceLabel) = directionLabels outgoing
      -- server.address in both directions: the local server on a server span, the remote
      -- one on a client span. It is also the column apis.endpoints.host is resolved from,
      -- so the filter matches the stored host exactly.
      q = "attributes.server.address==\"" <> enp.host <> "\" AND kind==\"" <> kindVal <> "\" AND attributes.http.route==\"" <> enp.urlPath <> "\" AND attributes.http.request.method==\"" <> enp.method <> "\""
  div_ [class_ "flex items-center justify-between gap-3 min-w-0"] do
    div_ [class_ "flex items-center gap-2 min-w-0"] do
      a_ ([class_ "inline-flex items-center gap-1.5 font-medium text-textBrand hover:underline underline-offset-2 decoration-from-font truncate min-w-0", href_ ("/p/" <> pid.toText <> "/endpoints/details?var-endpointHash=" <> enp.endpointHash <> "&var-host=" <> enp.host)] <> navTabAttrs) $ do
        span_ [class_ $ "endpoint endpoint-" <> T.toLower enp.method <> " shrink-0 !w-auto !p-0.5 !px-1.5 !m-0 !text-xs !rounded", data_ "enp-urlMethod" enp.method] $ toHtml enp.method
        span_ [class_ "inconsolata text-sm truncate", data_ "enp-urlPath" enp.urlPath] $ toHtml $ if T.null enp.urlPath then "/" else T.take 150 enp.urlPath
      servicesBadges_
        sourceLabel
        (\svc -> logExplorerHref pid $ "resource.service.name==\"" <> svc <> "\" AND kind==\"" <> kindVal <> "\" AND attributes.http.route==\"" <> enp.urlPath <> "\"")
        (V.toList enp.services)
    rowActions_
      [ RowAction{icon = "explore", label = "Logs", href = logExplorerHref pid q, attrs = navTabAttrs}
      , RowAction{icon = "brackets-curly", label = "Schema", href = docsHref pid enp.host (tabParam currentTab) (Just enp.endpointHash), attrs = []}
      ]


data EndpointRequestStatsVM
  = EndpointsListPage (PageCtx (Table EnpReqStatsVM))
  | EndpointsListRows (TableRows EnpReqStatsVM)


instance ToHtml EndpointRequestStatsVM where
  toHtml (EndpointsListPage pg) = toHtml pg
  toHtml (EndpointsListRows rows) = toHtml rows
  toHtmlRaw = toHtml


-- Learned API documentation ----------------------------------------------------

-- | Enough operations to document a real service; a host with more learned
-- routes than this is almost always a scanned WordPress site, and the whole
-- spec would be unreadable anyway. 'apiDocsH' says so on the page.
docsCap :: Int
docsCap = 200


-- | Above this the YAML is offered as a download instead of rendered inline —
-- a single WooCommerce products endpoint carries ~2000 learned fields, and a
-- host's worth of those is megabytes of text no @\<pre\>@ should hold.
inlineYamlCap :: Int
inlineYamlCap = 400_000


-- | Span kind for a catalog tab. Incoming traffic is what our server served;
-- outgoing is what it called. Archived spans both, so it filters on neither.
kindOf :: CatalogTab -> Maybe Text
kindOf TabIncoming = Just "server"
kindOf TabOutgoing = Just "client"
kindOf TabArchived = Nothing


-- | Link to the docs page for a host, optionally scoped to one endpoint. Plain
-- @href@ on purpose: the page needs Swagger UI's assets from 'headContent', and
-- the HTMX nav pattern swaps only the content container.
docsHref :: Projects.ProjectId -> Text -> Text -> Maybe Text -> Text
docsHref pid host reqType endpointM = "/p/" <> pid.toText <> "/api_catalog/docs" <> specQuery host reqType endpointM


-- | The query string all three learned-spec URLs share. An absent host is the
-- whole project, so it is omitted rather than sent empty.
specQuery :: Text -> Text -> Maybe Text -> Text
specQuery host reqType endpointM =
  "?request_type=" <> reqType <> foldMap (("&host=" <>) . toUriStr) (guarded (not . T.null) host) <> foldMap ("&endpoint=" <>) endpointM


-- | The learned OpenAPI document at whatever scope the caller asked for: the
-- whole project, one host, or one endpoint. Shared by the page and both spec
-- routes so all three can never disagree.
learnedSpec :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (AE.Value, Int)
learnedSpec pid hostM reqTypeM endpointM = do
  -- Project membership is enforced per handler, not by the router: the two spec
  -- routes render no page shell, so nothing else would check it and any signed-in
  -- user could read another tenant's learned schema by project id.
  (_, project) <- Projects.sessionAndProject pid
  let scopeHost = guarded (not . T.null) =<< hostM
  entries <- SchemaCatalog.endpointCatalog pid scopeHost (kindOf (parseTab reqTypeM)) endpointM docsCap
  pure (OpenApi.buildSpec (fromMaybe project.title scopeHost) (V.toList entries), V.length entries)


apiSpecJsonH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders AE.Value)
apiSpecJsonH pid hostM reqTypeM endpointM = addRespHeaders . fst =<< learnedSpec pid hostM reqTypeM endpointM


apiSpecYamlH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders Text)
apiSpecYamlH pid hostM reqTypeM endpointM = addRespHeaders . OpenApi.specYaml . fst =<< learnedSpec pid hostM reqTypeM endpointM


apiDocsH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ApiDocsPage)
apiDocsH pid hostM reqTypeM endpointM = do
  (_, project, bw) <- mkPageCtx pid
  (spec, operations) <- learnedSpec pid hostM reqTypeM endpointM
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan
  let host = maybeToMonoid hostM
      reqType = tabParam (parseTab reqTypeM)
      specUrl ext = "/p/" <> pid.toText <> "/api_catalog/openapi." <> ext <> specQuery host reqType endpointM
      yaml = OpenApi.specYaml spec
      bwconf =
        bw
          { prePageTitle = Just "API Catalog"
          , pageTitle = "Learned API docs" <> if T.null host then "" else " for " <> host
          , freeTierStatus
          , headContent = Just do
              link_ [rel_ "stylesheet", type_ "text/css", href_ $ assetUrl "/public/assets/deps/swagger-ui/swagger-ui.css"]
              link_ [rel_ "stylesheet", type_ "text/css", href_ $ assetUrl "/public/assets/css/swagger-ui-theme.css"]
              script_ [src_ $ assetUrl "/public/assets/deps/swagger-ui/swagger-ui-bundle.js", defer_ "true"] ("" :: Text)
          }
  addRespHeaders
    $ ApiDocsPage
    $ PageCtx
      bwconf
      ApiDocsVM
        { specJsonUrl = specUrl "json"
        , specYamlUrl = specUrl "yaml"
        , inlineYaml = guarded ((<= inlineYamlCap) . T.length) yaml
        , operations
        , truncated = operations >= docsCap
        }


data ApiDocsVM = ApiDocsVM
  { specJsonUrl :: Text
  , specYamlUrl :: Text
  , inlineYaml :: Maybe Text
  , operations :: Int
  , truncated :: Bool
  }


newtype ApiDocsPage = ApiDocsPage (PageCtx ApiDocsVM)


instance ToHtml ApiDocsPage where
  toHtml (ApiDocsPage pg) = toHtml pg
  toHtmlRaw = toHtml


instance ToHtml ApiDocsVM where
  toHtmlRaw = toHtml
  toHtml = toHtml . apiDocs_


apiDocs_ :: ApiDocsVM -> Html ()
apiDocs_ vm = div_ [class_ "group/apidocs flex flex-col h-full min-h-0 w-full"] do
  div_ [class_ "flex items-center gap-3 border-b border-strokeWeak px-4 shrink-0"] do
    nav_ [class_ "flex", term "_" "on click halt the event's bubbling"] do
      detailTab_ "apidocsTab" "apidocs-ref" "" True "Reference"
      detailTab_ "apidocsTab" "apidocs-yaml" "" False "OpenAPI 3.1"
    div_ [class_ "ml-auto flex items-center gap-2 py-1.5"] do
      span_ [class_ "text-xs text-textWeak tabular-nums"] $ toHtml (show vm.operations <> " learned operation" <> bool "s" "" (vm.operations == 1) :: Text)
      when vm.truncated
        $ span_ [class_ "badge badge-sm badge-warning", term "data-tippy-content" "Only the most recently seen operations are documented. Open a single endpoint's docs for the rest."] "truncated"
      forM_ [("YAML" :: Text, vm.specYamlUrl, "openapi.yaml" :: Text), ("JSON", vm.specJsonUrl, "openapi.json")] \(label, url, fname) ->
        a_ [href_ url, term "download" fname, class_ "btn btn-xs btn-ghost gap-1"] do
          faSprite_ "download" "regular" "h-3 w-3"
          toHtml label

  tabPanel_ "group-has-[.apidocs-ref:checked]/apidocs:block grow min-h-0 overflow-auto" "apidocs-reference" do
    -- The spec URL travels as an attribute rather than interpolated into the
    -- script: it carries a customer-controlled host, and Lucid escapes attributes.
    div_ [id_ "swagger-ui", data_ "spec-url" vm.specJsonUrl] ""
    -- A module script is deferred by definition, so it runs after the head's
    -- deferred swagger-ui-bundle.js has defined SwaggerUIBundle. Submit methods
    -- are empty on purpose: the servers block is the customer's real production
    -- host, and "Try it out" would fire live requests at it from our UI.
    script_
      [type_ "module"]
      """
      const el = document.getElementById('swagger-ui');
      SwaggerUIBundle({
        domNode: el, url: el.dataset.specUrl,
        presets: [SwaggerUIBundle.presets.apis], deepLinking: true,
        supportedSubmitMethods: [], tryItOutEnabled: false,
        docExpansion: 'list', defaultModelsExpandDepth: -1, defaultModelExpandDepth: 4
      });
      """

  tabPanel_ "group-has-[.apidocs-yaml:checked]/apidocs:block grow min-h-0 overflow-auto" "apidocs-spec" case vm.inlineYaml of
    Just yaml -> div_ [class_ "relative"] do
      copyButton_ "btn btn-xs btn-ghost absolute right-3 top-3 gap-1" "h-3 w-3" "#openapi-yaml's innerText" []
      pre_ [class_ "inconsolata text-xs leading-relaxed p-4 whitespace-pre overflow-x-auto", id_ "openapi-yaml"] $ toHtml yaml
    Nothing ->
      div_ [class_ "p-8 text-center text-sm text-textWeak"] do
        p_ "This host's learned spec is too large to show inline."
        a_ [href_ vm.specYamlUrl, term "download" "openapi.yaml", class_ "btn btn-sm mt-3"] "Download openapi.yaml"


-- Host bulk archive/unarchive --------------------------------------------------

newtype HostBulkActionForm = HostBulk {itemId :: [Text]}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data CatalogBulkAction = CatalogBulkDone


instance ToHtml CatalogBulkAction where
  toHtml CatalogBulkDone = ""
  toHtmlRaw = toHtml


-- | The slugs are the existing wire spellings, so live URLs are unchanged:
--
-- >>> map bulkActionSlug [minBound .. maxBound :: HostBulkAction]
-- ["archive","unarchive"]
data HostBulkAction = BAArchive | BAUnarchive
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (FromHttpApiData) via WrappedEnumSC 'Nothing "BA" HostBulkAction


-- | Past-tense verb for the success toast. Was built as @action <> "d"@, which only
-- worked because both slugs happen to end in @e@.
actionPast :: HostBulkAction -> Text
actionPast BAArchive = "Archived"
actionPast BAUnarchive = "Unarchived"


apiCatalogBulkActionH
  :: Projects.ProjectId -> HostBulkAction -> Maybe Text -> HostBulkActionForm -> ATAuthCtx (RespHeaders CatalogBulkAction)
apiCatalogBulkActionH pid action currentTabM items = do
  -- TODO: emit a host-activity log entry per item once the activity feed
  -- accepts non-issue events (mirrors anomalyBulkActionsPostH's per-item
  -- Issues.logIssueActivity). Keeps archive/unarchive auditable.
  (sess, _project) <- Projects.sessionAndProject pid
  -- request_type=Incoming/Outgoing scopes the action; absent (e.g. on the
  -- Archived tab) means apply to whichever direction the host carries.
  let outgoingM = directionOf =<< (parseTabM =<< currentTabM)
      requested = length items.itemId
      logCtx extra = AE.object $ ["project_id" AE..= pid.toText, "action" AE..= bulkActionSlug action, "requested_count" AE..= requested] <> extra
  if requested == 0
    then addErrorToast "No hosts selected" Nothing
    else do
      let op = case action of
            BAArchive -> Endpoints.ArchiveBy sess.user.id
            BAUnarchive -> Endpoints.Unarchive
      affected <- Endpoints.setHostsArchived pid outgoingM op items.itemId
      let touched = fromIntegral affected :: Int
          noun = bool "hosts" "host" (requested == 1)
      if touched == 0
        then do
          logAttention "api_catalog bulk action affected 0 rows" $ logCtx ["outgoing" AE..= outgoingM]
          addErrorToast ("Could not " <> bulkActionSlug action <> " " <> noun) (Just "Already in that state, or rows were removed")
        else do
          when (touched < requested)
            $ logAttention "api_catalog bulk action partially applied"
            $ logCtx ["affected_count" AE..= touched]
          addSuccessToast (actionPast action <> " " <> show touched <> bool (" of " <> show requested) "" (touched == requested) <> " " <> noun) Nothing
          addTriggerEvent "apiCatalogChanged" AE.Null
  addRespHeaders CatalogBulkDone
