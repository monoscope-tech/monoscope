module Pages.Telemetry (
  -- Metrics
  metricsOverViewGetH,
  metricDetailsGetH,
  metricServicesGetH,
  MetricsOverViewGet (..),
  metricBreakdownGetH,
  metricCardGetH,
  metricDetailUrl,
  metricExpandUrl,
  -- Trace
  traceH,
  TraceDetailsGet (..),
  tracePage,
  spanDetailAttrs_,
) where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Default
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (formatShow, iso8601Format)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.ServiceGraph (serviceMapNodeCap)
import Models.Telemetry.ServiceGraph qualified as ServiceGraph
import Models.Telemetry.Telemetry (SpanStatus (SSError))
import Models.Telemetry.Telemetry qualified as Telemetry
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, navTabAttrs)
import Pages.Components (EmptyStateAction (..), EmptyStateCfg (..))
import Pages.Components qualified as Components
import Pages.LogExplorer.LogItem (getRequestDetails, getServiceColor, getServiceName, spanHasErrors)
import Pages.LogExplorer.LogItem qualified as LogItem
import Pkg.Components.ServiceMap (serviceMapPanel_)
import Pkg.Components.Table (Table (..))
import Pkg.Components.Table qualified as Table
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Relude hiding (ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Tracing (withSpan_)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (LoadingSize (..), LoadingType (..), drawerLoadAttrs_, explorerNavTabs_, faSprite_, getDurationNSMS, getServiceColors, loadingIndicator_, onpointerdown_, parseTime, popoverPanel_, popoverTrigger_, prettyPrintCount, utcTimeToNanoseconds)


data MetricsOverViewGet
  = MetricsOVDataPointMain (PageCtx (Projects.ProjectId, V.Vector Telemetry.MetricDataPoint, Map Text (Int, Int, Int)))
  | MetricsOVChartsMain (PageCtx (Projects.ProjectId, V.Vector Telemetry.MetricChartListData, Map Text (V.Vector Text), V.Vector Telemetry.MetricChartListData, Text, Text, Int, Maybe Text))
  | MetricsOVChartsPaginated (Projects.ProjectId, V.Vector Telemetry.MetricChartListData, Map Text (V.Vector Text), Text, Maybe Text)


instance ToHtml MetricsOverViewGet where
  toHtml (MetricsOVDataPointMain (PageCtx bwconf (pid, datapoints, refCounts))) = toHtml $ PageCtx bwconf $ dataPointsPage pid datapoints refCounts
  toHtml (MetricsOVChartsMain (PageCtx bwconf (pid, mList, labels, inactive, source, prefix, activeCount, nextUrl))) = toHtml $ PageCtx bwconf $ chartsPage pid mList labels inactive source prefix activeCount nextUrl
  toHtml (MetricsOVChartsPaginated (pid, mList, labels, source, nextUrl)) = toHtml $ chartList pid labels source mList nextUrl
  toHtmlRaw = toHtml


data TraceDetailsGet
  = TraceDetails Projects.ProjectId Telemetry.Trace (V.Vector Telemetry.SpanRecord)
  | SpanDetails Projects.ProjectId Telemetry.OtelLogsAndSpans (Maybe Telemetry.OtelLogsAndSpans)
  | TraceDetailsNotFound Projects.ProjectId


data SpanMin = SpanMin
  { parentSpanId :: Maybe Text
  , spanId :: Text
  , spanName :: Text
  , spanDurationNs :: Integer
  , uSpanId :: UUID.UUID
  , hasErrors :: Bool
  , serviceName :: Text
  , startTime :: Integer
  , endTime :: Maybe Integer
  , timestamp :: UTCTime
  , attributes :: Maybe (Map Text AE.Value)
  }


-- | Extract a human-readable label from span attributes (db query, http route, rpc method, messaging).
-- Order matches semantic precedence: db query > http method+route > rpc > messaging > exception.
spanDisplayLabel :: Maybe (Map Text AE.Value) -> Maybe Text
spanDisplayLabel attrs =
  (T.take 200 <$> (look "db.query.text" <|> look "db.statement"))
    <|> joinWith " " (look "http.request.method") (look "http.route" <|> look "url.path" <|> look "url.full")
    <|> look "http.route"
    <|> look "url.path"
    <|> joinWith "/" (look "rpc.service") (look "rpc.method")
    <|> look "rpc.method"
    <|> joinWith " " (look "messaging.operation") (look "messaging.destination.name" <|> look "messaging.destination")
    <|> look "exception.type"
  where
    look k = Telemetry.atMapText k attrs
    joinWith sep = liftA2 (\a b -> a <> sep <> b)


-- | Draggable column resize divider. Parameters: CSS variable name, min%, max%, resize event, extra CSS classes.
resizeDivider_ :: Text -> Int -> Int -> Text -> Text -> Html ()
resizeDivider_ cssVar minPct maxPct resizeEvt extraCls =
  let minT = show minPct
      maxT = show maxPct
      qVar = "'" <> cssVar <> "'"
      script =
        [text|on pointerdown(clientX)
          set container to the closest parent <div[style*=$qVar]/>
          if no container exit end
          trigger setPointerCapture(pointerId: event.pointerId) on me
          set document.body.style.userSelect to 'none'
          set document.body.style.cursor to 'col-resize'
          repeat until event pointerup from document
            wait for pointermove(clientX) or pointerup from document
            if the event's type is 'pointerup' exit end
            set rect to container.getBoundingClientRect()
            set pct to ((clientX - rect.left) / rect.width) * 100
            if pct < $minT set pct to $minT end
            if pct > $maxT set pct to $maxT end
            call container.style.setProperty($qVar, pct + '%')
            send $resizeEvt to window
          end
          set document.body.style.userSelect to ''
          set document.body.style.cursor to ''
        |]
   in div_
        [ class_ $ "absolute top-0 bottom-0 w-2 cursor-col-resize z-20 flex justify-center group " <> extraCls
        , style_ $ "left:calc(var(" <> cssVar <> ") - 4px)"
        , term "_" script
        ]
        $ div_ [class_ "w-px h-full bg-strokeWeak group-hover:bg-fillBrand-strong group-active:bg-fillBrand-strong transition-colors pointer-events-none relative"] do
          div_ [class_ "absolute top-1/2 -translate-y-1/2 -translate-x-[3px] w-2 h-6 flex flex-col justify-center gap-px opacity-0 group-hover:opacity-100 transition-opacity"] do
            forM_ [1 :: Int .. 3] \_ -> div_ [class_ "w-full h-px bg-fillBrand-strong rounded-full"] pass


data SpanTree = SpanTree
  { spanRecord :: SpanMin
  , children :: [SpanTree]
  }


-- Metric tree types
data MetricNode = MetricNode {parent :: Maybe Text, current :: Text} deriving stock (Eq)


data MetricTree = MetricTree MetricNode [MetricTree]


nodePath :: MetricNode -> Text
nodePath nd = maybe nd.current (<> "." <> nd.current) nd.parent


pathToNodes :: Text -> [MetricNode]
pathToNodes path =
  let segments = T.splitOn "." path
   in zipWith MetricNode (Nothing : map Just (scanl1 (\acc s -> acc <> "." <> s) segments)) segments


buildMetricTree :: [Text] -> [MetricTree]
buildMetricTree metrics = buildTree_ (foldr insertNode Map.empty (concatMap pathToNodes metrics)) Nothing
  where
    insertNode sp = Map.insertWith (\new old -> if sp `elem` old then old else new ++ old) sp.parent [sp]
    buildTree_ nodeMap parentId =
      [MetricTree mt (buildTree_ nodeMap (Just $ nodePath mt)) | mt <- Map.findWithDefault [] parentId nodeMap]


data MetricRow = MetricRow
  { level :: Int
  , segment :: Text
  , parentPath :: Maybe Text
  , fullPath :: Text
  , isGroup :: Bool
  , childCount :: Int
  , continuations :: [Bool]
  , metric :: Maybe Telemetry.MetricDataPoint
  }


flattenMetricTree :: Map Text Telemetry.MetricDataPoint -> [MetricTree] -> Int -> [Bool] -> V.Vector MetricRow
flattenMetricTree dataMap trees lvl conts = foldMap flatten trees
  where
    flatten (MetricTree nd children) =
      let fp = nodePath nd
          row = MetricRow{level = lvl, segment = nd.current, parentPath = nd.parent, fullPath = fp, isGroup = not (null children), childCount = length children, continuations = conts, metric = Map.lookup fp dataMap}
          childIsLast = replicate (length children - 1) False ++ [True]
       in V.cons row $ fold $ zipWith (\c cLast -> flattenMetricTree dataMap [c] (lvl + 1) (conts ++ [not cLast])) children childIsLast


instance ToHtml TraceDetailsGet where
  toHtml (TraceDetails pid tr spanRecs) = toHtml $ tracePage pid tr spanRecs
  toHtml (SpanDetails pid s aptSpn) = toHtml $ LogItem.expandedItemView pid s aptSpn Nothing
  toHtml (TraceDetailsNotFound pid) =
    div_ [class_ "min-h-screen flex items-center justify-center bg-bgBase p-6"]
      $ div_ [class_ "max-w-lg space-y-6 text-center"] do
        div_ [class_ "flex flex-col items-center gap-3"] do
          faSprite_ "circle-exclamation" "solid" "w-6 h-6 text-textWeak"
          div_ [class_ "space-y-1.5"] do
            h1_ [class_ "text-lg font-semibold text-textStrong"] "Couldn't load this trace"
            p_ [class_ "max-w-md text-sm leading-6 text-textWeak"] "Try again. If it still fails, return to your results and search for the trace again."
        div_ [class_ "flex flex-wrap justify-center gap-2"] do
          button_
            [ class_ "btn btn-sm btn-primary"
            , [__|on click send loadTrace(url: #trace_expanded_view's @data-trace-url) to #trace_expanded_view|]
            ]
            "Retry loading trace"
          button_
            [ class_ "btn btn-sm border-0 bg-fillWeaker text-textStrong hover:bg-fillWeak"
            , [__|on click send closeTraceView to #trace_expanded_view|]
            ]
            "Back to search results"
        a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer", class_ "link link-hover text-sm text-textWeak"] "Open Log Explorer"
  toHtmlRaw = toHtml


-- Metrics handlers
metricsOverViewGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders MetricsOverViewGet)
metricsOverViewGetH pid tabM fromM toM sinceM sourceM prefixM cursorM expandM labelM = do
  ctx <- Reader.ask @AuthContext
  (_, _, bw) <- mkPageCtx pid
  now <- Time.currentTime
  let dataPointsTab = any (/= "charts") tabM
      (from, to, currentRange) = parseTime fromM toM sinceM now
      bwconf =
        bw
          { prePageTitle = Just "Explorer"
          , pageTitle = "Metrics"
          , menuItem = Just "Explorer"
          , navTabs = Just $ explorerNavTabs_ pid "Metrics"
          , docsLink = Just "https://monoscope.tech/docs/dashboard/dashboard-pages/metrics/"
          , pageActions = Just $ div_ [class_ "inline-flex gap-2"] do
              TimePicker.timepicker_ Nothing currentRange Nothing
              TimePicker.refreshButton_
          }
  if dataPointsTab
    then do
      dataPoints <- Telemetry.getDataPointsData ctx.env.enableTimefusionReads pid (from, to)
      dashboards <- Dashboards.selectDashboardsSortedBy pid "updated_at"
      monitors <- Monitors.queryMonitorsAll pid
      let refCounts = metricRefCounts dashboards monitors (map (.metricName) dataPoints)
      addRespHeaders $ MetricsOVDataPointMain $ PageCtx bwconf (pid, V.fromList dataPoints, refCounts)
    else do
      let cursor = fromMaybe 0 cursorM
          cutoff = addUTCTime (-(7 * 24 * 3600)) now
          pageSize = 20
      -- Only the requested page comes back, not the whole catalogue. The inactive tail rides
      -- along in the same result set because it falls out of the same aggregate.
      page <- Telemetry.getMetricCatalogPage pid sourceM prefixM cutoff pageSize cursor (cursor == 0)
      let metricList = page.active
          params = foldMap (\(k, v) -> foldMap (("&" <> k <> "=") <>) v) ([("metric_source", sourceM), ("from", fromM), ("to", toM), ("since", sinceM), ("metric_prefix", prefixM)] :: [(Text, Maybe Text)])
          nextFetchUrl = do
            guard $ cursor + pageSize < page.activeTotal
            pure $ "/p/" <> pid.toText <> "/metrics?tab=charts" <> params <> "&cursor=" <> show (cursor + pageSize)
      let labels = Map.fromList $ (\metric -> (metric.metricName, metric.metricLabels)) <$> V.toList metricList
      if cursor == 0
        then do
          dashboards <- Dashboards.selectDashboardsSortedBy pid "updated_at"
          monitors <- Monitors.queryMonitorsAll pid
          -- The full catalogue and the service list are only needed to render an open
          -- drawer, so they are read inside the `forM` rather than on every page load.
          drawerM <- forM expandM \metricName -> do
            metricM <- Telemetry.getMetricData pid metricName
            serviceNames <- V.fromList <$> Telemetry.getMetricServiceNames pid Nothing serviceOptionsLimit
            candidates <- V.fromList <$> Telemetry.getMetricChartListData pid sourceM prefixM
            pure $ metricM <&> \metric ->
              metricsDetailsPage pid serviceNames metric candidates (metricReferences metric.metricName dashboards monitors) (fromMaybe "all" sourceM) (mfilter (`elem` metric.metricLabels) labelM) currentRange
          let bwconf' = bwconf{globalDrawerContent = join drawerM}
          addRespHeaders $ MetricsOVChartsMain $ PageCtx bwconf' (pid, metricList, labels, page.inactive, fromMaybe "all" sourceM, fromMaybe "all" prefixM, page.activeTotal, nextFetchUrl)
        else do
          addRespHeaders $ MetricsOVChartsPaginated (pid, metricList, labels, fromMaybe "all" sourceM, nextFetchUrl)


-- | Options for the metric-source picker, searched and capped server-side.
metricServicesGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
metricServicesGetH pid searchM currentM = do
  void $ Projects.sessionAndProject pid
  services <- Telemetry.getMetricServiceNames pid searchM serviceOptionsLimit
  addRespHeaders $ serviceOptions_ (fromMaybe "all" currentM) services


metricDetailsGetH :: Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
metricDetailsGetH pid metricName fromM toM sinceM source labelM = do
  void $ Projects.sessionAndProject pid
  now <- Time.currentTime
  let (_, _, currentRange) = parseTime fromM toM sinceM now
  metricM <- Telemetry.getMetricData pid metricName
  relatedCandidates <- V.fromList <$> Telemetry.getMetricChartListData pid (mfilter (/= "all") source) Nothing
  dashboards <- Dashboards.selectDashboardsSortedBy pid "updated_at"
  monitors <- Monitors.queryMonitorsAll pid
  addRespHeaders
    $ maybe
      (div_ [class_ "flex flex-col gap-2 -10 text-2xl"] "Metric not found")
      (\metric -> metricsDetailsPage pid metric.serviceNames metric relatedCandidates (metricReferences metric.metricName dashboards monitors) (fromMaybe "all" source) (mfilter (`elem` metric.metricLabels) labelM) currentRange)
      metricM


metricBreakdownGetH :: Projects.ProjectId -> Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
metricBreakdownGetH pid metricName labelM = do
  metricM <- Telemetry.getMetricData pid metricName
  addRespHeaders $ flip foldMap metricM \metric ->
    metricDetailChart pid metric "all" (mfilter (`elem` metric.metricLabels) labelM) ("details_" <> T.replace "." "_" metric.metricName)


-- Trace handler
traceH :: Projects.ProjectId -> Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders TraceDetailsGet)
traceH pid trId timestamp spanIdM nav = do
  useTf <- (.env.enableTimefusionReads) <$> Reader.ask @AuthContext
  withSpan_ "trace.load" [] do
    now <- Time.currentTime
    if isJust nav
      then do
        spanRecords' <- V.fromList <$> Telemetry.getSpanRecordsByTraceId useTf pid trId timestamp now
        let bySpanId i = V.find (\x -> (x.context >>= (.span_id)) == Just i) spanRecords'
        case (spanIdM >>= bySpanId) <|> spanRecords' V.!? 0 of
          Nothing -> addRespHeaders $ TraceDetailsNotFound pid
          Just clicked -> do
            let sdkNear =
                  V.find
                    (\x -> x.name == Just Telemetry.sdkSpanStoredName && x.start_time >= addUTCTime (-1) clicked.start_time && x.start_time <= addUTCTime 1 (LogItem.spanEndOrCap clicked))
                    spanRecords'
                (targetSpan, atpSpan) = runIdentity $ LogItem.anchorSdkSpan (pure . bySpanId) (pure sdkNear) clicked
            addRespHeaders $ SpanDetails pid targetSpan atpSpan
      else do
        traceItemM <- Telemetry.getTraceDetailsForView useTf pid trId timestamp now
        addRespHeaders $ maybe (TraceDetailsNotFound pid) (\(traceItem, spans) -> TraceDetails pid traceItem spans) traceItemM


-- Metrics UI components
overViewTabs :: Projects.ProjectId -> Text -> Html ()
overViewTabs pid tab =
  div_ [class_ "flex items-center gap-2 shrink-0"] do
    span_ [class_ "text-xs font-medium text-textWeak"] "View"
    div_ [class_ "tabs tabs-box tabs-outline", role_ "tablist", Aria.label_ "Metric view"] do
      let viewTab label view =
            a_
              ( [ href_ $ "/p/" <> pid.toText <> "/metrics?tab=" <> view
                , role_ "tab"
                , Aria.selected_ $ bool "false" "true" (tab == view)
                , class_ $ "tab h-8 min-h-8 px-3 text-xs " <> bool "" "tab-active" (tab == view)
                ]
                  <> navTabAttrs
              )
              label
      viewTab "Charts" "charts"
      viewTab "Table" "datapoints"


-- | A "select ... reload with query param" dropdown, shared by the service/metric-group filters.
filterSelect_ :: Foldable f => Text -> Text -> Text -> Text -> Text -> f Text -> (Text -> Html ()) -> Html ()
filterSelect_ widthCls param ariaLbl allLabel current opts display =
  select_
    [ class_ $ "join-item select select-sm bg-bgBase border border-strokeWeak h-10 " <> widthCls <> " max-md:w-1/2 shadow-none cursor-pointer hover:border-strokeStrong transition-colors focus:outline-hidden focus:ring-2 focus:ring-strokeFocus"
    , Aria.label_ ariaLbl
    , onchange_ $ "window.setQueryParamAndReload('" <> param <> "', this.value)"
    ]
    do
      option_ ([selected_ "all" | "all" == current] ++ [value_ "all"]) $ toHtml allLabel
      forM_ opts $ \o -> option_ ([selected_ o | o == current] ++ [value_ o]) $ display o


-- | Service picker for the metric catalogue. A project can have thousands of services, so
-- the list is searched server-side and capped rather than rendered up front — the options
-- only exist once the popover is opened, and only the matching ones.
servicePicker_ :: Projects.ProjectId -> Text -> Html ()
servicePicker_ pid current = div_ [class_ "join-item"] do
  button_
    ( [ class_ "select select-sm bg-bgBase border border-strokeWeak h-10 w-36 max-md:w-1/2 shadow-none cursor-pointer hover:border-strokeStrong transition-colors focus:outline-hidden focus:ring-2 focus:ring-strokeFocus flex items-center"
      , type_ "button"
      , Aria.label_ "Filter by service"
      ]
        <> popoverTrigger_ "metric-service-picker"
    )
    $ span_ [class_ "truncate"]
    $ toHtml
    $ if current == "all" then "All Services" else current
  div_ ([class_ "dropdown bg-bgRaised border border-strokeWeak rounded-lg shadow-lg w-72 p-0"] <> popoverPanel_ "metric-service-picker") do
    input_
      [ class_ "input input-sm w-full border-0 border-b border-strokeWeak rounded-none focus:outline-hidden"
      , type_ "search"
      , placeholder_ "Search services"
      , Aria.label_ "Search services"
      , hxGet_ $ "/p/" <> pid.toText <> "/metrics/services"
      , hxTrigger_ "input changed delay:200ms, load"
      , hxTarget_ "#metric-service-options"
      , name_ "q"
      ]
    div_ [class_ "max-h-72 overflow-y-auto c-scroll", id_ "metric-service-options"] pass


-- | Options for 'servicePicker_'. Rendered by the server so the picker ships no markup
-- until it is opened.
serviceOptions_ :: Text -> [Text] -> Html ()
serviceOptions_ current services = do
  forM_ ("all" : services) \s ->
    button_
      [ class_ $ "w-full text-left px-3 py-1.5 text-sm hover:bg-fillWeak cursor-pointer truncate" <> bool "" " bg-fillWeak font-medium" (s == current)
      , type_ "button"
      , onpointerdown_ $ "window.setQueryParamAndReload('metric_source', '" <> s <> "')"
      ]
      $ toHtml
      $ if s == "all" then "All Services" else s
  when (null services)
    $ div_ [class_ "px-3 py-4 text-xs text-textWeak"] "No matching services."
  -- The list is capped; say so rather than silently showing a prefix of the truth.
  when (length services >= serviceOptionsLimit)
    $ div_ [class_ "px-3 py-2 text-2xs text-textWeak border-t border-strokeWeak"]
    $ toHtml
    $ "Showing the first "
    <> show @Text serviceOptionsLimit
    <> " — type to narrow."


serviceOptionsLimit :: Int
serviceOptionsLimit = 50


chartsPage :: Projects.ProjectId -> V.Vector Telemetry.MetricChartListData -> Map Text (V.Vector Text) -> V.Vector Telemetry.MetricChartListData -> Text -> Text -> Int -> Maybe Text -> Html ()
chartsPage pid metricList labels inactive source mFilter activeCount nextUrl = do
  div_ [class_ "flex flex-col gap-4 px-4 overflow-y-scroll", term "hx-preload" "false"]
    $ do
      div_ [class_ "w-full"] do
        div_ [class_ "w-full flex flex-wrap gap-3 max-md:gap-2 items-center min-h-10 py-2 border-b border-strokeWeak"] do
          overViewTabs pid "charts"
          div_ [class_ "w-px h-6 bg-strokeWeak max-md:hidden"] pass
          let metricNames =
                ordNub
                  $ ( \x ->
                        let sep = if "." `T.isInfixOf` x.metricName then "." else "_"
                         in fst (T.breakOn sep x.metricName) <> sep
                    )
                  <$> V.toList metricList
              stripTrailing t = fromMaybe t $ T.stripSuffix "." t <|> T.stripSuffix "_" t
          div_ [class_ "flex items-center gap-2 shrink-0 max-md:w-full max-md:flex-wrap"] do
            span_ [class_ "text-xs font-medium text-textWeak"] "Scope"
            div_ [class_ "join max-md:w-full"] do
              servicePicker_ pid source
              filterSelect_ "w-auto" "metric_prefix" "Filter by metric group" "All metric groups" mFilter metricNames (toHtml . stripTrailing)
          div_ [class_ "w-px h-6 bg-strokeWeak max-md:hidden"] pass
          label_ [class_ "input input-sm flex grow min-w-0 max-md:w-full max-md:flex-none h-10 bg-bgBase border border-strokeWeak shadow-none overflow-hidden items-center gap-2 hover:border-strokeStrong transition-colors focus-within:outline-hidden focus-within:ring-2 focus-within:ring-strokeFocus focus-within:border-strokeFocus"] do
            faSprite_ "magnifying-glass" "regular" "w-4 h-4 opacity-50"
            input_
              [ class_ "grow"
              , type_ "text"
              , placeholder_ "Search metrics"
              , Aria.label_ "Search metrics"
              , id_ "search-input"
              , [__| on input show .metric_filterble in #metric_list_container when its textContent.toLowerCase() contains my value.toLowerCase() |]
              ]
          span_ [class_ "ml-auto shrink-0 text-xs text-textWeak tabular-nums max-md:ml-0 max-md:w-full", data_ "tippy-content" "Metric names seen in the past 7 days. This catalog is independent of the selected chart range."] $ toHtml $ "Catalog: " <> prettyPrintCount activeCount <> " metrics seen in 7d"
      if V.null metricList && V.null inactive
        then
          div_ [class_ "w-full flex items-center justify-center h-96"]
            $ Components.emptyState_ def{icon = Just "chart-line", action = ESLink "https://monoscope.tech/docs/sdks/" "View SDK setup guides"} "No metrics found" "Metrics will appear here once your application starts sending telemetry data."
        else do
          when (V.null metricList && not (V.null inactive))
            $ div_ [class_ "text-textWeak text-sm py-4"] "No metrics received in the last 7 days."
          unless (V.null metricList)
            $ div_ [class_ "w-full grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4", id_ "metric_list_container"]
            $ chartList pid labels source metricList nextUrl
          unless (V.null inactive) $ inactiveMetricsList pid source inactive


metricDetailUrl :: Projects.ProjectId -> Text -> Text -> Maybe Text -> Text
metricDetailUrl pid metricName source labelM = "/p/" <> pid.toText <> "/metrics/details/" <> metricName <> "/?metric_source=" <> source <> maybe "" ("&label=" <>) labelM


metricExpandUrl :: Projects.ProjectId -> Text -> Text -> Maybe Text -> Text
metricExpandUrl pid metricName source labelM = "/p/" <> pid.toText <> "/metrics?tab=charts&metric_source=" <> source <> "&expand=" <> metricName <> maybe "" ("&label=" <>) labelM


-- | htmx wiring shared by every control that re-renders the metric details drawer.
metricDetailSwap_ :: [Attribute]
metricDetailSwap_ = [hxTarget_ "#metric-details-content", hxSwap_ "outerMorph", hxIndicator_ "#global-data-drawer-indicator"]


-- | htmx wiring for loading a span into the trace details side panel.
spanDetailAttrs_ :: Text -> Text -> UTCTime -> [Attribute]
spanDetailAttrs_ pidT spanUuid ts =
  [ hxGet_ $ "/p/" <> pidT <> "/log_explorer/" <> spanUuid <> "/" <> toText (formatShow iso8601Format ts) <> "/detailed?source=spans"
  , hxTarget_ "#trace_details_content"
  , hxSwap_ "innerHTML"
  , term "hx-on::before-request" "window.showTraceDetailsLoading(this)"
  , hxIndicator_ "#loading-span-list"
  ]


-- | Shared WTTimeseriesLine widget for a metric. @mTitle@/@mId@/@mExpandBtn@
-- carry the per-callsite differences between the chart-list card and the details page.
metricWidget :: Projects.ProjectId -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Widget.Widget
metricWidget pid metricName metricType metricUnit mLabel mTitle mId mExpandBtn =
  def
    { Widget.wType = Widget.WTTimeseriesLine
    , Widget.title = if isDistributionMetricType metricType then (<> " · mean") <$> mTitle else mTitle
    , Widget.query = Just $ metricQuery metricName metricType mLabel
    , Widget.layout = Just $ Widget.Layout{x = Just 0, y = Just 0, w = Just 2, h = Just 1}
    , Widget.unit = Just metricUnit
    , Widget.eager = Just True
    , Widget.hideSubtitle = Just True
    , Widget._projectId = Just pid
    , Widget.id = mId
    , Widget.expandBtnFn = mExpandBtn
    }


isDistributionMetricType :: Text -> Bool
isDistributionMetricType = (`elem` ["HISTOGRAM", "EXPONENTIAL_HISTOGRAM", "SUMMARY"])


metricQuery :: Text -> Text -> Maybe Text -> Text
metricQuery metricName metricType mLabel =
  "metrics | where metric_name == \""
    <> metricName
    <> "\""
    <> bool "" " and distribution_count > 0 and distribution_sum != null" isDistribution
    <> " | summarize "
    <> bool "avg(value)" "sum(distribution_sum) / sum(distribution_count)" isDistribution
    <> " by bin_auto(timestamp)"
    <> maybe "" ("," <>) mLabel
  where
    isDistribution = isDistributionMetricType metricType


chartList :: Projects.ProjectId -> Map Text (V.Vector Text) -> Text -> V.Vector Telemetry.MetricChartListData -> Maybe Text -> Html ()
chartList pid labels source metricList nextUrl = do
  forM_ metricList \metric ->
    metricCard pid source metric.metricName metric.metricType metric.metricUnit (fromMaybe V.empty $ Map.lookup metric.metricName labels) Nothing
  whenJust nextUrl \url ->
    a_ [hxTrigger_ "intersect once", hxSwap_ "outerHTML", hxGet_ url] pass


metricCardGetH :: Projects.ProjectId -> Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
metricCardGetH pid metricName labelM = do
  metricM <- Telemetry.getMetricData pid metricName
  addRespHeaders $ flip foldMap metricM \metric ->
    let selected = mfilter (\l -> l == "all" || l `elem` metric.metricLabels) labelM
     in metricCard pid "all" metric.metricName metric.metricType metric.metricUnit metric.metricLabels selected


metricCard :: Projects.ProjectId -> Text -> Text -> Text -> Text -> V.Vector Text -> Maybe Text -> Html ()
metricCard pid source metricName metricType metricUnit labels selectedM = do
  let selected = if selectedM == Just "all" then Nothing else selectedM <|> labels V.!? 0
      cardId = "metric_" <> T.replace "." "_" metricName
      detailUrl = metricDetailUrl pid metricName source selected
  div_ [class_ "w-full flex flex-col gap-2 metric_filterble", id_ cardId]
    $ div_ [class_ "h-56"]
    $ toHtml
    $ (metricWidget pid metricName metricType metricUnit selected (Just metricName) Nothing (Just detailUrl))
      { Widget.expandPushUrl = Just $ metricExpandUrl pid metricName source selected
      , Widget.groupByOptions = V.toList labels <$ guard (not $ V.null labels)
      , Widget.groupBySelected = selectedM <|> selected
      , Widget.groupByUrl = Just $ "/p/" <> pid.toText <> "/metrics/card/" <> metricName
      , Widget.groupByTarget = Just $ "#" <> cardId
      }


inactiveMetricsList :: Projects.ProjectId -> Text -> V.Vector Telemetry.MetricChartListData -> Html ()
inactiveMetricsList pid source metrics = do
  details_ [class_ "collapse collapse-arrow bg-bgRaised border border-strokeWeak mt-4"] do
    summary_ [class_ "collapse-title font-medium text-sm text-textWeak"]
      $ toHtml
      $ show (V.length metrics)
      <> " inactive metric"
      <> bool "s" "" (V.length metrics == 1)
      <> " (no data in 7 days)"
    div_ [class_ "collapse-content"] do
      div_ [class_ "flex flex-col divide-y divide-strokeWeak"] do
        forM_ metrics $ \metric ->
          div_
            (class_ "flex items-center justify-between py-2 px-2 cursor-pointer hover:bg-fillWeak rounded" : drawerLoadAttrs_ (metricDetailUrl pid metric.metricName source Nothing))
            do
              div_ [class_ "flex items-center gap-2"] do
                faSprite_ "chart-line" "regular" "w-3.5 h-3.5 text-textWeak"
                span_ [class_ "text-sm font-mono"] $ toHtml metric.metricName
              span_ [class_ "text-xs text-textWeak"] $ toHtml $ "Last seen " <> formatTime defaultTimeLocale "%b %d, %Y" metric.lastSeen


dataPointsPage :: Projects.ProjectId -> V.Vector Telemetry.MetricDataPoint -> Map Text (Int, Int, Int) -> Html ()
dataPointsPage pid metrics refCounts = do
  let dataMap = Map.fromList [(m.metricName, m) | m <- V.toList metrics]
      tree = buildMetricTree $ V.toList $ (.metricName) <$> metrics
      rows = flattenMetricTree dataMap tree 0 []
  div_ [class_ "flex flex-col gap-4 px-4 overflow-y-scroll"] $ do
    div_ [class_ "w-full"] do
      Components.drawer_ "global-data-drawer" False Nothing Nothing ""
      template_ [id_ "loader-tmp"] $ loadingIndicator_ LdMD LdDots
      div_ [class_ "w-full flex gap-3 items-center min-h-10"] do
        overViewTabs pid "datapoints"
    toHtml
      Table
        { config = def{Table.elemID = "dataPointsTable", Table.showHeader = True, Table.renderAsTable = True, Table.noDividers = True}
        , columns =
            [ Table.col "Metric" \r -> do
                let indent = 28 :: Int
                div_ [class_ "flex items-center relative", style_ "min-height:20px"] do
                  when (r.level > 0) $ forM_ (zip [0 ..] r.continuations) \(i, continues) -> do
                    let px = show (i * indent + 12) <> "px"
                        isLast = i >= r.level - 1
                    when (continues || isLast)
                      $ div_ [class_ $ "absolute top-0 border-l border-l-strokeWeak " <> bool "bottom-0" "h-1/2" (isLast && not continues), style_ $ "left:" <> px] pass
                    when isLast
                      $ div_ [class_ "absolute h-[1px] bg-strokeWeak", style_ $ "left:" <> px <> "; top:50%; width:16px"] pass
                  div_ [class_ "flex items-center gap-2", style_ $ "padding-left:" <> show (r.level * indent) <> "px"] do
                    div_ [class_ "w-10 shrink-0"]
                      $ when r.isGroup
                      $ div_ [class_ "w-full border border-strokeWeak flex justify-between gap-1 items-center rounded-sm px-1 py-0.5"] do
                        faSprite_ "chevron-right" "regular" "h-3 w-3 shrink-0 text-textStrong tree-chevron rotate-90 transition-transform"
                        span_ [class_ "text-xs"] $ toHtml $ show r.childCount
                    whenJust r.parentPath \p -> span_ [class_ "text-textDisabled"] $ toHtml $ p <> "."
                    case r.metric of
                      Nothing -> span_ [class_ "text-textStrong font-medium"] $ toHtml r.segment
                      Just _ ->
                        a_
                          ( [ class_ "cursor-pointer font-medium text-textStrong hover:text-textBrand focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-strokeFocus"
                            , href_ $ "/p/" <> pid.toText <> "/metrics?tab=datapoints&expand=" <> r.fullPath
                            , [__|on keydown[key=='Enter'] halt the event then trigger click end|]
                            ]
                              <> drawerLoadAttrs_ (metricDetailUrl pid r.fullPath "all" Nothing)
                          )
                          $ toHtml r.segment
            , Table.col "Sources" (\r -> div_ [class_ "flex gap-1 flex-wrap"] $ whenJust r.metric \m -> forM_ m.serviceNames $ span_ [class_ "badge badge-ghost text-xs"] . toHtml) & Table.withAttrs [class_ "w-48"]
            , Table.col "Datapoints" (\r -> whenJust r.metric \m -> span_ [class_ "tabular-nums"] $ toHtml $ prettyPrintCount m.dataPointsCount) & Table.withAttrs [class_ "w-28"]
            , Table.col
                "Referenced in"
                ( \r -> whenJust r.metric \_ -> do
                    let (d, w, a) = fromMaybe (0, 0, 0) $ Map.lookup r.fullPath refCounts
                    div_ [class_ "flex gap-2 items-center text-xs"] do
                      span_ [class_ "flex gap-1 items-center badge badge-ghost", data_ "tippy-content" "Dashboards"] do faSprite_ "grid" "regular" "w-3 h-3"; toHtml $ show d
                      span_ [class_ "flex gap-1 items-center badge badge-ghost", data_ "tippy-content" "Widgets"] do faSprite_ "chart-line" "regular" "w-3 h-3"; toHtml $ show w
                      span_ [class_ "flex gap-1 items-center badge badge-ghost", data_ "tippy-content" "Monitors"] do faSprite_ "bell" "regular" "w-3 h-3"; toHtml $ show a
                )
                & Table.withAttrs [class_ "w-40"]
            ]
        , rows
        , features =
            def
              { Table.search = Just Table.ClientSide
              , Table.treeConfig = Just Table.TreeConfig{rowLevel = (.level), rowPath = (.fullPath), isGroupRow = (.isGroup)}
              , Table.zeroState = Just Table.ZeroState{icon = "chart-line", title = "No metrics found", description = "Metrics will appear here once your application starts sending telemetry data.", actionText = "View SDK setup guides", destination = Right "https://monoscope.tech/docs/sdks/"}
              }
        }


metricsDetailsPage :: Projects.ProjectId -> V.Vector Text -> Telemetry.MetricDataPoint -> V.Vector Telemetry.MetricChartListData -> ([Dashboards.DashboardVM], [Monitors.QueryMonitor]) -> Text -> Maybe Text -> Maybe (Text, Text) -> Html ()
metricsDetailsPage pid sources metric candidates (dashboards, monitors) source selected currentRange = do
  let refreshId = "metric-details-chart-refresh"
      chartId = "details_" <> T.replace "." "_" metric.metricName
      labelPriority label
        | "attributes." `T.isPrefixOf` label = 0 :: Int
        | "resource.service." `T.isPrefixOf` label = 1
        | otherwise = 2
      sortedLabels = sortOn (labelPriority &&& id) $ V.toList metric.metricLabels
      dimensions = maybe sortedLabels (\label -> label : filter (/= label) sortedLabels) selected
      (topDimensions, moreDimensions) = splitAt 4 dimensions
      dimensionChips ds = div_ [class_ "mt-2 flex flex-wrap gap-1.5"] $ forM_ ds $ metricDimension pid metric.metricName source selected
  div_
    ( [ id_ "metric-details-content"
      , class_ "flex flex-col gap-5 h-full"
      , hxGet_ $ metricDetailUrl pid metric.metricName source selected
      , hxTrigger_ "update-query from:window"
      , term "hx-ext" "forward-page-params"
      , [__|on htmx:afterSwap if event.target is me call window.evalScriptsFromContent(me) end end|]
      ]
        <> metricDetailSwap_
    )
    do
      div_ [class_ "sticky top-0 z-50 -mx-8 px-8 py-3 bg-bgBase border-b border-strokeWeak"] do
        div_ [class_ "flex flex-wrap items-center justify-between gap-3"] do
          div_ [class_ "min-w-0"] do
            span_ [class_ "block truncate text-sm font-semibold text-textStrong", title_ metric.metricName] $ toHtml metric.metricName
          div_ [class_ "flex flex-wrap items-center gap-2"] do
            select_
              ( [ class_ "select select-sm cursor-pointer bg-bgRaised text-textStrong border border-strokeStrong rounded-lg w-36 focus:outline-hidden focus:ring-2 focus:ring-strokeFocus max-md:h-11"
                , Aria.label_ "Metric data source"
                , hxGet_ $ "/p/" <> pid.toText <> "/metrics/details/" <> metric.metricName <> "/"
                , name_ "metric_source"
                ]
                  <> metricDetailSwap_
              )
              do
                option_ ([selected_ "all" | "all" == source] ++ [value_ "all"]) "All sources"
                forM_ sources $ \s -> option_ ([selected_ s | s == source] ++ [value_ s]) $ toHtml s
            TimePicker.timepicker_ (Just refreshId) currentRange (Just "metric-details")
            TimePicker.refreshButton_
            label_ [class_ "btn btn-ghost btn-circle btn-sm cursor-pointer tap-target text-iconNeutral hover:text-iconBrand", Aria.label_ "Close metric detail", data_ "tippy-content" "Close metric detail", Lucid.for_ "global-data-drawer"] $ faSprite_ "xmark" "regular" "w-3 h-3"
        div_ [id_ refreshId, class_ "hidden", [__|on submit trigger 'update-query' on window|]] ""
      metricDetailChart pid metric source selected chartId

      div_ [class_ "flex flex-col gap-2 rounded-2xl border border-strokeWeak", id_ "metric-tabs-container"] $ do
        div_ [class_ "flex", [__|on click halt|]] $ do
          button_ [class_ "cursor-pointer a-tab border-b border-b-strokeWeak px-4 py-1.5 t-tab-active focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-strokeFocus max-md:min-h-11", onclick_ "navigatable(this, '#ov-content', '#metric-tabs-container', 't-tab-active')"] "Overview"
          button_ [class_ "cursor-pointer a-tab border-b w-max whitespace-nowrap border-b-strokeWeak px-4 py-1.5 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-strokeFocus max-md:min-h-11", onclick_ "navigatable(this, '#rl-content', '#metric-tabs-container', 't-tab-active')"] "Related metrics"
          div_ [class_ "w-full border-b border-b-strokeWeak"] pass

        div_ [class_ "grid px-4 pb-4 mt-2 text-textWeak font-normal"] $ do
          div_ [class_ "a-tab-content", id_ "ov-content"] $ do
            div_ [class_ "flex flex-col gap-4"] do
              div_ [class_ "flex flex-wrap gap-x-6 gap-y-3 text-sm"] do
                div_ [class_ "flex flex-col gap-0.5"] do
                  span_ [class_ "text-xs font-medium text-textWeak"] "Type"
                  span_ [class_ "text-textStrong"] $ toHtml metric.metricType
                div_ [class_ "flex flex-col gap-0.5"] do
                  span_ [class_ "text-xs font-medium text-textWeak"] "Unit"
                  span_ [class_ "text-textStrong"] $ toHtml if metric.metricUnit == "" then "Unit not reported" else metric.metricUnit
                div_ [class_ "flex flex-col gap-0.5"] do
                  span_ [class_ "text-xs font-medium text-textWeak"] "Reporting services"
                  span_ [class_ "text-textStrong"] $ toHtml $ prettyPrintCount (V.length sources)
                div_ [class_ "flex flex-col gap-0.5"] do
                  span_ [class_ "text-xs font-medium text-textWeak"] "Available dimensions"
                  span_ [class_ "text-textStrong"] $ toHtml $ prettyPrintCount (V.length metric.metricLabels)
              when (metric.metricDescription /= "")
                $ div_ [class_ "border-t border-strokeWeak pt-3 text-sm leading-6"]
                $ toHtml metric.metricDescription
              unless (null dimensions)
                $ div_ [class_ "border-t border-strokeWeak pt-4"] do
                  span_ [class_ "text-xs font-medium text-textWeak"] "Dimensions"
                  span_ [class_ "mt-1 block text-xs text-textWeak"] "Choose a dimension to split the chart."
                  dimensionChips topDimensions
                  unless (null moreDimensions)
                    $ details_ [class_ "mt-3"] do
                      summary_ [class_ "inline-flex cursor-pointer items-center text-sm text-textWeak hover:text-textStrong focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-strokeFocus max-md:min-h-11 max-md:text-sm"] $ "Show " <> toHtml (prettyPrintCount $ length moreDimensions) <> " more dimensions"
                      dimensionChips moreDimensions
              unless (V.null sources)
                $ details_ [class_ "border-t border-strokeWeak pt-4"] do
                  summary_ [class_ "inline-flex cursor-pointer items-center text-sm text-textWeak hover:text-textStrong focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-strokeFocus max-md:min-h-11 max-md:text-sm"] do
                    "Reporting services"
                    span_ [class_ "ml-1 text-textDisabled"] $ "(" <> toHtml (prettyPrintCount $ V.length sources) <> ")"
                  div_ [class_ "mt-2 flex flex-wrap gap-1.5"] $ forM_ sources $ \service ->
                    span_ [class_ "badge badge-ghost font-normal"] $ toHtml service
              unless (null dashboards && null monitors)
                $ div_ [class_ "border-t border-strokeWeak pt-4"] do
                  span_ [class_ "text-xs font-medium text-textWeak"] "Used by"
                  div_ [class_ "mt-2 flex flex-col divide-y divide-strokeWeak border-y border-strokeWeak"]
                    $ forM_ (map (\d -> ("dashboards/" <> d.id.toText, d.title)) dashboards <> map (\m -> ("monitors/alerts/" <> m.id.toText, "Monitor")) monitors)
                    $ \(path, label) ->
                      a_ [class_ "flex cursor-pointer items-center justify-between gap-3 px-1 py-2 text-sm hover:text-textBrand focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-strokeFocus", href_ $ "/p/" <> pid.toText <> "/" <> path] do
                        span_ [class_ "truncate text-textStrong"] $ toHtml label
                        faSprite_ "arrow-up-right-from-square" "regular" "w-3 shrink-0 text-iconNeutral"
          div_ [class_ "hidden a-tab-content", id_ "rl-content"] $ relatedMetrics pid source metric candidates


metricDimension :: Projects.ProjectId -> Text -> Text -> Maybe Text -> Text -> Html ()
metricDimension pid metricName source selected label =
  button_
    ( [ class_ $ "badge cursor-pointer font-normal hover:border-strokeStrong hover:bg-fillWeak focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-strokeFocus max-md:min-h-11 max-md:text-sm " <> bool "badge-ghost" "border-strokeStrong bg-fillWeak text-textStrong" (selected == Just label)
      , hxGet_ $ metricDetailUrl pid metricName source (Just label)
      , data_ "tippy-content" $ "Group chart by " <> label
      ]
        <> metricDetailSwap_
    )
    $ toHtml label


relatedMetrics :: Projects.ProjectId -> Text -> Telemetry.MetricDataPoint -> V.Vector Telemetry.MetricChartListData -> Html ()
relatedMetrics pid source metric candidates =
  case take 6 $ map snd $ sortWith (Down . fst) [(score, c) | c <- V.toList candidates, c.metricName /= metric.metricName, let score = relatedMetricScore metric c, score > 0] of
    [] -> div_ [class_ "px-5 py-8 text-sm text-textWeak"] $ if source == "all" then "No metrics with a similar name or dimensions were found." else "No similar metrics in this source. Try All sources."
    related -> do
      div_ [class_ "px-5 pb-3 pt-5"] do
        span_ [class_ "block text-sm font-semibold text-textStrong"] "Related metrics"
        span_ [class_ "mt-1 block text-xs leading-5 text-textWeak"] "Metrics with a similar name or dimensions."
      div_ [class_ "flex flex-col divide-y divide-strokeWeak border-y border-strokeWeak"] $ forM_ related $ \candidate ->
        a_
          ( [ class_ "group flex cursor-pointer items-center justify-between gap-4 px-5 py-4 transition-colors hover:bg-fillWeak focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-strokeFocus"
            , href_ $ metricDetailUrl pid candidate.metricName source Nothing
            , hxGet_ $ metricDetailUrl pid candidate.metricName source Nothing
            ]
              <> metricDetailSwap_
          )
          do
            div_ [class_ "min-w-0"] do
              span_ [class_ "block truncate font-medium text-textStrong"] $ toHtml candidate.metricName
              div_ [class_ "mt-1 flex flex-wrap items-center gap-x-2 gap-y-1 text-xs text-textWeak"] do
                span_ $ toHtml $ candidate.metricType <> if candidate.metricUnit == "" then "" else " · " <> candidate.metricUnit
                span_ [class_ "text-textDisabled"] "•"
                span_ $ toHtml $ relatedMetricContext metric candidate
              when (candidate.metricDescription /= "")
                $ span_ [class_ "mt-1 block truncate text-xs text-textWeak"]
                $ toHtml candidate.metricDescription
            faSprite_ "arrow-right" "regular" "w-3 shrink-0 text-iconNeutral transition-transform group-hover:translate-x-0.5"


-- | Common leading dot-segments of two metric names, shared by scoring and display.
sharedNameSegments :: Text -> Text -> [Text]
sharedNameSegments a b = map fst $ takeWhile (uncurry (==)) $ zip (T.splitOn "." a) (T.splitOn "." b)


sharedLabelCount :: Telemetry.MetricDataPoint -> Telemetry.MetricChartListData -> Int
sharedLabelCount metric candidate = length $ S.intersection (S.fromList $ V.toList metric.metricLabels) (S.fromList $ V.toList candidate.metricLabels)


relatedMetricContext :: Telemetry.MetricDataPoint -> Telemetry.MetricChartListData -> Text
relatedMetricContext metric candidate =
  namespace <> sharedDimensions
  where
    shared = sharedNameSegments metric.metricName candidate.metricName
    namespace = if null shared then "Similar dimensions" else "Same " <> T.intercalate "." shared <> " namespace"
    sharedAttributesCount = sharedLabelCount metric candidate
    sharedDimensions = if sharedAttributesCount == 0 then "" else " · " <> prettyPrintCount sharedAttributesCount <> " shared dimensions"


relatedMetricScore :: Telemetry.MetricDataPoint -> Telemetry.MetricChartListData -> Int
relatedMetricScore metric candidate =
  100 * length (sharedNameSegments metric.metricName candidate.metricName) + sharedLabelCount metric candidate


metricDetailChart :: Projects.ProjectId -> Telemetry.MetricDataPoint -> Text -> Maybe Text -> Text -> Html ()
metricDetailChart pid metric source selected chartId =
  div_ [class_ "h-72 w-full", id_ $ chartId <> "-container"]
    $ toHtml
    $ (metricWidget pid metric.metricName metric.metricType metric.metricUnit selected Nothing (Just chartId) Nothing)
      { Widget.groupByOptions = V.toList metric.metricLabels <$ guard (not $ V.null metric.metricLabels)
      , Widget.groupBySelected = selected
      , Widget.groupByUrl = Just $ metricDetailUrl pid metric.metricName source Nothing
      , Widget.groupByTarget = Just "#metric-details-content"
      }


-- Metric reference counting: (dashboardCount, widgetCount, alertCount) per metric
metricRefCounts :: [Dashboards.DashboardVM] -> [Monitors.QueryMonitor] -> [Text] -> Map Text (Int, Int, Int)
metricRefCounts dashboards monitors = Map.fromList . map countRefs
  where
    countRefs metricName =
      let (matchingDashboards, matchingMonitors) = metricReferences metricName dashboards monitors
       in (metricName, (length matchingDashboards, sum (countWidgetsWithMetric metricName <$> matchingDashboards), length matchingMonitors))


metricReferences :: Text -> [Dashboards.DashboardVM] -> [Monitors.QueryMonitor] -> ([Dashboards.DashboardVM], [Monitors.QueryMonitor])
metricReferences metricName dashboards monitors = (filter ((> 0) . countWidgetsWithMetric metricName) dashboards, filter (monitorHasMetric metricName) monitors)


countWidgetsWithMetric :: Text -> Dashboards.DashboardVM -> Int
countWidgetsWithMetric metricName dashboard =
  sum $ map (fromEnum . widgetRefsMetric metricName) $ flip foldMap dashboard.schema \schema -> schema.widgets <> foldMap (concatMap (.widgets)) schema.tabs


monitorHasMetric :: Text -> Monitors.QueryMonitor -> Bool
monitorHasMetric metricName monitor = any (`T.isInfixOf` monitor.logQuery) ["\"" <> metricName <> "\"", "'" <> metricName <> "'", "metric=" <> metricName, "metric_name=" <> metricName]


widgetRefsMetric :: Text -> Widget.Widget -> Bool
widgetRefsMetric metricName widget =
  let allTexts = maybeToList widget.query <> maybeToList widget.sql
      quoted = "\"" <> metricName <> "\""
   in any (T.isInfixOf quoted) allTexts || any (widgetRefsMetric metricName) (fromMaybe [] widget.children)


-- Trace UI components
traceDetailsLoading_ :: Html ()
traceDetailsLoading_ =
  div_ [class_ "trace-details-loading hidden p-4", role_ "status", Aria.live_ "polite"] do
    div_ [class_ "flex items-center gap-2 text-sm font-medium text-textStrong"] do
      loadingIndicator_ LdSM LdSpinner
      "Loading span details…"
    div_ [class_ "mt-4 space-y-3"] do
      div_ [class_ "h-20 rounded-lg skeleton-shimmer"] ""
      div_ [class_ "h-8 w-full rounded skeleton-shimmer"] ""
      div_ [class_ "h-4 w-3/4 rounded skeleton-shimmer"] ""
      div_ [class_ "h-4 w-full rounded skeleton-shimmer"] ""


tracePage :: Projects.ProjectId -> Telemetry.Trace -> V.Vector Telemetry.SpanRecord -> Html ()
tracePage pid traceItem rawSpanRecords = do
  -- Collapse once so every tab (waterfall, timeline, services list) hides the SDK row.
  let spanRecords = collapseSdkSpans rawSpanRecords
      serviceDurations = Map.fromListWith (+) [(getServiceName sp.resource, sp.spanDurationNs) | sp <- V.toList spanRecords]
      serviceNames = V.fromList $ ordNub $ V.toList $ getServiceName . (.resource) <$> spanRecords
      serviceColors = getServiceColors serviceNames
      rootSpans = buildSpanTree spanRecords
      -- rangeSeconds is 0: throughput is meaningless for a single request, so the map
      -- reports counts and durations only. Node size comes from the duration share.
      (traceHops, traceDurationNs) = ServiceGraph.traceEdgeSamples spanRecords
      traceGraph = ServiceGraph.buildServiceGraph 0 serviceMapNodeCap (Just traceDurationNs) traceHops
  div_ [class_ "w-full h-full flex overflow-hidden", id_ "trace_span_container"] $ do
    div_ [class_ "flex flex-col grow min-w-0 gap-4 p-2 pb-4 overflow-y-auto overflow-x-hidden c-scroll"] $ do
      div_ [class_ "flex flex-wrap justify-between items-center gap-y-1"] do
        div_ [class_ "flex items-center gap-3"] $ do
          button_
            [ class_ "cursor-pointer hidden [#apiLogsPage_&]:flex items-center gap-1.5 text-sm font-medium text-textBrand"
            , term "data-share-hide" ""
            , term "aria-label" "Back to event details"
            , [__|on click send closeTraceView to #trace_expanded_view|]
            ]
            (faSprite_ "chevron-left" "regular" "w-3.5 h-3.5" >> "Back")
          h3_ [class_ "whitespace-nowrap font-semibold text-textStrong"] "Trace Breakdown"
        div_ [class_ "flex items-center gap-2 ml-auto shrink-0"] $ do
          Components.dateTime traceItem.traceStartTime (Just traceItem.traceEndTime)
          div_ [class_ "flex gap-1 items-center"] do
            button_
              [ class_ "fs-trace-toggle cursor-pointer rounded-md p-1 hover:bg-fillWeak transition-colors hidden md:[#apiLogsPage_&]:block tooltip tooltip-bottom"
              , term "data-share-hide" ""
              , term "aria-label" "Toggle fullscreen"
              , data_ "tip" "Expand trace"
              , [__|on click send toggleFullscreen(mode: 'trace') to #apiLogsPage|]
              ]
              do
                faSprite_ "expand" "regular" "w-3.5 h-3.5 text-iconNeutral [#apiLogsPage[data-fullscreen=trace]_&]:hidden!"
                faSprite_ "compress" "regular" "hidden! w-3.5 h-3.5 text-iconNeutral [#apiLogsPage[data-fullscreen=trace]_&]:block!"
            button_
              [ class_ "cursor-pointer rounded-md p-1 hover:bg-fillWeak transition-colors hidden [#apiLogsPage_&]:block tooltip tooltip-left"
              , term "data-share-hide" ""
              , term "aria-label" "Close details"
              , data_ "tip" "Close"
              , [__|on click send closeDetailPanel to #log_details_container|]
              ]
              $ faSprite_ "xmark" "regular" "w-3.5 h-3.5 text-iconNeutral"

      div_ [class_ "flex gap-1 w-full max-md:mt-2 mt-5"] $ do
        div_ [role_ "tablist", class_ "w-full flex flex-col gap-2", id_ "trace-tabs"] $ do
          div_ [class_ "flex flex-col gap-2"] do
            div_ [class_ "flex flex-wrap justify-between gap-y-1 mb-2"] do
              div_ [class_ "flex items-center gap-2 text-textWeak font-medium min-w-0 overflow-x-auto"] do
                button_ [class_ "a-tab text-sm px-3 py-1.5 border-b-2 border-b-transparent whitespace-nowrap shrink-0 t-tab-active", onpointerdown_ "navigatable(this, '#water_fall', '#trace-tabs', 't-tab-active')"] "Waterfall"
                button_ [class_ "a-tab text-sm px-3 border-b-2 border-b-transparent py-1.5 whitespace-nowrap shrink-0", onpointerdown_ "navigatable(this, '#flame_graph', '#trace-tabs', 't-tab-active')"] "Timeline"
                button_ [class_ "a-tab text-sm px-3 border-b-2 border-b-transparent py-1.5 whitespace-nowrap shrink-0", onpointerdown_ "navigatable(this, '#span_list', '#trace-tabs', 't-tab-active')"] "Services"
                button_ [class_ "a-tab text-sm px-3 border-b-2 border-b-transparent py-1.5 whitespace-nowrap shrink-0", onpointerdown_ "navigatable(this, '#service_map', '#trace-tabs', 't-tab-active')"] "Map"
              div_ [class_ "flex items-center gap-2 shrink-0"] do
                stBox "Spans" (show $ length spanRecords) Nothing
                stBox "Errors" (show $ length $ V.filter (\s -> s.status == Just SSError) spanRecords) $ Just (faSprite_ "alert-triangle" "regular" "w-3 h-3 text-iconError")
                stBox "Duration" (toText $ getDurationNSMS traceItem.traceDurationNs) $ Just (faSprite_ "clock" "regular" "w-3 h-3 text-iconNeutral")
            div_ [class_ "flex gap-2 w-full items-center"] do
              div_ [class_ "flex items-center gap-2 w-full rounded-lg px-3 grow-1 h-9 border border-strokeWeak bg-fillWeaker"] do
                faSprite_ "magnifying-glass" "regular" "w-3 h-3 text-iconNeutral"
                input_
                  [ class_ "w-full text-textStrong bg-transparent hover:outline-hidden focus:outline-hidden focus:ring-0"
                  , type_ "text"
                  , placeholder_ "Search"
                  , id_ "search-input"
                  , [__| on input show .span-filterble in #trace_span_container when its textContent.toLowerCase() contains my value.toLowerCase() |]
                  ]
                -- Span ids live on the container so the two buttons don't each embed the whole list.
                div_ [class_ "flex items-center gap-1", id_ "currentSpanIndex", term "data-span" "0", term "data-span-ids" $ decodeUtf8 $ AE.encode $ (.spanId) <$> spanRecords] do
                  button_
                    [ class_ "h-6 w-6 flex items-center justify-center bg-fillWeaker rounded-full font-bold border border-strokeWeak text-textStrong  cursor-pointer"
                    , onpointerdown_ "navigateSpans('prev')"
                    ]
                    $ faSprite_ "chevron-up" "regular" "w-3 h-3"
                  button_
                    [ class_ "h-6 w-6 flex items-center justify-center rounded-full bg-fillWeaker font-bold border border-strokeWeak text-textStrong cursor-pointer"
                    , onpointerdown_ "navigateSpans('next')"
                    ]
                    $ faSprite_ "chevron-down" "regular" "h-3 w-3"
              button_ [class_ "btn border border-strokeWeak bg-fillWeaker h-9 hidden", id_ "reset-zoom-btn"] "Reset Zoom"
          div_ [role_ "tabpanel", class_ "a-tab-content w-full hidden", id_ "flame_graph"] do
            div_ [class_ "flex max-md:flex-col gap-2 w-full pt-2 relative", style_ "--tl-left:65%", id_ $ "timeline-layout-" <> traceItem.traceId] do
              div_
                [ class_ "md:w-[var(--tl-left)] w-full group px-2 pt-4 border relative flex flex-col rounded-lg overflow-hidden"
                , id_ $ "flame-graph-container-" <> traceItem.traceId
                ]
                do
                  div_ [class_ "w-full sticky top-0 border-b border-b-strokeWeak h-6 text-xs relative", id_ $ "time-container-" <> traceItem.traceId] pass
                  div_ [class_ "w-full overflow-x-hidden min-h-56 h-full relative", id_ $ "a" <> traceItem.traceId] pass
                  div_ [class_ "h-full top-0 absolute z-50 hidden", id_ $ "time-bar-indicator-" <> traceItem.traceId] do
                    div_ [class_ "relative h-full"] do
                      div_ [class_ "text-xs top-[-18px] absolute -translate-x-1/2 whitespace-nowrap", id_ $ "line-time-" <> traceItem.traceId] "2 ms"
                      div_ [class_ "h-[calc(100%-24px)] mt-[24px] w-[1px] bg-strokeWeak"] pass
              resizeDivider_ "--tl-left" 30 85 "resize" "max-md:hidden top-2"
              div_ [class_ "border rounded-lg md:w-[calc(100%-var(--tl-left)-8px)] w-full overflow-x-hidden"] do
                h3_ [class_ "w-full flex px-3 py-2 font-medium justify-between items-center text-xs text-textWeak border-b"] do
                  span_ [] "Services"
                  span_ [] "Exec Time %"
                div_ [class_ "w-full overflow-x-hidden text-textWeak text-xs", id_ $ "services-" <> traceItem.traceId] do
                  let allDur = sum serviceDurations
                  forM_ serviceNames $ \s -> do
                    let duration = Map.findWithDefault 0 s serviceDurations
                        percent = show $ (fromIntegral duration / fromIntegral allDur) * 100
                        color = getServiceColor s serviceColors
                    div_ [class_ "flex items-center justify-between px-2 py-1"] $ do
                      div_ [class_ "flex gap-1 items-center"] $ do
                        div_ [class_ $ "w-2.5 h-2.5 rounded-sm " <> color] pass
                        span_ [] $ toHtml s
                      div_ [class_ "flex gap-1 items-center tabular-nums"] $ do
                        span_ [class_ "max-w-52 truncate"] $ toHtml $ T.take 4 percent <> "%"
                        div_ [class_ "w-[80px] h-2 bg-fillWeak rounded-sm overflow-hidden"]
                          $ div_ [class_ $ "h-full " <> color, style_ $ "width:" <> percent <> "%"] pass

          div_ [role_ "tabpanel", class_ "a-tab-content pt-2", id_ "water_fall"] do
            when (length serviceNames > 1)
              $ div_ [class_ "flex flex-wrap items-center gap-x-4 gap-y-1 px-1 pb-2 text-xs text-textWeak"]
              $ forM_ serviceNames
              $ \s ->
                div_ [class_ "flex items-center gap-1.5"] do
                  div_ [class_ $ "w-2.5 h-2.5 rounded-full shrink-0 " <> getServiceColor s serviceColors] pass
                  span_ [class_ "truncate max-w-40"] $ toHtml s
            div_ [class_ "border border-strokeWeak w-full rounded-2xl min-h-[230px] overflow-y-auto overflow-x-hidden relative", style_ "--wf-left:35%", id_ $ "waterfall-container-" <> traceItem.traceId] do
              resizeDivider_ "--wf-left" 15 70 "waterfallResize" "waterfall-divider"
              div_ [class_ "h-full top-0 absolute z-30 hidden pointer-events-none", id_ $ "wf-time-indicator-" <> traceItem.traceId] do
                div_ [class_ "relative h-full"] do
                  div_ [class_ "text-xs top-1 absolute -translate-x-1/2 whitespace-nowrap bg-bgOverlay px-1 rounded text-textStrong", id_ $ "wf-time-label-" <> traceItem.traceId] ""
                  div_ [class_ "h-full w-px bg-strokeBrand-strong mt-5"] pass
              div_ [class_ "flex sticky top-0 z-10 bg-bgBase border-b border-b-strokeWeak h-8 text-xs items-end"] do
                div_ [class_ "shrink-0 px-2 pb-1 text-textWeak font-medium", style_ "width:var(--wf-left)"] "Service / Span"
                div_ [class_ "relative grow min-w-0", id_ $ "waterfall-time-container-" <> traceItem.traceId] pass
              div_ [class_ "py-1", id_ $ "waterfall-rows-" <> traceItem.traceId] do
                forM_ rootSpans \c -> buildSpanTree_ pid c 0 serviceColors

          div_ [role_ "tabpanel", class_ "a-tab-content pt-2 hidden", id_ "span_list"] do
            div_ [class_ "border border-strokeWeak w-full rounded-2xl min-h-[230px] overflow-x-hidden "] do
              renderSpanListTable serviceNames serviceColors spanRecords

          -- How this one request moved through the system: the same graph grammar as the
          -- global service map, scoped to this trace's spans (no extra query — the spans
          -- are already on the page).
          div_ [role_ "tabpanel", class_ "a-tab-content pt-2 hidden", id_ "service_map"] do
            serviceMapPanel_ pid ("trace-service-map-" <> traceItem.traceId) traceGraph serviceColors
    -- Same grip affordance as resizer_ (border + centred dots, brand-coloured on hover):
    -- the drag target was invisible without it, so the panel read as unresizable.
    div_ [class_ "hidden shrink-0 max-md:hidden", id_ "trace-details-resizer-wrapper"]
      $ div_
        [ class_ "group relative w-3 h-full flex items-center justify-center cursor-ew-resize overflow-visible select-none touch-none"
        , id_ "trace_details_resizer"
        , role_ "separator"
        , Aria.label_ "Resize span details"
        ]
      $ div_ [class_ "h-full border-l border-strokeWeak group-hover:border-strokeBrand-strong"]
      $ div_ [class_ "absolute left-1/2 top-1/2 z-10 -translate-x-1/2 leading-none py-1 -translate-y-1/2 bg-bgBase rounded-sm border border-strokeBrand-weak group-hover:border-strokeBrand-strong text-iconNeutral group-hover:text-iconBrand"]
      $ faSprite_ "grip-dots-vertical" "regular" "w-4 h-5"
    div_
      [ class_ "details-panel shrink-0 h-full overflow-hidden overflow-y-auto c-scroll bg-bgBase border-l border-strokeWeak max-md:fixed max-md:inset-0 max-md:z-50"
      , id_ "trace_details_container"
      , style_ "width:0"
      , -- Close buttons in the shared detail view send closeDetailPanel to their
        -- .details-panel; in the trace view that's this container.
        [__|on closeDetailPanel call window.closeTraceDetails(me)|]
      ]
      do
        traceDetailsLoading_
        div_ [id_ "trace_details_content", term "hx-on::after-swap" "window.traceDetailsLoaded(this)"] pass

  -- Use skew-adjusted SpanMin from the span tree so flame/timeline matches the waterfall.
  let flattenTrees = concatMap (\t -> t.spanRecord : flattenTrees t.children)
      spanMinToFlame sp =
        AE.object
          [ "spanId" AE..= sp.spanId
          , "name" AE..= sp.spanName
          , "label" AE..= fromMaybe sp.spanName (spanDisplayLabel sp.attributes)
          , "value" AE..= sp.spanDurationNs
          , "start" AE..= sp.startTime
          , "parentId" AE..= sp.parentSpanId
          , "serviceName" AE..= sp.serviceName
          , "hasErrors" AE..= sp.hasErrors
          , "totalSpans" AE..= (1 :: Int)
          ]
      spanJson = decodeUtf8 $ AE.encode $ spanMinToFlame <$> flattenTrees rootSpans
      colorsJson = decodeUtf8 $ AE.encode $ AE.object [AEKey.fromText k AE..= v | (k, v) <- HM.toList serviceColors]
      trId = traceItem.traceId
  script_
    [text|
    function navigateSpans(direction) {
      const container = document.querySelector('#currentSpanIndex')
      const spans = JSON.parse(container.dataset.spanIds)
      const currentSpan = Number(container.dataset.span)
      if (direction == 'next' && currentSpan >= spans.length) return;
      if (direction == 'prev' && currentSpan <= 0) return;
      const spandInd = direction == 'next' ? currentSpan + 1 : currentSpan - 1
      container.dataset.span = spandInd
      htmx.trigger('#trigger-span-' + spans[spandInd], 'click')
    }
    function initTraceCharts() {
      var tabs = document.getElementById('trace-tabs');
      var timeline = document.getElementById('flame_graph');
      if (typeof waterFallGraphChart === 'undefined' || !tabs) return;
      if (!tabs.dataset.waterfallInit) {
        tabs.dataset.waterfallInit = '1';
        waterFallGraphChart("$trId", $colorsJson);
      }
      if (timeline && !timeline.dataset.timelineListener) {
        timeline.dataset.timelineListener = '1';
        timeline.addEventListener('tab-visible', () => {
          if (typeof flameGraphChart === 'undefined' || timeline.dataset.timelineInit) return;
          timeline.dataset.timelineInit = '1';
          flameGraphChart($spanJson, "$trId", $colorsJson);
        }, {once: true});
      }
      // ECharts renders 0x0 into a hidden panel, so the map waits for its tab.
      var mapPanel = document.getElementById('service_map');
      if (mapPanel && !mapPanel.dataset.mapListener) {
        mapPanel.dataset.mapListener = '1';
        mapPanel.addEventListener('tab-visible', () => {
          if (typeof serviceMapChart === 'undefined' || mapPanel.dataset.mapInit) return;
          mapPanel.dataset.mapInit = '1';
          const el = mapPanel.querySelector('[data-service-map]');
          if (!el) return;
          serviceMapChart(el.id, JSON.parse(document.getElementById(el.id + '-data').textContent), {
            colors: JSON.parse(document.getElementById(el.id + '-colors').textContent),
          });
        }, {once: true});
      }
    }
    window.openTraceDetails = function(panel) {
      if (panel.offsetWidth > 5) return;
      const storedWidth = localStorage.getItem('resizer-trace_details_width');
      panel.style.width = window.innerWidth < 768 ? '100%' : (storedWidth || '38%');
      panel.classList.add('open');
      document.getElementById('trace-details-resizer-wrapper')?.classList.remove('hidden');
      window.dispatchEvent(new Event('loglist-resize'));
    };
    window.showTraceDetailsLoading = function() {
      const panel = document.getElementById('trace_details_container');
      if (!panel) return;
      window.openTraceDetails(panel);
      panel.querySelector('.trace-details-loading')?.classList.remove('hidden');
      document.getElementById('trace_details_content')?.classList.add('hidden');
    };
    window.traceDetailsLoaded = function(content) {
      const panel = content.closest('#trace_details_container');
      if (!panel) return;
      window.openTraceDetails(panel);
      panel.querySelector('.trace-details-loading')?.classList.add('hidden');
      content.classList.remove('hidden');
    };
    window.closeTraceDetails = function(trigger) {
      const panel = trigger.closest('#trace_details_container');
      if (!panel) return;
      panel.style.width = '0';
      panel.classList.remove('open');
      panel.querySelector('.trace-details-loading')?.classList.add('hidden');
      const content = document.getElementById('trace_details_content');
      if (content) content.innerHTML = '';
      document.getElementById('trace-details-resizer-wrapper')?.classList.add('hidden');
      document.querySelectorAll('.waterfall-active').forEach((row) => row.classList.remove('bg-fillBrand-weak', 'waterfall-active'));
      window.dispatchEvent(new Event('loglist-resize'));
    };
    var traceDetailsResizer = document.getElementById('trace_details_resizer');
    if (traceDetailsResizer) {
      traceDetailsResizer.onpointerdown = (event) => {
        const panel = document.getElementById('trace_details_container');
        if (!panel) return;
        const startX = event.clientX;
        const startWidth = panel.offsetWidth;
        traceDetailsResizer.setPointerCapture(event.pointerId);
        const move = (moveEvent) => {
          const width = Math.min(window.innerWidth * 0.7, Math.max(260, startWidth - (moveEvent.clientX - startX)));
          panel.style.width = width + 'px';
          window.dispatchEvent(new Event('loglist-resize'));
        };
        const end = () => {
          localStorage.setItem('resizer-trace_details_width', panel.offsetWidth + 'px');
          traceDetailsResizer.removeEventListener('pointermove', move);
          traceDetailsResizer.removeEventListener('pointerup', end);
          traceDetailsResizer.removeEventListener('pointercancel', end);
        };
        traceDetailsResizer.addEventListener('pointermove', move);
        traceDetailsResizer.addEventListener('pointerup', end);
        traceDetailsResizer.addEventListener('pointercancel', end);
      };
    }
    function scheduleTraceCharts() {
      requestAnimationFrame(() => {
        const run = () => initTraceCharts();
        if (window.requestIdleCallback) window.requestIdleCallback(run, {timeout: 200});
        else setTimeout(run, 0);
      });
    }
    document.addEventListener("DOMContentLoaded", scheduleTraceCharts);
    scheduleTraceCharts();
  |]


renderSpanRecordRow :: Integer -> V.Vector Telemetry.SpanRecord -> HashMap Text Text -> Text -> Html ()
renderSpanRecordRow totalDuration spanRecords colors service = do
  let filterRecords = V.filter (\x -> getServiceName x.resource == service) spanRecords
      listLen = V.length filterRecords
      duration = sum $ (.spanDurationNs) <$> filterRecords
      errCount = V.length $ V.filter spanHasErrors filterRecords
      hasErr = errCount > 0
      -- Zero guards: a service name may match no span, and all-instant traces have totalDuration=0.
      avgDuration = if listLen == 0 then 0 else duration `div` toInteger listLen
      pctOfTotal = if totalDuration == 0 then 0 else duration * 100 `div` totalDuration
  tr_
    [ class_ $ "w-full overflow-x-hidden p-2 cursor-pointer font-medium hover:bg-fillWeaker border-b-2 last:border-b-0" <> bool "" " bg-fillError-weak/30" hasErr
    , [__|on click toggle .hidden on next <tr/> then toggle .rotate-90 on the first <svg/> in the first <td/> in me|]
    ]
    do
      td_ [class_ "ml-1 px-2 py-1 w-[600px] text-textStrong truncate flex items-center gap-1 font-medium"] do
        div_ [class_ $ "w-1 h-4 " <> if hasErr then "bg-fillError-strong" else "bg-fillBrand-weak"] pass
        faSprite_ "chevron-right" "regular" "h-3 w-3 mr-2 text-iconNeutral"
        div_ [class_ $ "w-3 h-3 rounded-sm " <> getServiceColor service colors] pass
        span_ [] $ toHtml service
      td_ [class_ "px-2 py-1 max-w-48 text-textWeak truncate pl-4 tabular-nums"] $ toHtml $ show listLen
      td_ [class_ "px-2 py-1 max-w-48 truncate pl-4 tabular-nums"]
        $ if hasErr
          then span_ [class_ "inline-flex items-center gap-1 text-textError font-semibold", title_ "Spans with exception events"] do
            span_ [class_ "w-1.5 h-1.5 rounded-full bg-fillError-strong"] pass
            toHtml $ show errCount
          else span_ [class_ "text-textWeak/60"] "—"
      td_ [class_ "px-2 py-1 max-w-48 text-textWeak truncate pl-4 tabular-nums"] $ toHtml $ getDurationNSMS avgDuration
      td_ [class_ "px-2 py-1 max-w-48 text-textWeak truncate pl-4 tabular-nums"] $ toHtml $ getDurationNSMS duration
      td_ [class_ "px-2 py-1 max-w-48 text-textWeak truncate pl-4 tabular-nums"] $ toHtml $ show pctOfTotal <> "%"
  tr_ [class_ "hidden p-0 m-0", [__|on click halt|]] do
    td_ [colspan_ "6", class_ "pl-[13px] overflow-x-hidden"] do
      spanTable filterRecords


renderSpanListTable :: V.Vector Text -> HashMap Text Text -> V.Vector Telemetry.SpanRecord -> Html ()
renderSpanListTable services colors records =
  table_ [class_ "w-full table table-sm overflow-x-hidden"] $ do
    thead_ [class_ "border-b"] $ do
      tr_ [class_ "p-2 border-b font-normal"] $ do
        th_ "Resource"
        th_ "Spans"
        th_ "Errors"
        th_ "Avg. Duration"
        th_ "Exec. Time"
        th_ "%Exec. Time"
    tbody_ [class_ "space-y-0"]
      $ traverse_ (renderSpanRecordRow (sum $ (.spanDurationNs) <$> records) records colors) services


spanTable :: V.Vector Telemetry.SpanRecord -> Html ()
spanTable records =
  div_ [class_ "rounded-xl my-2 mx-3 border border-strokeWeak"] do
    table_ [class_ "table w-full"] do
      thead_ [class_ "border-b border-strokeWeak"]
        $ tr_ [class_ "p-2 border-b font-medium"]
        $ do
          td_ "Time"
          td_ "Span name"
          td_ "Event type"
          td_ "Span kind"
          td_ "Exec. time"
      tbody_ do
        forM_ records $ \spanRecord -> do
          let reqType = maybe "" (\(t, _, _, _) -> t) $ getRequestDetails spanRecord.attributes
          tr_
            ( [ id_ $ "sp-list-" <> spanRecord.spanId
              , class_ "span-filterble font-medium"
              ]
                <> spanDetailAttrs_ (UUID.toText spanRecord.projectId) spanRecord.uSpanId spanRecord.timestamp
            )
            $ do
              td_ $ Components.localTimeFmt_ "MMM dd yyyy HH:mm:ss.SSS" spanRecord.timestamp
              td_ $ toHtml spanRecord.spanName
              td_ do
                span_ [class_ "cbadge-sm badge-neutral"] $ toHtml reqType
              td_ do
                span_ [class_ "cbadge-sm badge-neutral"] $ toHtml $ T.drop 2 $ maybe "" show spanRecord.kind
              td_ do
                span_ [class_ "cbadge-sm badge-neutral"] $ toHtml $ getDurationNSMS spanRecord.spanDurationNs


stBox :: Text -> Text -> Maybe (Html ()) -> Html ()
stBox label value iconM =
  div_ [class_ "flex items-center px-2 gap-1.5 border-r last:border-r-0 whitespace-nowrap shrink-0", title_ label] do
    fold iconM
    span_ [class_ "text-textStrong text-sm tabular-nums"] $ toHtml value
    span_ [class_ "font-medium text-textWeak text-xs"] $ toHtml label


syntheticMissingParentKey :: Text
syntheticMissingParentKey = "monoscope.synthetic.missing_parent"


-- | The SDK payload span ("monoscope.http") is a synthetic carrier for request/
-- response bodies, not a real operation — its bodies surface on the parent
-- request's detail panel. When that parent is present in the trace, drop the
-- SDK row and graft its children onto the parent so the waterfall shows only
-- real work.
collapseSdkSpans :: V.Vector Telemetry.SpanRecord -> V.Vector Telemetry.SpanRecord
collapseSdkSpans spans =
  let ids = S.fromList $ V.toList $ (.spanId) <$> spans
      remap = Map.fromList [(sp.spanId, p) | sp <- V.toList spans, sp.spanName == Telemetry.sdkSpanStoredName, Just p <- [sp.parentSpanId], not (T.null p), S.member p ids]
      -- Nearest surviving ancestor: chained SDK spans remap through each other.
      resolve p = maybe p resolve (Map.lookup p remap)
   in V.map (\sp -> maybe sp (\p -> sp{Telemetry.parentSpanId = Just (resolve p)}) (sp.parentSpanId >>= (`Map.lookup` remap)))
        $ V.filter (\sp -> not (Map.member sp.spanId remap)) spans


-- | Build span tree with clock skew adjustment: if a child starts before its
-- parent, shift it forward to the parent's start time. Orphans cluster under
-- one synthetic placeholder per missing parent_id, instead of flattening as
-- sibling roots.
buildSpanTree :: V.Vector Telemetry.SpanRecord -> [SpanTree]
buildSpanTree spans =
  -- Keyed by literal parent_id so missing parents (orphans) stay detectable.
  let spanMap = V.foldr (\sp m -> Map.insertWith (++) sp.parentSpanId [sp] m) Map.empty spans
      ids :: Set Text
      ids = V.foldr (S.insert . (.spanId)) S.empty spans
      missingParents :: [Text]
      missingParents =
        ordNub $ V.toList $ V.mapMaybe (mfilter (\p -> not (T.null p) && not (S.member p ids)) . (.parentSpanId)) spans
      realRoots = buildTree spanMap Nothing (0, toInteger (maxBound :: Int64))
      orphanRoots = mapMaybe (syntheticRoot spanMap) missingParents
   in realRoots <> orphanRoots
  where
    buildTree :: Map.Map (Maybe Text) [Telemetry.SpanRecord] -> Maybe Text -> (Integer, Integer) -> [SpanTree]
    buildTree spanMap parentId (pStart, pEnd) =
      [ let cStart = utcTimeToNanoseconds sp.startTime
            delta = max 0 (pStart - cStart)
            adjStart = cStart + delta
            adjEnd = fmap ((+ delta) . utcTimeToNanoseconds) sp.endTime
            adjDur = if delta > 0 then min sp.spanDurationNs (pEnd - adjStart) else sp.spanDurationNs
            rec = (spanMinFromRecord sp){spanDurationNs = adjDur, startTime = adjStart, endTime = adjEnd}
         in SpanTree rec (buildTree spanMap (Just sp.spanId) (adjStart, adjStart + adjDur))
      | sp <- Map.findWithDefault [] parentId spanMap
      ]

    syntheticRoot :: Map.Map (Maybe Text) [Telemetry.SpanRecord] -> Text -> Maybe SpanTree
    syntheticRoot spanMap missingPid = case Map.lookup (Just missingPid) spanMap of
      Just kids@(sp : rest) ->
        let startNs = foldr (min . (utcTimeToNanoseconds . (.startTime))) (utcTimeToNanoseconds sp.startTime) rest
            endNs = foldr (max . utcTimeToNanoseconds) startNs (mapMaybe (.endTime) kids)
            rec =
              (spanMinFromRecord sp)
                { parentSpanId = Nothing
                , spanId = missingPid
                , uSpanId = UUID.nil
                , spanName = "Upstream span missing \x2014 " <> T.take 8 missingPid
                , spanDurationNs = max 0 (endNs - startNs)
                , startTime = startNs
                , endTime = Just endNs
                , hasErrors = False
                , attributes = Just (one (syntheticMissingParentKey, AE.String missingPid))
                }
         in Just (SpanTree rec (buildTree spanMap (Just missingPid) (startNs, endNs)))
      _ -> Nothing


-- | Lift a 'Telemetry.SpanRecord' into the lighter 'SpanMin' the waterfall
-- renderer consumes. Time fields are normalised to nanos and end-time is
-- mapped through Maybe; callers override durations / bounds when applying
-- clock-skew adjustment or building synthetic placeholders.
spanMinFromRecord :: Telemetry.SpanRecord -> SpanMin
spanMinFromRecord sp =
  SpanMin
    { parentSpanId = sp.parentSpanId
    , spanId = sp.spanId
    , uSpanId = fromMaybe UUID.nil (UUID.fromText sp.uSpanId)
    , spanName = sp.spanName
    , spanDurationNs = sp.spanDurationNs
    , serviceName = getServiceName sp.resource
    , startTime = utcTimeToNanoseconds sp.startTime
    , endTime = utcTimeToNanoseconds <$> sp.endTime
    , hasErrors = spanHasErrors sp
    , timestamp = sp.timestamp
    , attributes = sp.attributes
    }


buildSpanTree_ :: Projects.ProjectId -> SpanTree -> Int -> HashMap Text Text -> Html ()
buildSpanTree_ pid sp level scol = do
  let hasChildren = not $ null sp.children
      serviceCol = getServiceColor sp.spanRecord.serviceName scol
      indent = show (level * 12) <> "px"
      isSynthetic = maybe False (Map.member syntheticMissingParentKey) sp.spanRecord.attributes
  div_ [class_ "span-filterble"] do
    div_
      ( [ class_ $ "flex items-center w-full h-7 hover:bg-fillWeaker waterfall-row" <> bool "" " bg-fillError-weak/40 hover:bg-fillError-weak" sp.spanRecord.hasErrors <> bool " cursor-pointer" " italic text-textWeak bg-fillWeaker/50" isSynthetic
        , id_ $ "trigger-span-" <> sp.spanRecord.spanId
        ]
          <> ( if isSynthetic
                 then [title_ ("Upstream parent span " <> sp.spanRecord.spanId <> " was never reported by the service. Showing an inferred placeholder.")]
                 else
                   [__|on click remove .bg-fillBrand-weak from .waterfall-active then add .bg-fillBrand-weak .waterfall-active to me|]
                     : spanDetailAttrs_ pid.toText (UUID.toText sp.spanRecord.uSpanId) sp.spanRecord.timestamp
             )
      )
      do
        div_ [class_ "shrink-0 flex items-center gap-1 px-2 overflow-hidden", style_ $ "width:var(--wf-left);padding-left:" <> indent] do
          if hasChildren
            then
              button_
                [ class_ "waterfall-toggle w-4 h-4 flex items-center justify-center shrink-0"
                , [__|on click halt the event's bubbling then toggle .hidden on the next .waterfall-children from the closest .span-filterble then toggle .rotate-90 on the first <svg/> in me|]
                ]
                $ faSprite_ "chevron-right" "regular" "h-3 w-3 text-textWeak rotate-90"
            else div_ [class_ "w-4 shrink-0"] pass
          let label = fromMaybe sp.spanRecord.spanName (spanDisplayLabel sp.spanRecord.attributes)
              tip = sp.spanRecord.serviceName <> " — " <> label
          if isSynthetic
            then faSprite_ "circle-question" "regular" "h-3 w-3 text-textWeak shrink-0"
            else div_ [class_ $ "w-2.5 h-2.5 rounded-full shrink-0 " <> serviceCol, title_ sp.spanRecord.serviceName] pass
          when sp.spanRecord.hasErrors
            $ span_
              [ class_ "shrink-0 w-3.5 h-3.5 rounded-sm bg-fillError-strong text-textInverse-strong text-2xs font-bold flex items-center justify-center leading-none"
              , title_ "This span errored"
              , Aria.label_ "Error span"
              ]
            $ toHtml @Text "!"
          span_ [class_ "text-xs font-medium text-textStrong truncate max-w-20", title_ tip] $ toHtml sp.spanRecord.serviceName
          span_ [class_ "text-xs text-textWeak truncate", title_ tip] $ toHtml label
        div_
          [ class_ "h-full relative grow min-w-0"
          , id_ $ "waterfall-bar-" <> sp.spanRecord.spanId
          , term "data-start" $ show sp.spanRecord.startTime
          , term "data-duration" $ show sp.spanRecord.spanDurationNs
          , term "data-service" sp.spanRecord.serviceName
          , term "data-span-name" sp.spanRecord.spanName
          , term "data-has-errors" $ bool "false" "true" sp.spanRecord.hasErrors
          ]
          pass
    when hasChildren
      $ div_ [class_ "waterfall-children"] do
        forM_ sp.children \c ->
          buildSpanTree_ pid c (level + 1) scol
