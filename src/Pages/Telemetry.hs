module Pages.Telemetry (
  -- Metrics
  metricsOverViewGetH,
  metricDetailsGetH,
  MetricsOverViewGet (..),
  metricBreakdownGetH,
  -- Trace
  traceH,
  TraceDetailsGet (..),
  tracePage,
) where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Default
import Data.HashMap.Internal.Strict qualified as HM
import Data.Map qualified as Map
import Data.Map.Strict qualified as MapS
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (formatShow, iso8601Format)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (SpanStatus (SSError))
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components qualified as Components
import Pages.LogExplorer.LogItem (getRequestDetails, getServiceColor, getServiceName, spanHasErrors)
import Pages.LogExplorer.LogItem qualified as LogItem
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Relude hiding (ask)
import System.Config (AuthContext (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (faSprite_, getDurationNSMS, getServiceColors, onpointerdown_, parseTime, prettyPrintCount, utcTimeToNanoseconds)


-- Metrics types
data MetricNode = MetricNode
  { parent :: Text
  , current :: Text
  }
  deriving (Eq, Show)


data MetricTree = MetricTree
  { spanRecord :: MetricNode
  , children :: [MetricTree]
  }
  deriving (Generic, Show)


data MetricsOverViewGet
  = MetricsOVDataPointMain (PageCtx (Projects.ProjectId, V.Vector Telemetry.MetricDataPoint))
  | MetricsOVChartsMain (PageCtx (Projects.ProjectId, V.Vector Telemetry.MetricChartListData, V.Vector Text, Text, Text, Maybe Text))
  | MetricsOVChartsPaginated (Projects.ProjectId, V.Vector Telemetry.MetricChartListData, Text, Maybe Text)


instance ToHtml MetricsOverViewGet where
  toHtml (MetricsOVDataPointMain (PageCtx bwconf (pid, datapoints))) = toHtml $ PageCtx bwconf $ dataPointsPage pid datapoints
  toHtml (MetricsOVChartsMain (PageCtx bwconf (pid, mList, serviceNames, source, prefix, nextUrl))) = toHtml $ PageCtx bwconf $ chartsPage pid mList serviceNames source prefix nextUrl
  toHtml (MetricsOVChartsPaginated (pid, mList, source, nextUrl)) = toHtml $ chartList pid source mList nextUrl
  toHtmlRaw = toHtml


data TraceDetailsGet
  = TraceDetails Projects.ProjectId Telemetry.Trace (V.Vector Telemetry.SpanRecord)
  | SpanDetails Projects.ProjectId Telemetry.OtelLogsAndSpans (Maybe Telemetry.OtelLogsAndSpans) (Maybe Text) (Maybe Text)
  | TraceDetailsNotFound Text


data ServiceData = ServiceData {name :: Text, duration :: Integer}


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
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data SpanTree = SpanTree
  { spanRecord :: SpanMin
  , children :: [SpanTree]
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


instance ToHtml TraceDetailsGet where
  toHtml (TraceDetails pid tr spanRecs) = toHtml $ tracePage pid tr spanRecs
  toHtml (SpanDetails pid s aptSpn left right) = toHtml $ LogItem.expandedItemView pid s aptSpn left right
  toHtml (TraceDetailsNotFound msg) = toHtml msg
  toHtmlRaw = toHtml


-- Metrics handlers
metricsOverViewGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> ATAuthCtx (RespHeaders MetricsOverViewGet)
metricsOverViewGetH pid tabM fromM toM sinceM sourceM prefixM cursorM = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  now <- Time.currentTime
  let tab = maybe "charts" (\t -> if t == "charts" then t else "datapoints") tabM
  let (from, to, currentRange) = parseTime fromM toM sinceM now
      bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , prePageTitle = Just "Explorer"
          , pageTitle = "Metrics"
          , config = appCtx.env
          , navTabs = Just $ div_ [class_ "tabs tabs-box tabs-outline items-center"] do
              a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer", role_ "tab", class_ "tab h-auto! "] "Events"
              a_ [href_ $ "/p/" <> pid.toText <> "/metrics", role_ "tab", class_ "tab h-auto! tab-active text-textStrong"] "Metrics"
          , docsLink = Just "https://apitoolkit.io/docs/dashboard/dashboard-pages/metrics/"
          , pageActions = Just $ div_ [class_ "inline-flex gap-2"] do
              TimePicker.timepicker_ Nothing currentRange Nothing
              TimePicker.refreshButton_
          }
  if tab == "datapoints"
    then do
      dataPoints <- Telemetry.getDataPointsData pid (from, to)
      addRespHeaders $ MetricsOVDataPointMain $ PageCtx bwconf (pid, V.fromList dataPoints)
    else do
      let cursor = fromMaybe 0 cursorM
      metricList <- V.fromList <$> Telemetry.getMetricChartListData pid sourceM prefixM (from, to) cursor
      let sourceQ = maybe "" ("&source=" <>) sourceM
          fromQ = maybe "" ("&from=" <>) fromM
          toQ = maybe "" ("&from=" <>) toM
          sinceQ = maybe "" ("&since=" <>) sinceM
          prfixQ = maybe "" ("&prefix=" <>) prefixM
          cursorQ = "&cursor=" <> show (cursor + 20)
          nextFetchUrl =
            if V.length metricList < 20
              then Nothing
              else Just $ "/p/" <> pid.toText <> "/metrics?tab=charts" <> sourceQ <> fromQ <> toQ <> sinceQ <> prfixQ <> cursorQ
      serviceNames <- V.fromList <$> Telemetry.getMetricServiceNames pid
      if cursor == 0
        then do
          addRespHeaders $ MetricsOVChartsMain $ PageCtx bwconf (pid, metricList, serviceNames, fromMaybe "all" sourceM, fromMaybe "all" prefixM, nextFetchUrl)
        else do
          addRespHeaders $ MetricsOVChartsPaginated (pid, metricList, fromMaybe "all" sourceM, nextFetchUrl)


metricDetailsGetH :: Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
metricDetailsGetH pid metricName source fromM toM sinceM = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  let (_, _, currentRange) = parseTime fromM toM sinceM now
  metricM <- Telemetry.getMetricData pid metricName
  case metricM of
    Just metric -> do
      addRespHeaders $ metricsDetailsPage pid metric.serviceNames metric (fromMaybe "all" source) currentRange
    Nothing -> do
      addRespHeaders $ div_ [class_ "flex flex-col gap-2 -10 text-2xl"] "Metric not found"


metricBreakdownGetH :: Projects.ProjectId -> Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
metricBreakdownGetH pid metricName labelM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let label = fromMaybe "all" labelM
  if label == "all"
    then do
      metricM <- Telemetry.getMetricData pid metricName
      case metricM of
        Just metric -> do
          addRespHeaders $ metricBreakdown pid Nothing metric.metricLabels
        Nothing -> addRespHeaders $ metricBreakdown pid Nothing []
    else do
      lableValues <- V.fromList <$> Telemetry.getMetricLabelValues pid metricName label
      addRespHeaders $ metricBreakdown pid labelM lableValues


-- Trace handler
traceH :: Projects.ProjectId -> Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders TraceDetailsGet)
traceH pid trId timestamp spanIdM nav = do
  now <- Time.currentTime
  if isJust nav
    then do
      spanRecords' <- Telemetry.getSpanRecordsByTraceId pid trId timestamp now
      let spanRecords = V.catMaybes $ Telemetry.convertOtelLogsAndSpansToSpanRecord <$> spanRecords'
      let sid = fromMaybe "" spanIdM
          targetSpan = fromMaybe (V.head spanRecords') (V.find (\x -> maybe False (\s -> s.span_id == Just sid) x.context) spanRecords')
          targetIndex = fromMaybe 0 (V.findIndex (\x -> maybe False (\s -> s.span_id == Just sid) x.context) spanRecords')
          prevSpan =
            if targetIndex > 0
              then Just (spanRecords V.! (targetIndex - 1))
              else Nothing
          nextSpan =
            if targetIndex < V.length spanRecords - 1
              then Just (spanRecords V.! (targetIndex + 1))
              else Nothing
      let atpSpan = V.find (\x -> x.name == Just "monoscope.http") spanRecords'
      addRespHeaders $ SpanDetails pid targetSpan atpSpan (prevSpan >>= \s -> Just s.spanId) (nextSpan >>= \s -> Just s.spanId)
    else do
      traceItemM <- Telemetry.getTraceDetails pid trId timestamp now
      case traceItemM of
        Just traceItem -> do
          spanRecords' <- Telemetry.getSpanRecordsByTraceId pid trId timestamp now
          let spanRecords = V.catMaybes $ Telemetry.convertOtelLogsAndSpansToSpanRecord <$> spanRecords'
          addRespHeaders $ TraceDetails pid traceItem spanRecords
        Nothing -> addRespHeaders $ TraceDetailsNotFound "Trace not found"


-- Metrics UI components
overViewTabs :: Projects.ProjectId -> Text -> Html ()
overViewTabs pid tab = do
  div_ [class_ "w-max mt-5"] do
    div_ [class_ "tabs tabs-box tabs-md tabs-outline items-center border"] do
      a_ [onclick_ "window.setQueryParamAndReload('tab', 'charts')", role_ "tab", class_ $ "tab py-1.5 h-auto! " <> if tab == "charts" then "tab-active" else ""] "Overview"
      a_ [onclick_ "window.setQueryParamAndReload('tab', 'datapoints')", role_ "tab", class_ $ "tab py-1.5 h-auto!  " <> if tab == "datapoints" then "tab-active" else ""] "Datapoints"


chartsPage :: Projects.ProjectId -> V.Vector Telemetry.MetricChartListData -> V.Vector Text -> Text -> Text -> Maybe Text -> Html ()
chartsPage pid metricList sources source mFilter nextUrl = do
  div_ [class_ "flex flex-col gap-6 px-6 h-[calc(100%-60px)] overflow-y-scroll"] $ do
    overViewTabs pid "charts"
    div_ [class_ "w-full"] do
      Components.drawer_ "global-data-drawer" Nothing Nothing ""
      template_ [id_ "loader-tmp"] $ span_ [class_ "loading loading-dots loading-md"] ""
      div_ [class_ "w-full flex gap-1 items-start"] do
        select_
          [ class_ "select bg-fillWeaker  border !border-strokeWeak h-12 rounded-xl w-36 focus:outline-hidden"
          , onchange_ "(() => {window.setQueryParamAndReload('metric_source', this.value)})()"
          ]
          do
            option_ ([selected_ "all" | "all" == source] ++ [value_ "all"]) "Data Source"
            forM_ sources $ \s -> option_ ([selected_ s | s == source] ++ [value_ s]) $ toHtml s
        div_ [class_ "flex items-center gap-2 w-full rounded-xl px-3 h-12 border border-strokeWeak bg-fillWeaker"] do
          faSprite_ "magnifying-glass" "regular" "w-4 h-4 text-iconNeutral"
          input_
            [ class_ "w-full text-textStrong bg-transparent hover:outline-hidden focus:outline-hidden"
            , type_ "text"
            , placeholder_ "Search"
            , id_ "search-input"
            , [__| on input show .metric_filterble in #metric_list_container when its textContent.toLowerCase() contains my value.toLowerCase() |]
            ]
        select_
          [ class_ "select bg-fillWeaker h-12 border !border-strokeWeak rounded-xl w-42 focus:outline-hidden"
          , onchange_ "(() => {window.setQueryParamAndReload('metric_prefix', this.value)})()"
          ]
          do
            let metricNames =
                  ( \x ->
                      let (n, pr) = if length (T.splitOn "." x.metricName) == 1 then (T.splitOn "_" x.metricName, "_") else (T.splitOn "." x.metricName, ".")
                       in fromMaybe "" (viaNonEmpty head n) <> pr
                  )
                    <$> V.toList metricList
            option_ ([selected_ "all" | "all" == mFilter] ++ [value_ "all"]) "View By"
            forM_ (ordNub metricNames) $ \m -> option_ ([selected_ m | m == mFilter] ++ [value_ m]) $ toHtml m
    if V.null metricList
      then
        div_ [class_ "w-full flex items-center justify-center h-96"]
          $ Components.emptyState_ "No metrics found" "There are no metrics to display at the moment. Metrics will appear here once your application starts sending telemetry data." Nothing ""
      else
        div_ [class_ "w-full grid grid-cols-3 gap-4", id_ "metric_list_container"]
          $ chartList pid source metricList nextUrl


chartList :: Projects.ProjectId -> Text -> V.Vector Telemetry.MetricChartListData -> Maybe Text -> Html ()
chartList pid source metricList nextUrl = do
  forM_ metricList $ \metric -> do
    div_ [class_ "w-full flex flex-col gap-2 metric_filterble"] do
      let detailUrl = "/p/" <> pid.toText <> "/metrics/details/" <> metric.metricName <> "/?source=" <> source
      let expandBtn =
            [text|on pointerdown or click set #global-data-drawer.checked to true
                  then set #global-data-drawer-content.innerHTML to #loader-tmp.innerHTML
                  then fetch $detailUrl
                  then set #global-data-drawer-content.innerHTML to it
                  then htmx.process(#global-data-drawer-content)
                  then _hyperscript.processNode(#global-data-drawer-content)
                  then window.evalScriptsFromContent(#global-data-drawer-content)|]
      div_ [class_ "h-52"]
        $ toHtml
        $ def
          { Widget.wType = Widget.WTTimeseriesLine
          , Widget.title = Just metric.metricName
          , Widget.query = Just $ "metrics | where metric_name == \"" <> metric.metricName <> "\" | summarize avg(metric_value.contents.value) by bin_auto(timestamp),attributes"
          , Widget.layout = Just $ Widget.Layout{x = Just 0, y = Just 0, w = Just 2, h = Just 1}
          , Widget.unit = Just metric.metricUnit
          , Widget.hideLegend = Nothing
          , Widget.eager = Just True
          , Widget._projectId = Just pid
          , Widget.expandBtnFn = Just expandBtn
          }
  whenJust nextUrl \url ->
    a_ [hxTrigger_ "intersect once", hxSwap_ "outerHTML", hxGet_ url] pass


dataPointsPage :: Projects.ProjectId -> V.Vector Telemetry.MetricDataPoint -> Html ()
dataPointsPage pid metrics = do
  div_ [class_ "flex flex-col gap-2 px-6 h-[calc(100%-60px)] overflow-y-scroll"] $ do
    overViewTabs pid "datapoints"
    div_
      [ class_ "w-full rounded-2xl mt-4 border flex flex-col"
      ]
      do
        div_ [class_ "flex px-4 justify-between py-3 text-sm font-medium border-b text-textStrong"] $ do
          div_ [class_ " w-[calc(40vw-46px)]"] "Metric"
          div_ [class_ "w-[10vw] "] "Sources"
          div_ [class_ "w-[8vw] ml-2"] "Datapoint"
          div_ [class_ "w-[10vw] ml-2"] "Referenced in"
        if V.null metrics
          then
            div_ [class_ "w-full flex items-center justify-center h-96"]
              $ Components.emptyState_ "No metrics found" "There are no metrics to display at the moment. Metrics will appear here once your application starts sending telemetry data." Nothing ""
          else div_ [class_ "w-full"] $ do
            let metrMap = Map.fromList $ V.toList $ V.map (\mdp -> (mdp.metricName, mdp)) metrics
            metricsTree pid metrics metrMap


metricsDetailsPage :: Projects.ProjectId -> V.Vector Text -> Telemetry.MetricDataPoint -> Text -> Maybe (Text, Text) -> Html ()
metricsDetailsPage pid sources metric source currentRange = do
  div_ [class_ "flex flex-col gap-8 h-full"] do
    div_ [class_ "flex items-center w-full"] do
      div_ [class_ "flex flex-col gap-1"] do
        span_ [class_ "text-textStrong text-sm font-medium"] "Data source"
        select_
          [ class_ "select select-sm bg-fillWeaker border border-strokeWeak rounded-xl w-36 focus:outline-hidden"
          , hxGet_ $ "/p/" <> pid.toText <> "/metrics/details/" <> metric.metricName <> "/"
          , name_ "metric_source"
          , hxTarget_ "#global-data-drawer-content"
          , hxSwap_ "innerHTML"
          ]
          do
            option_ ([selected_ "all" | "all" == source] ++ [value_ "all"]) "All"
            forM_ sources $ \s -> option_ ([selected_ s | s == source] ++ [value_ s]) $ toHtml s
    div_ [class_ "w-full border border-strokeWeak rounded-2xl p-2 sticky z-50 bg-bgBase top-4"] do
      div_ [class_ "flex items-center text-sm"] $ span_ [] $ toHtml metric.metricName
      div_ [class_ "h-64 w-full"]
        $ toHtml
        $ def
          { Widget.wType = Widget.WTTimeseriesLine
          , Widget.title = Nothing
          , Widget.query = Just $ "metrics | where metric_name == \"" <> metric.metricName <> "\" | summarize avg(metric_value.contents.value) by bin_auto(timestamp),attributes"
          , Widget.layout = Just $ Widget.Layout{x = Just 0, y = Just 0, w = Just 2, h = Just 1}
          , Widget.unit = Just metric.metricUnit
          , Widget.hideLegend = Nothing
          , Widget.eager = Just True
          , Widget._projectId = Just pid
          , Widget.id = Just $ "details_" <> T.replace "." "_" metric.metricName
          , Widget.expandBtnFn = Nothing
          }

    div_ [class_ "flex flex-col gap-2 rounded-2xl border border-strokeWeak", id_ "metric-tabs-container"] $ do
      div_ [class_ "flex", [__|on click halt|]] $ do
        button_ [class_ "a-tab border-b border-b-strokeWeak px-4 py-1.5 t-tab-active", onclick_ "navigatable(this, '#ov-content', '#metric-tabs-container', 't-tab-active')"] "Overview"
        button_ [class_ "a-tab border-b border-b-strokeWeak px-4 py-1.5 ", onclick_ "navigatable(this, '#br-content', '#metric-tabs-container', 't-tab-active')"] "Breakdown"
        button_ [class_ "a-tab border-b w-max whitespace-nowrap border-b-strokeWeak px-4 py-1.5 ", onclick_ "navigatable(this, '#rl-content', '#metric-tabs-container', 't-tab-active')"] "Related metrics"
        div_ [class_ "w-full border-b border-b-strokeWeak"] pass

      div_ [class_ "grid px-4 pb-4 mt-2 text-textWeak font-normal"] $ do
        div_ [class_ "a-tab-content", id_ "ov-content"] $ do
          div_ [class_ "flex flex-col gap-4"] do
            div_ [class_ "flex flex-col gap-1"] $ do
              span_ [class_ "text-textStrong font-medium"] "Description"
              span_ [] $ toHtml $ if metric.metricDescription == "" then "No description" else metric.metricDescription
            div_ [class_ "flex flex-col gap-1"] $ do
              span_ [class_ "text-textStrong font-medium"] "Type"
              span_ [] $ toHtml metric.metricType
            div_ [class_ "flex flex-col gap-1"] $ do
              span_ [class_ "text-textStrong font-medium"] "Unit"
              span_ [] $ toHtml if metric.metricUnit == "" then "No unit" else metric.metricUnit
            div_ [class_ "flex flex-col gap-1"] $ do
              span_ [class_ "text-textStrong font-medium"] "Labels"
              div_ [class_ "flex items-center"] do
                forM_ metric.metricLabels $ \label -> span_ [class_ "badge badge-ghost text-textWeak"] $ toHtml label
        div_ [class_ "hidden a-tab-content", id_ "br-content"] do
          div_ [class_ "flex flex-col gap-4"] $ do
            div_ [class_ "flex flex-col gap-1"] do
              span_ [class_ "text-textStrong text-sm font-medium"] "By label"
              select_
                [ class_ "select select-sm bg-fillWeaker border border-strokeWeak rounded-xl w-36 focus:outline-hidden"
                , hxGet_ $ "/p/" <> pid.toText <> "/metrics/details/" <> metric.metricName <> "/breakdown"
                , name_ "label"
                , hxTarget_ "#breakdown-container"
                , hxSwap_ "innerHTML"
                ]
                do
                  option_ ([selected_ "all" | "all" == source] ++ [value_ "all"]) "All"
                  forM_ metric.metricLabels $ \s -> option_ ([selected_ s | s == source] ++ [value_ s]) $ toHtml s
            div_ [class_ "flex flex-col gap-2", id_ "breakdown-container"] do
              metricBreakdown pid Nothing metric.metricLabels

        div_ [class_ "hidden a-tab-content", id_ "rl-content"] pass


metricBreakdown :: Projects.ProjectId -> Maybe Text -> V.Vector Text -> Html ()
metricBreakdown pid label values = do
  div_ [class_ "grid grid-cols-2 gap-2"] do
    forM_ values $ \v -> do
      div_ [class_ "w-full flex flex-col gap-2 metric_filterble rounded-lg p-2 border border-strokeWeak"] do
        div_ [class_ "w-full justify-between flex gap-2 items-center"] do
          div_ [class_ "flex gap-1 items-center"] do
            span_ [class_ "text-sm"] $ toHtml v
          button_
            [ class_ "btn border border-strokeWeak btn-xs btn-circle"
            ]
            do
              faSprite_ "up-right-and-down-left-from-center" "regular" "w-3 h-3 text-iconNeutral"
        div_ [class_ "h-48"] pass


pathToNodes :: Text -> [MetricNode]
pathToNodes path =
  let segments = T.splitOn "." path
   in zipWith toNode ("___root___" : scanl1 combine segments) segments
  where
    combine acc segment = acc <> "." <> segment
    toNode = MetricNode


buildMetricNodes :: [Text] -> [MetricNode]
buildMetricNodes = concatMap pathToNodes


buildMetricMap :: [MetricNode] -> Map (Maybe Text) [MetricNode]
buildMetricMap = Relude.foldr insertNode Map.empty
  where
    insertNode :: MetricNode -> Map (Maybe Text) [MetricNode] -> Map (Maybe Text) [MetricNode]
    insertNode sp m =
      let key = if parent sp == "___root___" then Nothing else Just (parent sp)
          newEntry = [sp]
       in Map.insertWith
            (\new old -> if sp `elem` old then old else new ++ old)
            key
            newEntry
            m


buildMetricTree :: [Text] -> [MetricTree]
buildMetricTree metrics =
  let metricsNodes = buildMetricNodes metrics
      metricMap = buildMetricMap metricsNodes
   in buildTree_ metricMap Nothing
  where
    buildTree_ metricMap parentId =
      case Map.lookup parentId metricMap of
        Nothing -> []
        Just metrics' ->
          [ MetricTree
              MetricNode
                { parent = mt.parent
                , current = mt.current
                }
              (buildTree_ metricMap (if mt.parent == "___root___" then Just mt.current else Just $ mt.parent <> "." <> mt.current))
          | mt <- metrics'
          ]


metricsTree :: Projects.ProjectId -> V.Vector Telemetry.MetricDataPoint -> Map Text Telemetry.MetricDataPoint -> Html ()
metricsTree pid metrics dp = do
  let tr = buildMetricTree $ V.toList $ (.metricName) <$> metrics
  div_ [class_ "px-4 py-2 flex flex-col gap-2"] do
    forM_ (Relude.zip [0 ..] tr) \(i, c) -> do
      buildMetricTree_ pid c 0 True dp


buildMetricTree_ :: Projects.ProjectId -> MetricTree -> Int -> Bool -> Map Text Telemetry.MetricDataPoint -> Html ()
buildMetricTree_ pid sp level isLasChild dp = do
  let hasChildren = not $ null sp.children
  let paddingLeft = show (35 * level + 46) <> "px)"
  div_ [class_ "flex items-start w-full relative span-filterble"] do
    when (level /= 0) $ div_ [class_ "w-8 shrink-0 ml-2 h-[1px] mt-2 bg-strokeWeak"] pass
    unless (level == 0) $ div_ [class_ "absolute -top-3 left-2 border-l h-5 border-l-strokeWeak"] pass
    unless isLasChild $ div_ [class_ "absolute top-1 left-2 border-l h-full border-l-strokeWeak"] pass
    div_ [class_ "flex flex-col w-full grow-1 shrink-1 border-strokeWeak relative"] do
      when hasChildren $ div_ [class_ "absolute top-1 left-2 border-l h-2 border-l-strokeWeak"] pass
      div_
        [ class_ "w-full cursor-pointer flex tree_opened justify-between max-w-full items-center h-5 hover:bg-fillWeaker"
        , [__| on click toggle .tree_opened on me|]
        ]
        do
          div_ [class_ "flex w-full justify-between items-center"] do
            div_ [class_ "flex items-center overflow-y-hidden", style_ $ "width: calc(40vw - " <> paddingLeft] do
              when hasChildren $ faSprite_ "chevron-up" "regular" "toggler rotate-0 w-4 border border-strokeWeak h-4 shadow-xs rounded-sm px-0.5 z-50 bg-fillWeaker mr-1 shrink-0 text-textStrong"
              unless (sp.spanRecord.parent == "___root___") $ span_ [class_ "text-textDisabled"] $ toHtml $ sp.spanRecord.parent <> "."
              span_ [class_ "text-textStrong "] $ toHtml sp.spanRecord.current
              when hasChildren $ span_ [class_ "badge badge-ghost text-xs"] $ toHtml $ show $ length sp.children
            unless hasChildren $ do
              let fullPath = (if sp.spanRecord.parent == "___root___" then "" else sp.spanRecord.parent <> ".") <> sp.spanRecord.current
              let target = Map.lookup fullPath dp
              whenJust target $ \t -> do
                span_ [class_ "w-[10vw] truncate"] $ toHtml $ T.intercalate ", " $ V.toList t.serviceNames
                div_ [class_ "w-[8vw]"] do
                  span_ [class_ "badge badge-ghost"] $ toHtml $ prettyPrintCount t.dataPointsCount
            div_ [class_ "flex w-[10vw] items-center text-xs"] do
              div_ [class_ "flex gap-1 items-center badge badge-ghost"] do
                faSprite_ "dashboard" "regular" "w-4 h-4"
                span_ "0"
              div_ [class_ "flex gap-1 items-center badge badge-ghost"] do
                faSprite_ "dashboard" "regular" "w-4 h-4"
                span_ "10"
              div_ [class_ "flex gap-1 items-center badge badge-ghost"] do
                faSprite_ "caution" "regular" "w-4 h-4"
                span_ "5"

      when hasChildren $ do
        div_ [class_ "flex-col hidden children_container gap-2 mt-2"] do
          forM_ (zip [0 ..] sp.children) \(i, c) -> do
            buildMetricTree_ pid c (level + 1) (i == length sp.children - 1) dp


-- Trace UI components
tracePage :: Projects.ProjectId -> Telemetry.Trace -> V.Vector Telemetry.SpanRecord -> Html ()
tracePage pid traceItem spanRecords = do
  let serviceData = V.toList $ getServiceData <$> spanRecords
      serviceNames = V.fromList $ ordNub $ (.name) <$> serviceData
      serviceColors = getServiceColors serviceNames
      rootSpans = buildSpanTree spanRecords
  div_ [class_ "w-full p-2", id_ "trace_span_container"] $ do
    div_ [class_ "flex flex-col w-full gap-4 pb-4"] $ do
      div_ [class_ "flex justify-between items-center"] do
        div_ [class_ "flex items-center gap-4"] $ do
          h3_ [class_ "whitespace-nowrap  font-semibold text-textStrong"] "Trace Breakdown"
        div_ [class_ "flex items-center gap-2"] $ do
          Components.dateTime traceItem.traceStartTime (Just traceItem.traceEndTime)
          button_ [class_ "p-0 m-0", [__| on click add .hidden to #trace_expanded_view then call updateUrlState('showTrace', '', 'delete')|]] do
            faSprite_ "side-chevron-left-in-box" "regular" "w-5 h-5 text-textBrand rotate-180"

      div_ [class_ "flex gap-1 w-full mt-5"] $ do
        div_ [role_ "tablist", class_ "w-full flex flex-col gap-2", id_ "trace-tabs"] $ do
          div_ [class_ "flex flex-col gap-2"] do
            div_ [class_ "flex justify-between mb-2"] do
              div_ [class_ "flex items-center gap-2 text-textWeak font-medium"] do
                button_ [class_ "a-tab text-sm px-3 py-1.5 border-b-2 border-b-transparent t-tab-active", onpointerdown_ "navigatable(this, '#flame_graph', '#trace-tabs', 't-tab-active')"] "Flame Graph"
                button_ [class_ "a-tab text-sm px-3 border-b-2 border-b-transparent py-1.5", onpointerdown_ "navigatable(this, '#water_fall', '#trace-tabs', 't-tab-active')"] "Waterfall"
                button_ [class_ "a-tab text-sm px-3 border-b-2 border-b-transparent py-1.5", onpointerdown_ "navigatable(this, '#span_list', '#trace-tabs', 't-tab-active')"] "Spans List"
              div_ [class_ "flex items-center gap-2"] do
                stBox (show $ length spanRecords) Nothing
                stBox (show $ length $ V.filter (\s -> s.status == Just SSError) spanRecords) $ Just (faSprite_ "alert-triangle" "regular" "w-3 h-3 text-textError")
                stBox (toText $ getDurationNSMS traceItem.traceDurationNs) $ Just (faSprite_ "clock" "regular" "w-3 h-3 text-textWeak")
            div_ [class_ "flex gap-2 w-full items-center"] do
              div_ [class_ "flex items-center gap-2 w-full rounded-lg px-3 grow-1 h-9 border border-strokeWeak bg-fillWeaker"] do
                faSprite_ "magnifying-glass" "regular" "w-3 h-3 text-textWeak"
                input_
                  [ class_ "w-full text-textStrong bg-transparent hover:outline-hidden focus:outline-hidden"
                  , type_ "text"
                  , placeholder_ "Search"
                  , id_ "search-input"
                  , [__| on input show .span-filterble in #trace_span_container when its textContent.toLowerCase() contains my value.toLowerCase() |]
                  ]
                let spanIds = decodeUtf8 $ AE.encode $ (.spanId) <$> spanRecords
                div_ [class_ "flex items-center gap-1", id_ "currentSpanIndex", term "data-span" "0"] do
                  button_
                    [ class_ "h-6 w-6 flex items-center justify-center bg-fillWeaker rounded-full font-bold border border-strokeWeak text-textStrong  cursor-pointer"
                    , onpointerdown_ [text|navigateSpans($spanIds, "prev")|]
                    ]
                    do
                      faSprite_ "chevron-up" "regular" "w-3 h-3"
                  button_
                    [ class_ "h-6 w-6 flex items-center justify-center rounded-full bg-fillWeaker font-bold border border-strokeWeak text-textStrong cursor-pointer"
                    , onpointerdown_ [text|navigateSpans($spanIds, "next")|]
                    ]
                    do
                      faSprite_ "chevron-down" "regular" "h-3 w-3"
              button_ [class_ "btn border border-strokeWeak bg-fillWeaker h-9"] "Reset Zoom"
          div_ [role_ "tabpanel", class_ "a-tab-content w-full", id_ "flame_graph"] do
            div_ [class_ "flex gap-2 w-full pt-2"] do
              div_
                [ class_ "w-[65%] group px-2 pt-4 border relative flex flex-col rounded-lg overflow-hidden"
                , id_ $ "flame-graph-container-" <> traceItem.traceId
                ]
                do
                  div_ [class_ "w-full sticky top-0 border-b border-b-strokeWeak h-6 text-xs relative", id_ $ "time-container-" <> traceItem.traceId] pass
                  div_ [class_ "w-full overflow-x-hidden min-h-56 h-full relative", id_ $ "a" <> traceItem.traceId] pass
                  div_ [class_ "h-full top-0  absolute z-50 hidden", id_ $ "time-bar-indicator-" <> traceItem.traceId] do
                    div_ [class_ "relative h-full"] do
                      div_ [class_ "text-xs top-[-18px] absolute -translate-x-1/2 whitespace-nowrap", id_ $ "line-time-" <> traceItem.traceId] "2 ms"
                      div_ [class_ "h-[calc(100%-24px)] mt-[24px] w-[1px] bg-strokeWeak"] pass

              div_ [class_ "border rounded-lg w-[35%] overflow-x-hidden"] do
                h3_ [class_ "w-full flex p-3 font-medium justify-between items-center text-sm border-b"] do
                  span_ [] "Services"
                  span_ [] "Exec Time %"
                div_ [class_ "w-full overflow-x-hidden  text-textWeak", id_ $ "services-" <> traceItem.traceId] do
                  forM_ serviceNames $ \s -> do
                    let spans = filter (\x -> x.name == s) serviceData
                        duration = sum $ (.duration) <$> spans
                        allDur = sum $ (.duration) <$> serviceData
                        percent = show $ (fromIntegral duration / fromIntegral allDur) * 100
                        color = getServiceColor s serviceColors
                    div_ [class_ "flex items-center justify-between px-2 py-1"] $ do
                      div_ [class_ "flex gap-1 items-center"] $ do
                        div_ [class_ $ "w-3 h-3 rounded-sm " <> color] pass
                        span_ [class_ ""] $ toHtml s
                      div_ [class_ "flex gap-1 items-center"] $ do
                        span_ [class_ "text-xs max-w-52 truncate"] $ toHtml $ T.take 4 percent <> "%"
                        div_ [class_ "w-[100px] h-3 bg-fillWeak rounded-sm overflow-hidden"]
                          $ div_ [class_ $ "h-full pl-2 text-xs font-medium " <> color, style_ $ "width:" <> percent <> "%"] pass

          div_ [role_ "tabpanel", class_ "a-tab-content pt-2 hidden", id_ "water_fall"] do
            div_ [class_ "border border-strokeWeak flex w-full rounded-2xl min-h-[230px]  overflow-y-auto overflow-x-hidden "] do
              div_ [class_ "w-full border-r overflow-x-hidden"] do
                div_ [class_ "border-b h-10 border-b-strokeWeak"] pass
                waterFallTree pid rootSpans traceItem.traceId serviceColors
              div_ [class_ "shrink-0 px-2"] do
                div_
                  [ class_ "w-xl sticky top-0 border-b border-b-strokeWeak h-10 text-xs relative"
                  , id_ "waterfall-time-container"
                  ]
                  pass
                div_ [class_ "w-xl overflow-x-hidden py-2 relative flex flex-col gap-2", id_ $ "waterfall-" <> traceItem.traceId] pass

          div_ [role_ "tabpanel", class_ "a-tab-content pt-2 hidden", id_ "span_list"] do
            div_ [class_ "border border-strokeWeak w-full rounded-2xl min-h-[230px] overflow-x-hidden "] do
              renderSpanListTable serviceNames serviceColors spanRecords

  let spanJson = decodeUtf8 $ AE.encode $ spanRecords <&> getSpanJson
  let waterFallJson = decodeUtf8 $ AE.encode rootSpans

  let colorsJson = decodeUtf8 $ AE.encode $ AE.object [AEKey.fromText k AE..= v | (k, v) <- HM.toList serviceColors]
  let trId = traceItem.traceId
  script_
    [text|
      function navigateSpans(spans, direction) {
         const container = document.querySelector('#currentSpanIndex')
         const currentSpan = Number(container.dataset.span)
         if (direction == 'next' && currentSpan >= spans.length) {
           return
         }
         if (direction == 'prev' && currentSpan <= 0) {
           return
         }
         const spandInd = direction == 'next' ? currentSpan + 1 : currentSpan - 1
         const span = spans[spandInd]
         container.dataset.span = spandInd
         htmx.trigger('#trigger-span-' + span, 'click')
      }
  |]
  script_
    [text|
   window.addEventListener('DOMContentLoaded', function() {
      flameGraphChart($spanJson, "$trId", $colorsJson);
      waterFallGraphChart($waterFallJson, "waterfall-$trId", $colorsJson);
   });
  |]


getSpanJson :: Telemetry.SpanRecord -> AE.Value
getSpanJson sp =
  AE.object
    [ "spanId" AE..= sp.spanId
    , "name" AE..= sp.spanName
    , "value" AE..= sp.spanDurationNs
    , "start" AE..= start
    , "parentId" AE..= sp.parentSpanId
    , "serviceName" AE..= getServiceName sp.resource
    , "hasErrors" AE..= spanHasErrors sp
    ]
  where
    start = utcTimeToNanoseconds sp.startTime


renderSpanRecordRow :: V.Vector Telemetry.SpanRecord -> HashMap Text Text -> Text -> Html ()
renderSpanRecordRow spanRecords colors service = do
  let totalDuration = sum $ (.spanDurationNs) <$> spanRecords
  let filterRecords = V.filter (\x -> getServiceName x.resource == service) spanRecords
  let listLen = V.length filterRecords
  let duration = sum $ (.spanDurationNs) <$> filterRecords
  tr_
    [ class_ "w-full overflow-x-hidden p-2 cursor-pointer font-medium hover:bg-fillWeaker border-b-2 last:border-b-0"
    , [__|on click toggle .hidden on next <tr/> then toggle .rotate-90 on the first <svg/> in the first <td/> in me|]
    ]
    do
      td_ [class_ "ml-1 px-2 py-1 w-[600px] text-textStrong truncate flex items-center gap-1 font-medium"] do
        div_ [class_ "w-1 bg-fillBrand-weak h-4"] pass
        faSprite_ "chevron-right" "regular" "h-3 w-3 mr-2 text-textWeak"
        div_ [class_ $ "w-3 h-3 rounded-sm " <> getServiceColor service colors] pass
        span_ [] $ toHtml service
      td_ [class_ "px-2 py-1 max-w-48 text-textWeak truncate pl-4"] $ toHtml $ show listLen
      td_ [class_ "px-2 py-1 max-w-48 text-textWeak truncate pl-4"] $ toHtml $ getDurationNSMS $ duration `div` toInteger listLen
      td_ [class_ "px-2 py-1 max-w-48 text-textWeak truncate pl-4"] $ toHtml $ getDurationNSMS duration
      td_ [class_ "px-2 py-1 max-w-48 text-textWeak truncate pl-4"] $ toHtml $ show (duration * 100 `div` totalDuration) <> "%"
  tr_ [class_ "hidden p-0 m-0", [__|on click halt|]] do
    td_ [colspan_ "5", class_ "pl-[13px] overflow-x-hidden"] do
      spanTable filterRecords


renderSpanListTable :: V.Vector Text -> HashMap Text Text -> V.Vector Telemetry.SpanRecord -> Html ()
renderSpanListTable services colors records =
  table_ [class_ "w-full table table-sm overflow-x-hidden"] $ do
    thead_ [class_ "border-b"] $ do
      tr_ [class_ "p-2 border-b font-normal"] $ do
        th_ "Resource"
        th_ "Spans"
        th_ "Avg. Duration"
        th_ "Exec. Time"
        th_ "%Exec. Time"
    tbody_ [class_ "space-y-0"]
      $ mapM_ (renderSpanRecordRow records colors) services


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
          let pidText = UUID.toText spanRecord.projectId
              spanid = spanRecord.uSpanId
              tme = fromString (formatShow iso8601Format spanRecord.timestamp)
              (reqType, _, _, _) = fromMaybe ("", "", "", 0) $ getRequestDetails spanRecord.attributes
          tr_
            [ hxGet_ $ "/p/" <> pidText <> "/log_explorer/" <> spanid <> "/" <> tme <> "/detailed?source=spans"
            , hxTarget_ "#log_details_container"
            , hxSwap_ "innerHTML"
            , id_ $ "sp-list-" <> spanRecord.spanId
            , class_ "span-filterble font-medium"
            , hxIndicator_ "#loading-span-list"
            ]
            $ do
              td_ $ toHtml $ formatTime defaultTimeLocale "%b %d %Y %H:%M:%S%Q" spanRecord.timestamp
              td_ $ toHtml spanRecord.spanName
              td_ do
                span_ [class_ "cbadge-sm badge-neutral"] $ toHtml reqType
              td_ do
                span_ [class_ "cbadge-sm badge-neutral"] $ toHtml $ T.drop 2 $ maybe "" show spanRecord.kind
              td_ do
                span_ [class_ "cbadge-sm badge-neutral"] $ toHtml $ getDurationNSMS spanRecord.spanDurationNs


getServiceData :: Telemetry.SpanRecord -> ServiceData
getServiceData sp = ServiceData{name = getServiceName sp.resource, duration = sp.spanDurationNs}


stBox :: Text -> Maybe (Html ()) -> Html ()
stBox value iconM =
  div_ [class_ "flex items-center px-2 gap-2 border-r last:border-r-0"] do
    whenJust iconM id
    span_ [class_ "text-textStrong text-sm"] $ toHtml value
    if isNothing iconM then span_ [class_ "font-medium text-textWeak text-xs"] "Spans" else pass


buildSpanMap :: V.Vector Telemetry.SpanRecord -> MapS.Map (Maybe Text) [Telemetry.SpanRecord]
buildSpanMap = V.foldr (\sp m -> MapS.insertWith (++) sp.parentSpanId [sp] m) MapS.empty


buildSpanTree :: V.Vector Telemetry.SpanRecord -> [SpanTree]
buildSpanTree spans =
  let spanMap = buildSpanMap spans
   in buildTree spanMap Nothing
  where
    buildTree spanMap parentId =
      case MapS.lookup parentId spanMap of
        Nothing -> []
        Just spans' ->
          [ SpanTree
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
                }
              (buildTree spanMap (Just sp.spanId))
          | sp <- spans'
          ]


waterFallTree :: Projects.ProjectId -> [SpanTree] -> Text -> HashMap Text Text -> Html ()
waterFallTree pid records trId scols = do
  div_ [class_ "pl-2 py-2 flex flex-col gap-2"] do
    forM_ (zip [0 ..] records) \(i, c) -> do
      buildSpanTree_ pid c trId 0 scols True


buildSpanTree_ :: Projects.ProjectId -> SpanTree -> Text -> Int -> HashMap Text Text -> Bool -> Html ()
buildSpanTree_ pid sp trId level scol isLasChild = do
  let hasChildren = not $ null sp.children
      serviceCol = getServiceColor sp.spanRecord.serviceName scol
  div_ [class_ "flex items-start w-full relative span-filterble"] do
    when (level /= 0) $ div_ [class_ "w-4 shrink-0 ml-2 h-[1px] mt-2 bg-strokeWeak"] pass
    unless (level == 0) $ div_ [class_ "absolute -top-3 left-2 border-l h-5 border-l-strokeWeak"] pass
    unless isLasChild $ div_ [class_ "absolute top-1 left-2 border-l h-full border-l-strokeWeak"] pass
    div_ [class_ "flex flex-col w-full grow-1 shrink-1 border-strokeWeak relative"] do
      when hasChildren $ div_ [class_ "absolute top-1 left-2 border-l h-2 border-l-strokeWeak"] pass
      let tme = fromString (formatShow iso8601Format sp.spanRecord.timestamp)
          spanId = UUID.toText sp.spanRecord.uSpanId
      div_
        [ class_ "w-full cursor-pointer flex justify-between max-w-full items-center h-5 collapsed"
        , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer/" <> spanId <> "/" <> tme <> "/detailed?source=spans"
        , hxTarget_ "#log_details_container"
        , hxSwap_ "innerHTML"
        , hxIndicator_ "#loading-span-list"
        , id_ $ "trigger-span-" <> sp.spanRecord.spanId
        ]
        do
          div_ [class_ "flex items-center w-[95%] gap-1 border-strokeBrand-strong rounded-lg overflow-x-hidden waterfall-item", [__|on click remove .border from .waterfall-item then add .border to me|]] do
            when hasChildren $ do
              div_ [class_ "border border-strokeWeak w-7 flex justify-between gap-1 items-center rounded-sm p-0.5"] do
                faSprite_ "chevron-right" "regular" "h-3 w-3 shrink-0 font-bold text-textStrong waterfall-item-tree-chevron"
                span_ [class_ "text-xs"] $ toHtml $ show (length sp.children)
            span_ [class_ "font-medium text-textStrong "] $ toHtml sp.spanRecord.serviceName
            faSprite_ "chevron-right" "regular" "h-3 w-3 shrink-0 text-textStrong"
            span_ [class_ "text-textWeak text-sm whitespace-nowrap"] $ toHtml sp.spanRecord.spanName
          span_ [class_ $ "w-1 rounded-sm h-5 shrink-0 " <> serviceCol] ""
      when hasChildren $ do
        div_ [class_ "flex flex-col children_container gap-2 mt-2", id_ $ "waterfall-tree-" <> sp.spanRecord.spanId] do
          forM_ (zip [0 ..] sp.children) \(i, c) -> do
            buildSpanTree_ pid c trId (level + 1) scol (i == length sp.children - 1)
