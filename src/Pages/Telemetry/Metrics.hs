module Pages.Telemetry.Metrics (metricsOverViewGetH, metricDetailsGetH, MetricsOverViewGet (..), metricBreakdownGetH) where

import Data.Default
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components qualified as Components
import Pages.Telemetry.Utils (metricsTree)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (faSprite_, parseTime)


metricsOverViewGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> ATAuthCtx (RespHeaders MetricsOverViewGet)
metricsOverViewGetH pid tabM fromM toM sinceM sourceM prefixM cursorM = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  let tab = maybe "datapoints" (\t -> if t == "datapoints" then t else "charts") tabM
  let (from, to, currentRange) = parseTime fromM toM sinceM now
      bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , prePageTitle = Just "Explorer"
          , pageTitle = "Metrics"
          , navTabs = Just $ div_ [class_ "tabs tabs-box tabs-md p-0 tabs-outline items-center border"] do
              a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer", role_ "tab", class_ "tab h-auto! "] "Events"
              a_ [href_ $ "/p/" <> pid.toText <> "/metrics", role_ "tab", class_ "tab h-auto! tab-active text-textStrong"] "Metrics"
          , docsLink = Just "https://apitoolkit.io/docs/dashboard/dashboard-pages/metrics/"
          , pageActions = Just $ div_ [class_ "inline-flex gap-2"] do
              TimePicker.timepicker_ Nothing currentRange
              TimePicker.refreshButton_
          }
  if tab == "datapoints"
    then do
      dataPoints <- Telemetry.getDataPointsData pid (from, to)
      addRespHeaders $ MetricsOVDataPointMain $ PageCtx bwconf (pid, dataPoints)
    else do
      let cursor = fromMaybe 0 cursorM
      metricList <- Telemetry.getMetricChartListData pid sourceM prefixM (from, to) cursor
      let sourceQ = maybe "" ("&source=" <>) sourceM
          fromQ = maybe "" ("&from=" <>) fromM
          toQ = maybe "" ("&from=" <>) toM
          sinceQ = maybe "" ("&since=" <>) sinceM
          prfixQ = maybe "" ("&prefix=" <>) prefixM
          cursorQ = "&cursor=" <> show (cursor + 20)
      let nextFetchUrl = "/p/" <> pid.toText <> "/metrics?tab=charts" <> sourceQ <> fromQ <> toQ <> sinceQ <> prfixQ <> cursorQ
      serviceNames <- Telemetry.getMetricServiceNames pid
      if cursor == 0
        then do
          addRespHeaders $ MetricsOVChartsMain $ PageCtx bwconf (pid, metricList, serviceNames, fromMaybe "all" sourceM, fromMaybe "all" prefixM, nextFetchUrl)
        else do
          addRespHeaders $ MetricsOVChartsPaginated (pid, metricList, fromMaybe "all" sourceM, nextFetchUrl)


data MetricsOverViewGet
  = MetricsOVDataPointMain (PageCtx (Projects.ProjectId, V.Vector Telemetry.MetricDataPoint))
  | MetricsOVChartsMain (PageCtx (Projects.ProjectId, V.Vector Telemetry.MetricChartListData, V.Vector Text, Text, Text, Text))
  | MetricsOVChartsPaginated (Projects.ProjectId, V.Vector Telemetry.MetricChartListData, Text, Text)


instance ToHtml MetricsOverViewGet where
  toHtml (MetricsOVDataPointMain (PageCtx bwconf (pid, datapoints))) = toHtml $ PageCtx bwconf $ dataPointsPage pid datapoints
  toHtml (MetricsOVChartsMain (PageCtx bwconf (pid, mList, serviceNames, source, prefix, nextUrl))) = toHtml $ PageCtx bwconf $ chartsPage pid mList serviceNames source prefix nextUrl
  toHtml (MetricsOVChartsPaginated (pid, mList, source, nextUrl)) = toHtml $ chartList pid source mList nextUrl
  toHtmlRaw = toHtml


overViewTabs :: Projects.ProjectId -> Text -> Html ()
overViewTabs pid tab = do
  div_ [class_ "w-max mt-5"] do
    div_ [class_ "tabs tabs-box tabs-md tabs-outline items-center border"] do
      a_ [onclick_ "window.setQueryParamAndReload('tab', 'datapoints')", role_ "tab", class_ $ "tab py-1.5 h-auto!  " <> if tab == "datapoints" then "tab-active" else ""] "Datapoints"
      a_ [onclick_ "window.setQueryParamAndReload('tab', 'charts')", role_ "tab", class_ $ "tab py-1.5 h-auto! " <> if tab == "charts" then "tab-active" else ""] "Charts List"


chartsPage :: Projects.ProjectId -> V.Vector Telemetry.MetricChartListData -> V.Vector Text -> Text -> Text -> Text -> Html ()
chartsPage pid metricList sources source mFilter nextUrl = do
  div_ [class_ "flex flex-col gap-6 px-6 py-4 h-[calc(100%-60px)] overflow-y-scroll"] $ do
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
    div_ [class_ "w-full grid grid-cols-3 gap-4", id_ "metric_list_container"] $ do
      chartList pid source metricList nextUrl


chartList :: Projects.ProjectId -> Text -> V.Vector Telemetry.MetricChartListData -> Text -> Html ()
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
  when (length metricList > 19)
    $ a_ [hxTrigger_ "intersect once", hxSwap_ "outerHTML", hxGet_ nextUrl] pass


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
        div_ [class_ "w-full"] $ do
          let metrMap = Map.fromList $ V.toList $ V.map (\mdp -> (mdp.metricName, mdp)) metrics
          metricsTree pid metrics metrMap


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
      lableValues <- Telemetry.getMetricLabelValues pid metricName label
      addRespHeaders $ metricBreakdown pid labelM lableValues


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
          , Widget.title = Nothing -- Title already shown above
          , Widget.query = Just $ "metrics | where metric_name == \"" <> metric.metricName <> "\" | summarize avg(metric_value.contents.value) by bin_auto(timestamp),attributes"
          , Widget.layout = Just $ Widget.Layout{x = Just 0, y = Just 0, w = Just 2, h = Just 1}
          , Widget.unit = Just metric.metricUnit
          , Widget.hideLegend = Nothing
          , Widget.eager = Just True
          , Widget._projectId = Just pid
          , Widget.id = Just $ "details_" <> T.replace "." "_" metric.metricName -- Replace dots with underscores for valid HTML ID
          , Widget.expandBtnFn = Nothing -- No expand button needed in detail view
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
